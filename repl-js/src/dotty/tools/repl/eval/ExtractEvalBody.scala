package dotty.tools
package repl
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.MacroTransform
import dotty.tools.dotc.util.SrcPos

import scala.collection.mutable

/** Post-typer phase that pulls the typed eval body out of its splice point and
 *  into the synthesised `__Expression.evaluate` method.
 *
 *  1. At the spliced `val __evalResult`, capture its rhs into [[EvalStore]] and
 *     replace it with `null.asInstanceOf[T]`.
 *  2. At `__Expression.evaluate`, run the captured body through
 *     [[ExtractTransformer]] to rewrite outer-scope references into `reflectEval`
 *     placeholders, and install the result as the rhs.
 *  3. The [[DenotTransformer]] hook re-parents body-local symbols from the val
 *     to `evaluate` so they survive the move.
 *
 *  Foundation port: the wrapper refl-helper retargeting (for private members) is
 *  omitted — those strategies route to runtime helpers that are stubbed pending
 *  task #10. Binding-based strategies (LocalValue/Assign, MethodCapture, This)
 *  are fully supported. */
private[eval] class ExtractEvalBody(config: EvalCompilerConfig, store: EvalStore)
  extends MacroTransform with DenotTransformer:

  override def phaseName: String = ExtractEvalBody.name

  override def transformPhase(using Context): Phase = this.next

  /** Re-own to `evaluate` exactly the symbols whose owner is the spliced
   *  `__evalResult` val — i.e. the eval body's *own* top-level locals. Symbols
   *  nested inside a local def within the body (a lambda's params, a local
   *  class's members) are owned by that inner def, not by `__evalResult`, so they
   *  stay put and ride along when their owner moves — closures (incl. a nested
   *  `embedRepl`'s `s => …`) stay intact instead of having their parameters
   *  detached. */
  override def transform(ref: SingleDenotation)(using Context): SingleDenotation =
    ref match
      case ref: SymDenotation if isExpressionVal(ref.symbol.maybeOwner) =>
        ref.copySymDenotation(owner = config.evaluateMethod)
      case _ => ref

  override protected def newTransformer(using Context): Transformer =
    new ExtractEvalBodyTransformer

  private def isExpressionVal(sym: Symbol)(using Context): Boolean =
    sym.exists && sym.name == SpliceEvalBody.EvalResultName

  private def isWrapperSymbol(sym: Symbol)(using Context): Boolean =
    sym.exists && sym.name.toString.contains(ExtractEvalBody.WrapperMarker)

  private def holdsNoNestedClasses(wrapperClass: TypeDef)(using Context): Boolean =
    wrapperClass.rhs match
      case impl: Template =>
        !impl.body.exists {
          case td: TypeDef => td.isClassDef
          case _ => false
        }
      case _ => true

  private class ExtractEvalBodyTransformer extends Transformer:
    private var bodyTree: Tree | Null = null

    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case PackageDef(pid, stats) =>
          val (exprClassDef, others) = stats.partition { stat =>
            stat.symbol.exists && stat.symbol == config.expressionClass
          }
          val transformedStats = (others ++ exprClassDef).map(transform)
          val wrapperClass = transformedStats.collectFirst {
            case stat: TypeDef if stat.isClassDef && isWrapperSymbol(stat.symbol) => stat
          }
          val canDropWrapper = wrapperClass.exists(holdsNoNestedClasses)
          val pruned =
            if !canDropWrapper then transformedStats
            else transformedStats.filter {
              case stat: TypeDef if isWrapperSymbol(stat.symbol) => false
              case stat: ValDef if isWrapperSymbol(stat.symbol) => false
              case _ => true
            }
          cpy.PackageDef(tree)(pid, pruned)

        case tree: ValDef if isExpressionVal(tree.symbol) =>
          bodyTree = tree.rhs
          store.store(tree.symbol)
          val defaultRhs = Literal(Constant(null)).cast(tree.tpt.tpe).withSpan(tree.rhs.span)
          cpy.ValDef(tree)(rhs = defaultRhs)

        case tree: DefDef if tree.symbol == config.evaluateMethod =>
          val captured = bodyTree
          if captured == null then tree
          else
            val rhs = ExtractTransformer.transform(captured)
            cpy.DefDef(tree)(rhs = rhs)

        case _ => super.transform(tree)
  end ExtractEvalBodyTransformer

  private object ExtractTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree = tree match
      case _: ImportOrExport => tree

      case tree: This =>
        val cls = tree.symbol
        if cls == config.expressionClass then super.transform(tree)
        // A class defined inside the eval body moves into `evaluate` with it,
        // so its own `this` (e.g. in its accessors) needs no outer walk.
        else if isLocalToBody(cls) then super.transform(tree)
        else if cls.is(ModuleClass) && isGloballyAccessible(cls) then
          super.transform(tree)
        else if cls.isClass && store.classOwners.contains(cls) then
          thisOrOuterValue(tree, cls.asClass)
        else
          report.error(
            s"eval: cannot reach outer `this` of class `${cls.name}` " +
              "from the eval call site (not in the captured owner chain).",
            tree.srcPos
          )
          super.transform(tree)

      case Assign(lhs: Ident, rhs)
          if isLocalVariable(lhs.symbol) && !isLocalToBody(lhs.symbol) =>
        setLocalValue(tree, lhs.symbol.asTerm, transform(rhs))

      case tree @ Assign(lhs, rhs) if isInaccessibleField(lhs) =>
        setField(tree, transformedQualifier(lhs), lhs.symbol.asTerm, transform(rhs))

      case tree: Ident =>
        val sym = tree.symbol
        if !sym.exists || isLocalToBody(sym) || isGloballyAccessible(sym) then
          super.transform(tree)
        else if isAccessibleViaStaticPrefix(tree) then
          super.transform(tree)
        else if isTermOwnedModule(sym) then
          super.transform(tree)
        else if isLocalVariable(sym) then
          getLocalValue(tree, sym.asTerm)
        else if isInaccessibleField(tree) then
          getField(tree, transformedQualifier(tree), sym.asTerm)
        else if isInaccessibleMethod(tree) then
          callMethod(tree, transformedQualifier(tree), sym.asTerm, Nil)
        else if isOuterMethodLocalDef(sym) then
          captureLocalMethod(tree, sym.asTerm, Nil)
        else
          report.error(
            s"eval: cannot reference outer symbol `${sym.name}` (owner: ${sym.owner}).",
            tree.srcPos
          )
          super.transform(tree)

      case tree: Select if isInaccessibleField(tree) =>
        getField(tree, transform(tree.qualifier), tree.symbol.asTerm)

      case tree: Select if isTermOwnedClassFieldAccess(tree) =>
        getField(tree, transform(tree.qualifier), tree.symbol.asTerm)

      case tree: Apply if isInaccessibleMethod(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: Apply if isTermOwnedClassMethodCall(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: TypeApply if isInaccessibleMethod(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: TypeApply if isTermOwnedClassMethodCall(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: Select if isInaccessibleMethod(tree) =>
        callMethod(tree, transform(tree.qualifier), tree.symbol.asTerm, Nil)

      case tree: Select if isTermOwnedClassMethodCall(tree) =>
        callMethod(tree, transform(tree.qualifier), tree.symbol.asTerm, Nil)

      case tree: Apply if isOuterMethodLocalDef(tree.fun.symbol) =>
        captureLocalMethod(tree, tree.fun.symbol.asTerm, transformedMethodArgs(tree))

      case tree: TypeApply if isOuterMethodLocalDef(tree.symbol) =>
        captureLocalMethod(tree, tree.symbol.asTerm, Nil)

      case _ => super.transform(tree)
    end transform

    private def getLocalValue(tree: Tree, sym: TermSymbol)(using Context): Tree =
      val castTo =
        if isTermOwnedClass(sym.info) then defn.ObjectType
        else tree.tpe.widen
      buildReflectEvalCast(tree, nullLiteral,
        ReflectEvalStrategy.LocalValue(sym, isByName(sym.info)), Nil, castTo)

    private def setLocalValue(tree: Tree, sym: TermSymbol, rhs: Tree)(using Context): Tree =
      reflectEvalPlaceholder(tree, nullLiteral,
        ReflectEvalStrategy.LocalValueAssign(sym), rhs :: Nil)

    private def getThisObject(tree: Tree, cls: ClassSymbol)(using Context): Tree =
      val castTo = if isTermOwnedSymbol(cls) then defn.ObjectType else cls.typeRef
      buildReflectEvalCast(tree, nullLiteral, ReflectEvalStrategy.This(cls), Nil, castTo)

    private def getOuter(tree: Tree, qualifier: Tree, outerCls: ClassSymbol)(using Context): Tree =
      buildReflectEvalCast(tree, qualifier, ReflectEvalStrategy.Outer(outerCls), Nil, outerCls.typeRef)

    private def buildReflectEvalCast(
        tree: Tree,
        qualifier: Tree,
        strategy: ReflectEvalStrategy,
        args: List[Tree],
        castTo: Type
    )(using Context): Tree =
      val evalArgs = List(
        qualifier,
        Literal(Constant(strategy.toString)),
        JavaSeqLiteral(args, TypeTree(defn.ObjectType))
      )
      cpy.Apply(tree)(
        Select(This(config.expressionClass), termName("reflectEval")),
        evalArgs
      ).withAttachment(ReflectEvalStrategy, strategy).cast(castTo)

    private def isAccessibleViaStaticPrefix(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      if sym.exists && (sym.isPrivate || sym.is(Protected)) then false
      else tree.tpe match
        case tref: TermRef => isStaticPrefix(tref.prefix)
        case _ => false

    private def isStaticPrefix(prefix: Type)(using Context): Boolean = prefix match
      case tt: ThisType =>
        val cls = tt.cls
        cls.exists && cls.is(ModuleClass) && isGloballyAccessible(cls)
      case tref: TermRef =>
        val sym = tref.symbol
        sym.exists && sym.is(Module) && isStaticPrefix(tref.prefix)
      case _ => false

    private def isInaccessibleField(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isField && sym.enclosingClass.isClass &&
        (sym.isPrivate || sym.is(Protected)) && !isLocalToBody(sym)

    private def isInaccessibleMethod(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isRealMethod && sym.enclosingClass.isClass &&
        (sym.isPrivate || sym.is(Protected)) && !isLocalToBody(sym)

    private def isTermOwnedClassFieldAccess(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isField && sym.enclosingClass.isClass &&
        isTermOwnedSymbol(sym.enclosingClass) &&
        !isLocalToBody(sym) &&
        !sym.isPrivate && !sym.is(Protected)

    private def isTermOwnedClassMethodCall(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isRealMethod && !sym.isClassConstructor &&
        sym.enclosingClass.isClass &&
        isTermOwnedSymbol(sym.enclosingClass) &&
        !isLocalToBody(sym) &&
        !sym.isPrivate && !sym.is(Protected)

    private def isTermOwnedSymbol(cls: Symbol)(using Context): Boolean =
      cls.exists && cls.maybeOwner.exists && cls.maybeOwner.isTerm

    private def isTermOwnedClass(tpe: Type)(using Context): Boolean =
      val sym = tpe.widen.typeSymbol
      sym.exists && sym.isClass && isTermOwnedSymbol(sym)

    private def isTermOwnedModule(sym: Symbol)(using Context): Boolean =
      sym.exists && sym.is(Module) && isTermOwnedSymbol(sym)

    private def isOuterMethodLocalDef(sym: Symbol)(using Context): Boolean =
      sym.exists && sym.is(Method) && !sym.isClassConstructor &&
        // Owner is any term scope (method, or a `val`/block whose RHS is a
        // block) — the rewriter captured such defs as eta-expanded bindings
        // regardless. (The JVM port restricts to method owners; we broaden so a
        // `def` declared inside a `val r = { def h … }` block is reachable too.)
        sym.owner.exists && sym.owner.isTerm && !isLocalToBody(sym)

    private def getField(tree: Tree, qual: Tree, field: TermSymbol)(using Context): Tree =
      val resultTpe =
        if isTermOwnedClass(field.info) then defn.ObjectType else field.info.widen
      buildReflectEvalCast(tree, qual,
        ReflectEvalStrategy.Field(field, isByName = false, useReceiverClass(field)),
        Nil, resultTpe)

    private def setField(tree: Tree, qual: Tree, field: TermSymbol, rhs: Tree)(using Context): Tree =
      buildReflectEvalCast(tree, qual,
        ReflectEvalStrategy.FieldAssign(field, useReceiverClass(field)),
        rhs :: Nil, defn.UnitType)

    private def useReceiverClass(member: Symbol)(using Context): Boolean =
      val cls = member.enclosingClass
      cls.exists && cls.isClass && isTermOwnedSymbol(cls)

    private def callMethod(tree: Tree, qual: Tree, method: TermSymbol, args: List[Tree])(using Context): Tree =
      val rawResult = method.info.finalResultType.widen
      val resultTpe =
        if isTermOwnedClass(rawResult) then defn.ObjectType else rawResult
      buildReflectEvalCast(tree, qual,
        ReflectEvalStrategy.MethodCall(method, useReceiverClass(method)),
        args, resultTpe)

    private def captureLocalMethod(tree: Tree, method: TermSymbol, args: List[Tree])(using Context): Tree =
      buildReflectEvalCast(tree, nullLiteral,
        ReflectEvalStrategy.MethodCapture(method, method, isByName = false),
        args, tree.tpe.widen)

    private def transformedMethodArgs(tree: Tree)(using Context): List[Tree] = tree match
      case _: (Ident | Select) => Nil
      case Apply(fun, args) => transformedMethodArgs(fun) ++ args.map(transform)
      case TypeApply(fun, _) => transformedMethodArgs(fun)
      case _ => Nil

    private def transformedQualifier(tree: Tree)(using Context): Tree = tree match
      case Select(qual, _) => transform(qual)
      case Apply(fun, _) => transformedQualifier(fun)
      case TypeApply(fun, _) => transformedQualifier(fun)
      case Assign(lhs, _) => transformedQualifier(lhs)
      case Ident(_) =>
        val cls = tree.symbol.enclosingClass
        if cls.isClass && store.classOwners.contains(cls) then
          thisOrOuterValue(tree, cls.asClass)
        else
          report.error(
            s"eval: cannot synthesise qualifier for `${tree.symbol.name}` — " +
              s"its enclosing class `${cls.name}` is not in the captured owner chain.",
            tree.srcPos
          )
          nullLiteral
      case _ => nullLiteral

    private def thisOrOuterValue(tree: Tree, cls: ClassSymbol)(using Context): Tree =
      if cls.is(ModuleClass) && isGloballyAccessible(cls) then
        return ref(cls.sourceModule).withSpan(tree.span)
      val owners = store.classOwners
      val target = owners.indexOf(cls)
      if target < 0 then
        report.error(s"internal error: class `${cls.name}` not in classOwners", tree.srcPos)
        return getThisObject(tree, cls)
      val ths = getThisObject(tree, owners.head.asClass)
      owners.iterator.drop(1).take(target).foldLeft(ths) { (inner, outerSym) =>
        getOuter(tree, inner, outerSym)
      }

    private def reflectEvalPlaceholder(
        tree: Tree,
        qualifier: Tree,
        strategy: ReflectEvalStrategy,
        args: List[Tree]
    )(using Context): Tree =
      buildReflectEvalCast(tree, qualifier, strategy, args, tree.tpe.widen)

    private def isLocalVariable(sym: Symbol)(using Context): Boolean =
      sym.exists && !sym.is(Method) && sym.isLocalToBlock

    private def isByName(tpe: Type)(using Context): Boolean = tpe match
      case _: ExprType => true
      case ref: TermRef => isByName(ref.symbol.info)
      case _ => false

    private def isLocalToBody(sym: Symbol)(using Context): Boolean =
      val valSym = store.symbol
      if !sym.exists || valSym == null then false
      else sym.ownersIterator.exists(o =>
        o == valSym || o == config.evaluateMethod || o == config.expressionClass
      )

    private def isGloballyAccessible(sym: Symbol)(using Context): Boolean =
      if !sym.exists then false
      else if sym.isPrivate || sym.is(Protected) then false
      else if sym.is(Package) || sym.is(PackageClass) then true
      else
        val owner = sym.owner
        if !owner.exists then false
        else if owner.is(PackageClass) then true
        else if owner.is(ModuleClass) then isGloballyAccessible(owner)
        else false

  end ExtractTransformer

private[eval] object ExtractEvalBody:
  val name: String = "extractEvalBody"
  /** Substring shared by every wrapper module synthesised by the eval driver. */
  val WrapperMarker: String = "__EvalWrapper"
