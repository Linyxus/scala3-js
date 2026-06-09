package dotty.tools
package repl

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.UndefOr

private[repl] trait ConsoleLineReader:
  def prompt(): Unit
  def close(): Unit

private[repl] object ConsoleLineReader:
  def create(
      process: js.Dynamic,
      promptText: String,
      onLine: String => Unit,
      onClose: () => Unit,
  ): ConsoleLineReader =
    val input = process.stdin
    val output = process.stdout
    if isTTY(input) && isTTY(output) then
      new TerminalConsoleLineReader(process, promptText, onLine, onClose)
    else
      new PlainConsoleLineReader(process, promptText, onLine, onClose)

  private def isTTY(stream: js.Dynamic): Boolean =
    val value = stream.selectDynamic("isTTY")
    !js.isUndefined(value) && value != null && value.asInstanceOf[Boolean]

private[repl] final class ReplLineEditor(historyLimit: Int = 500):
  private val history = mutable.ArrayBuffer.empty[String]
  private var historyIndex = 0
  private var draft = ""

  private[repl] var buffer: String = ""
  private var cursor = 0

  def cursorIndex: Int = cursor

  def reset(): Unit =
    buffer = ""
    cursor = 0
    historyIndex = history.length
    draft = ""

  def accept(): String =
    val accepted = buffer
    if accepted.trim.nonEmpty then
      val duplicate = history.indexOf(accepted)
      if duplicate >= 0 then history.remove(duplicate)
      history += accepted
      val overflow = history.length - historyLimit
      if overflow > 0 then history.remove(0, overflow)
    reset()
    accepted

  def insert(text: String): Unit =
    if text.nonEmpty then
      buffer = buffer.substring(0, cursor) + text + buffer.substring(cursor)
      cursor += text.length

  def insertNewline(): Unit = insert("\n")

  def moveLeft(): Unit =
    if cursor > 0 then cursor -= 1

  def moveRight(): Unit =
    if cursor < buffer.length then cursor += 1

  def moveToLineStart(): Unit =
    cursor = lineStart(cursor)

  def moveToLineEnd(): Unit =
    cursor = lineEnd(cursor)

  def moveUpOrHistory(): Unit =
    val start = lineStart(cursor)
    if start == 0 then previousHistory()
    else
      val col = cursor - start
      val prevEnd = start - 1
      val prevStart = lineStart(prevEnd)
      cursor = math.min(prevStart + col, prevEnd)

  def moveDownOrHistory(): Unit =
    val end = lineEnd(cursor)
    if end == buffer.length then nextHistory()
    else
      val col = cursor - lineStart(cursor)
      val nextStart = end + 1
      val nextEnd = lineEnd(nextStart)
      cursor = math.min(nextStart + col, nextEnd)

  def deleteBeforeCursor(): Unit =
    if cursor > 0 then
      deleteRange(cursor - 1, cursor)
      cursor -= 1

  def deleteAtCursor(): Unit =
    if cursor < buffer.length then deleteRange(cursor, cursor + 1)

  def killToLineStart(): Unit =
    val start = lineStart(cursor)
    if start < cursor then
      deleteRange(start, cursor)
      cursor = start

  def killToLineEnd(): Unit =
    val end = lineEnd(cursor)
    if cursor < end then deleteRange(cursor, end)
    else if cursor < buffer.length then deleteRange(cursor, cursor + 1)

  def deletePreviousWord(): Unit =
    if cursor > 0 then
      var start = cursor
      while start > 0 && buffer.charAt(start - 1).isWhitespace do start -= 1
      while start > 0 && !buffer.charAt(start - 1).isWhitespace do start -= 1
      deleteRange(start, cursor)
      cursor = start

  def moveWordLeft(): Unit =
    if cursor > 0 then
      while cursor > 0 && buffer.charAt(cursor - 1).isWhitespace do cursor -= 1
      while cursor > 0 && !buffer.charAt(cursor - 1).isWhitespace do cursor -= 1

  def moveWordRight(): Unit =
    if cursor < buffer.length then
      while cursor < buffer.length && !buffer.charAt(cursor).isWhitespace do cursor += 1
      while cursor < buffer.length && buffer.charAt(cursor).isWhitespace do cursor += 1

  def previousHistory(): Unit =
    if history.nonEmpty && historyIndex > 0 then
      if historyIndex == history.length then draft = buffer
      historyIndex -= 1
      setBuffer(history(historyIndex))

  def nextHistory(): Unit =
    if historyIndex < history.length then
      historyIndex += 1
      if historyIndex == history.length then setBuffer(draft)
      else setBuffer(history(historyIndex))

  private def setBuffer(text: String): Unit =
    buffer = text
    cursor = text.length

  private def deleteRange(from: Int, until: Int): Unit =
    buffer = buffer.substring(0, from) + buffer.substring(until)

  private def lineStart(pos: Int): Int =
    val i = buffer.lastIndexOf('\n', math.max(0, pos - 1))
    if i < 0 then 0 else i + 1

  private def lineEnd(pos: Int): Int =
    val i = buffer.indexOf('\n', math.min(pos, buffer.length))
    if i < 0 then buffer.length else i

private final class PlainConsoleLineReader(
    process: js.Dynamic,
    promptText: String,
    onLine: String => Unit,
    onClose: () => Unit,
) extends ConsoleLineReader:
  private val readline = js.Dynamic.global.require("readline")
  private val rl = readline.createInterface(js.Dynamic.literal(
    input = process.stdin,
    output = process.stdout,
    terminal = false,
  ))
  private var closed = false

  rl.on("line", ((line: String) => onLine(line)): js.Function1[String, Unit])
  rl.on("close", (() => closeFromInput()): js.Function0[Unit])

  def prompt(): Unit =
    process.stdout.write(promptText)
    ()

  def close(): Unit =
    if !closed then
      closed = true
      try rl.close() catch case _: Throwable => ()

  private def closeFromInput(): Unit =
    if !closed then
      closed = true
      onClose()

private final class TerminalConsoleLineReader(
    process: js.Dynamic,
    promptText: String,
    onLine: String => Unit,
    onClose: () => Unit,
) extends ConsoleLineReader:
  private val readline = js.Dynamic.global.require("readline")
  private val input = process.stdin
  private val output = process.stdout
  private val editor = new ReplLineEditor
  private var closed = false
  private var promptActive = false
  private var renderedCursorRow = 0

  private val onKeypress: js.Function2[UndefOr[String], js.Dynamic, Unit] =
    (s, key) => handleKeypress(s.toOption.getOrElse(""), key)
  private val onEnd: js.Function0[Unit] =
    () => closeFromInput()

  readline.emitKeypressEvents(input)
  input.on("keypress", onKeypress)
  input.on("end", onEnd)
  if hasFunction(input, "setRawMode") then input.setRawMode(true)
  input.resume()

  def prompt(): Unit =
    if !closed then
      editor.reset()
      promptActive = true
      renderedCursorRow = 0
      output.write(promptText)
      ()

  def close(): Unit =
    if !closed then
      closed = true
      promptActive = false
      restoreInput()

  private def closeFromInput(): Unit =
    if !closed then
      close()
      onClose()

  private def handleKeypress(s: String, key: js.Dynamic): Unit =
    if closed then ()
    else
      val name = keyString(key, "name")
      val keySequence = keyString(key, "sequence")
      val sequence = if keySequence.nonEmpty then keySequence else s
      val ctrl = keyBool(key, "ctrl")
      val meta = keyBool(key, "meta")

      if ctrl && name == "c" then
        output.write("^C\n")
        closeFromInput()
      else if promptActive then
        if isEnter(name, sequence) then
          submit()
        else if ctrl then
          handleCtrl(name)
        else if meta then
          handleMeta(name)
        else
          handleKey(name, s)

  private def handleCtrl(name: String): Unit =
    name match
      case "a" =>
        editor.moveToLineStart()
        render()
      case "e" =>
        editor.moveToLineEnd()
        render()
      case "b" =>
        editor.moveLeft()
        render()
      case "f" =>
        editor.moveRight()
        render()
      case "h" =>
        editor.deleteBeforeCursor()
        render()
      case "d" =>
        if editor.buffer.isEmpty then closeFromInput()
        else
          editor.deleteAtCursor()
          render()
      case "k" =>
        editor.killToLineEnd()
        render()
      case "u" =>
        editor.killToLineStart()
        render()
      case "w" =>
        editor.deletePreviousWord()
        render()
      case "p" =>
        editor.previousHistory()
        render()
      case "n" =>
        editor.nextHistory()
        render()
      case "o" =>
        editor.insertNewline()
        render()
      case "l" =>
        clearScreen()
      case "j" | "m" =>
        submit()
      case _ => ()

  private def handleMeta(name: String): Unit =
    name match
      case "b" =>
        editor.moveWordLeft()
        render()
      case "f" =>
        editor.moveWordRight()
        render()
      case _ => ()

  private def handleKey(name: String, s: String): Unit =
    name match
      case "left" =>
        editor.moveLeft()
        render()
      case "right" =>
        editor.moveRight()
        render()
      case "up" =>
        editor.moveUpOrHistory()
        render()
      case "down" =>
        editor.moveDownOrHistory()
        render()
      case "home" =>
        editor.moveToLineStart()
        render()
      case "end" =>
        editor.moveToLineEnd()
        render()
      case "backspace" =>
        editor.deleteBeforeCursor()
        render()
      case "delete" =>
        editor.deleteAtCursor()
        render()
      case "tab" =>
        editor.insert("\t")
        render()
      case _ =>
        val text = printableText(s)
        if text.nonEmpty then
          editor.insert(text)
          render()

  private def submit(): Unit =
    render(Some(editor.buffer.length))
    output.write("\n")
    promptActive = false
    renderedCursorRow = 0
    onLine(editor.accept())

  private def clearScreen(): Unit =
    output.write("\u001b[2J\u001b[H")
    renderedCursorRow = 0
    render()

  private def render(cursorOverride: Option[Int] = None): Unit =
    if promptActive then
      val cursor = cursorOverride.getOrElse(editor.cursorIndex)
      val rendered = TerminalLineRendering.render(promptText, editor.buffer)
      val endPos = terminalPosition(rendered)
      val cursorPos = terminalPosition(TerminalLineRendering.renderPrefix(promptText, editor.buffer, cursor))

      if renderedCursorRow > 0 then output.write(s"\u001b[${renderedCursorRow}A")
      output.write("\r\u001b[0J")
      output.write(rendered)

      val up = endPos.row - cursorPos.row
      if up > 0 then output.write(s"\u001b[${up}A")
      output.write("\r")
      if cursorPos.column > 0 then output.write(s"\u001b[${cursorPos.column}C")
      renderedCursorRow = cursorPos.row
      ()

  private def terminalPosition(text: String): TerminalPosition =
    val cols = terminalColumns
    var row = 0
    var column = 0
    text.foreach {
      case '\n' =>
        row += 1
        column = 0
      case '\t' =>
        column += 8 - (column % 8)
        while column >= cols do
          row += 1
          column -= cols
      case _ =>
        column += 1
        while column >= cols do
          row += 1
          column -= cols
    }
    TerminalPosition(row, column)

  private def terminalColumns: Int =
    val value = output.selectDynamic("columns")
    if js.isUndefined(value) || value == null then 80
    else math.max(1, value.asInstanceOf[Int])

  private def restoreInput(): Unit =
    try input.removeListener("keypress", onKeypress) catch case _: Throwable => ()
    try input.removeListener("end", onEnd) catch case _: Throwable => ()
    try if hasFunction(input, "setRawMode") then input.setRawMode(false) catch case _: Throwable => ()
    try input.pause() catch case _: Throwable => ()

  private def isEnter(name: String, sequence: String): Boolean =
    name == "return" || name == "enter" || sequence == "\r" || sequence == "\n"

  private def printableText(s: String): String =
    if s.exists(ch => ch == '\u001b' || ch == '\r' || ch == '\n' || ch == 0x7f.toChar) then ""
    else s.filter(ch => ch == '\t' || ch >= ' ')

  private def keyString(key: js.Dynamic, name: String): String =
    if isDefined(key) then
      val value = key.selectDynamic(name)
      if isDefined(value) then value.asInstanceOf[String] else ""
    else ""

  private def keyBool(key: js.Dynamic, name: String): Boolean =
    if isDefined(key) then
      val value = key.selectDynamic(name)
      isDefined(value) && value.asInstanceOf[Boolean]
    else false

  private def isDefined(value: js.Dynamic): Boolean =
    !js.isUndefined(value) && value != null

  private def hasFunction(value: js.Dynamic, name: String): Boolean =
    isDefined(value.selectDynamic(name))

private final case class TerminalPosition(row: Int, column: Int)

private[repl] object TerminalLineRendering:
  val ContinuationPrompt = "     | "

  def render(promptText: String, buffer: String): String =
    promptText + renderBuffer(buffer)

  def renderPrefix(promptText: String, buffer: String, cursor: Int): String =
    promptText + renderBuffer(buffer.substring(0, cursor))

  private def renderBuffer(buffer: String): String =
    buffer.replace("\n", "\n" + ContinuationPrompt)
