# Scala 3 on JavaScript

This is a fork of the [Scala 3](https://github.com/scala/scala3) that runs on JavaScript.

## Standalone Binaries

With `bun`, we package the Scala 3 compiler into standalone binaries. Check the release page. Pick the asset for your platform from the [latest release](https://github.com/Linyxus/scala3-js/releases/latest):

Download it, make it executable, and run `-help` (example for macOS on Apple Silicon):

```bash
curl -L -o scala3 https://github.com/Linyxus/scala3-js/releases/latest/download/scala3-darwin-arm64
chmod +x scala3
./scala3 -help
./scala3 run <file>.scala  // This compiles and runs a Scala file (using Scala.js)
```

