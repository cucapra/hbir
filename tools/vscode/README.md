# HBIR for Visual Studio Code

This is a [Visual Studio Code][vscode] extension with syntax highlighting for HBIR.

To use it, you can do one of two things:

- Use VSCode's extension debugging mode. Open this directory in VSCode and launch the debugger to get a window with the extension enabled.
- Build and install the extension:
    - Install the [vsce][] tool by typing `npm install -g vsce`.
    - Then type `vsce package` in this directory to generate a `.vsix` file.
    - Install the package by typing something like
      `code --install-extension *.vsix`.

[vscode]: https://code.visualstudio.com
[vsce]: https://code.visualstudio.com/api/working-with-extensions/publishing-extension
