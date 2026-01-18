# Quant Language

Syntax highlighting for the Quant programming language.

## Features

- Syntax highlighting for `.quant` and `.qa` files
- Custom file icon for Quant files
- **Code snippets** for common Quant patterns

## Snippets

The extension includes 25+ code snippets to speed up development:

### Quick Reference

| Prefix | Description |
|--------|-------------|
| `fn` | Function definition |
| `main` | Main function |
| `if` | If statement |
| `ife` | If-else statement |
| `ifel` | If-elif-else statement |
| `for` | For loop |
| `while` | While loop |
| `vari`, `varf`, `varb`, `vars`, `vara` | Variable declarations (int, float, bool, str, array) |
| `const` | Constant declaration |
| `import` | Import statement |
| `from` | Import from statement |
| `print` | Print statement |
| `ret` | Return statement |

**And more!** Type a prefix and press `Tab` or `Ctrl+Space` to see all available snippets.

## Installation

### Building from Source

1. **Prerequisites**
   - Install [Node.js](https://nodejs.org/) (v14 or higher)
   - Install [Visual Studio Code](https://code.visualstudio.com/)
   - Install `vsce` (Visual Studio Code Extension Manager):

     ```bash
     npm install -g @vscode/vsce
     ```

2. **Build the Extension**

   Navigate to the extension directory:

   ```bash
   cd extension/vscode/quant-highlighter
   ```

   Package the extension into a `.vsix` file:

   ```bash
   vsce package
   ```

   This will generate a file named `quant-1.0.0.vsix` (or similar, depending on the version).

3. **Install the Extension**

   **Option A: Install via Command Line**

   ```bash
   code --install-extension quant-1.0.0.vsix
   ```

   **Option B: Install via VS Code UI**
   - Open VS Code
   - Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS) to open the Command Palette
   - Type "Extensions: Install from VSIX..."
   - Select the generated `quant-1.0.0.vsix` file

   **Option C: Manual Installation**

   Copy the `.vsix` file to your VS Code extensions directory:
   - **Linux**: `~/.vscode/extensions/`
   - **macOS**: `~/.vscode/extensions/`
   - **Windows**: `%USERPROFILE%\.vscode\extensions\`

   Then extract it:

   ```bash
   # Linux/macOS
   unzip quant-1.0.0.vsix -d ~/.vscode/extensions/quant-1.0.0

   # Or use the built-in command
   code --install-extension quant-1.0.0.vsix
   ```

4. **Reload VS Code**

   After installation, reload VS Code to activate the extension:
   - Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS)
   - Type "Developer: Reload Window" and press Enter

## Usage

Simply open any `.quant` or `.qa` file and enjoy syntax highlighting.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

BSD 2-Clause License. See the [LICENSE](./LICENSE) file for details.
