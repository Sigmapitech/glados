# Quant Vim Syntax Highlighting

Syntax highlighting support for the Quant programming language in Vim.

## Installation

### Using a Plugin Manager

#### vim-plug

Add to your `~/.vimrc`:

```vim
Plug 'Sigmapitech/glados', {'rtp': 'extension/vim'}
```

#### Vundle

Add to your `~/.vimrc`:

```vim
Plugin 'Sigmapitech/glados', {'rtp': 'extension/vim'}
```

#### Pathogen

```bash
cd ~/.vim/bundle
git clone https://github.com/Sigmapitech/glados.git
ln -s ~/.vim/bundle/glados/extension/vim ~/.vim/bundle/quant-vim
```

### Manual Installation

Copy the files to your Vim runtime directory:

```bash
mkdir -p ~/.vim/syntax ~/.vim/ftdetect
cp extension/vim/syntax/quant.vim ~/.vim/syntax/
cp extension/vim/ftdetect/quant.vim ~/.vim/ftdetect/
```

## Features

- Syntax highlighting for keywords (`fn`, `return`, `if`, `else`, `for`, `while`, etc.)
- Type highlighting (`int`, `float`, `bool`, `str`, `void`)
- Comment support (C-style `//`, `/* */`, and Python-style `#`)
- String literals (single and double quotes)
- Number literals (decimal, hexadecimal, octal, binary)
- Operators and delimiters
- Function name highlighting

## File Association

The plugin automatically recognizes `.qa` files as Quant source files.

## Manual Filetype Setting

If automatic detection doesn't work, you can manually set the filetype:

```vim
:set filetype=quant
```

Or add a modeline at the top or bottom of your file:

```quant
# vim: set filetype=quant:
```
