# tl_playground

`tl_playground` is a very basic compiler, interpreter, and language server that can be used as the starting point for language developement

## Repo Overview

  - `tl_compiler` - The main compiler binary
  - `tl_core` - The core functionality such as parsing, lexing, and representation of the ast
  - `tl_lsp` - The language server program. This, for now, contains a typescript vscode extension with a rust LSP server (in the tl_lsp/lsp_server directory)
  - `tl_util` - Utility functionality such as tree printing and refs
  - `tl_vm` - Related to interpreting. This is not a binary but can be used in the compiler binary.
  - `test_files` - This is where I like to keep the target language source files for testing