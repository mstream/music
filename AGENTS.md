## Dev environment tips

Use Nix dev shell for executing commands. It contains programs
that the default shell may lack.

## Testing instructions

- run `nix develop -c run app-test` to execute unit tests
- do not write any tests, this is human developers responsibility
- do not modify existing test cases

## Code conventions
- do not use functions from PureScript `Partial.Unsafe` module in implementation code.
 
## Style conventions

- try to reuse already written code as much as possible
- when possible, do not exceed code line length of 72 columns
- keep code comments to minimum 
- use descriptive names inside code to minimize need for adding comments 
- when writing any key-value pair data structures, sort keys alphabetically
- if PureScript module provides binary operators for function,  
  such as `<$>`, `<*>`, or '<>', use them over function names. 
- annotate all PureScript values and functions even if compiler does
  not require it.

## Workflow

- do not modify any file inside `app/test` directory
- make sure that `nix develop -c run app-build` command does not produce any warning messages 
- use conventional commit messages
