## Testing instructions

- run `nix develop -c run app-test` to execute unit tests
- do not write any tests, this is human developers responsibility
- do not modify existing test cases

## Code conventions
- do not use functions from PureScript `Partial.Unsafe` module in implementation code.
 
## Style conventions

- if PureScript module provides binary operators for function,  
  such as `<$>`, `<*>`, or '<>', use them over function names. 
- annotate all PureScript values and functions even if compiler does
  not require it.

## Workflow

- do not modify any file inside `test` directory
- if a data type constructor is not exported out of a PureScript module, that's deliberate choice, do not change it
- do not add any new exports from a module, some definitions are kept private on purpose
- make sure that `nix develop -c run app-build` command does not produce any warning messages 
