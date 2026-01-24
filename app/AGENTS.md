## General Instructions

- do not add or modify any tests, this is human's responsibility
- do not modify any file inside the `test` directory
- if a data type constructor is not exported out of a PureScript module, that's deliberate choice, do not change it
- do not add any new exports from a module, some definitions are kept private on purpose
- do not use functions from PureScript `Partial.Unsafe` module in implementation code.
 
## Coding Style

- if PureScript module provides binary operators for function,  
  such as `<$>`, `<*>`, or '<>', use them over function names. 
- annotate all PureScript values and functions even if compiler does
  not require it.


