## Dev environment tips

## Testing instructions

- run `nix develop -c run app-test` to execute unit tests
- do not write any tests, this is human developers responsibility

## Code style

- try to reuse already written code as much as possible
- when possible, do not exceed code line length of 72 columns
- keep code comments to minimum 
- use descriptive names inside code to minimize need for adding comments 
- when writing any key-value pair data structures, sort keys alphabetically

## Workflow

- make sure that `nix develop -c run app-build` command does not produce any warning messages 
- use conventional commit messages
