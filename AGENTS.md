## Dev environment tips

## Testing instructions

- run `nix develop -c run app-test` to execute unit tests
- do not write any tests, this is human developers responsibility

## Code style

- when possible, do not exceed code line length of 72 columns
- keep code comments to minimum 
- when writing any key-value pair data structures, sort keys alphabetically

## Workflow

- make sure that `nix develop -c run app-build` command does not produce any warning messages 
- use conventional commit messages
