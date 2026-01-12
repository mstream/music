{
  categories,
  nix,
  npm,
  script,
  ...
}:
{
  check-all = {
    category = categories.checks;
    description = "Check all files";
    exec = script ''
      run app-test
      ${nix} flake check
    '';
  };
  format-all = {
    category = categories.formats;
    description = "Format all files";
    exec = script ''
      cd app
      ${npm} exec purescript-psa@0.9.0 --censor-lib --json-errors --stash 2>&1 | ${npm} exec purescript-suggest@2.2.0 --apply
      ${nix} fmt
    '';
  };
}
