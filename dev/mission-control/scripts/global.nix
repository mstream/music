{
  categories,
  npm,
  script,
  treefmt,
  ...
}:
{
  check-all = {
    category = categories.checks;
    description = "Check all files";
    exec = script ''
      run app-test
      ${treefmt} --ci --verbose
    '';
  };
  format-all = {
    category = categories.formats;
    description = "Format all files";
    exec = script ''
      ${treefmt}
      cd app
      ${npm} exec purescript-psa@0.9.0 --censor-lib --json-errors --stash 2>&1 | ${npm} exec purescript-suggest@2.2.0 --apply
    '';
  };
}
