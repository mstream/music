{
  categories,
  http-server,
  npm,
  script,
  spago,
  treefmt,
  ...
}:
{
  compile = {
    description = "Compile code";
    exec = script "app" ''
      ${spago} bundle --bundle-type module --platform browser
    '';
  };
  update-npm-packages = {
    description = "Update NPM packages";
    exec = script "app" ''
      ${npm} update
    '';
  };
  update-spago-packages = {
    description = "Update NPM packages";
    exec = script "app" ''
      ${spago} build
    '';
  };
  build-app = {
    category = categories.builds;
    description = "Build application";
    exec = script "app" ''
      ${spago} bundle --bundle-type module --platform browser
    '';
  };
  check-all = {
    category = categories.checks;
    description = "Check all files";
    exec = script "." ''
      ${treefmt} --ci --verbose
    '';
  };
  format-all = {
    category = categories.formats;
    description = "Format all files";
    exec = script "." ''
      ${treefmt}
      cd app
      ${npm} exec purescript-psa@0.9.0 --censor-lib --json-errors --stash 2>&1 | ${npm} exec purescript-suggest@2.2.0 --apply
    '';
  };
  preview-app = {
    category = categories.previews;
    description = "Preview website";
    exec = script "app" ''
      run build-app
      ${http-server} .
    '';
  };
}
