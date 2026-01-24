{
  categories,
  httpServer,
  nix,
  npm,
  script,
  spago,
  watchexec,
  ...
}:
{
  app-build = {
    category = categories.builds;
    description = "Build application";
    exec = script ''
      rm -f index.js
      ${spago} bundle \
        --bundle-type module \
        --module MainDebug \
        --package app \
        --platform browser
    '';
  };
  app-check = {
    category = categories.checks;
    description = "Check all files";
    exec = script ''
      rm -rf output
      ${spago} build --ensure-ranges --pedantic-packages --pure --strict
      run app-test
      ${nix} build .#music
    '';
  };
  app-format = {
    category = categories.formats;
    description = "Format application source code";
    exec =
      let
        psSuggest = "${npm} exec --package=purescript-suggest@2.2.0 -- ps-suggest";
      in
      script ''
        rm -rf output
        ${spago} build --json-errors | ${psSuggest} --apply
      '';
  };
  app-preview = {
    category = categories.previews;
    description = "Preview website";
    exec = script ''
      ${httpServer} . &
      trap "kill %1" EXIT
      ${watchexec} --exts html,js,purs,yaml --print-events -- "run app-build; echo Ctrl-C to exit"
    '';
  };
  app-test = {
    category = categories.checks;
    description = "Test application";
    exec = script ''
      ${spago} test 
    '';
  };
  app-update-npm-packages = {
    description = "Update NPM packages";
    exec = script ''
      ${npm} update
    '';
  };
  app-update-spago-packages = {
    description = "Update Spago packages";
    exec = script ''
      ${spago} build
    '';
  };
}
