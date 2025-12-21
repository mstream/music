{
  categories,
  http-server,
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
      ${spago} bundle --bundle-type module --platform browser
    '';
  };
  app-preview = {
    category = categories.previews;
    description = "Preview website";
    exec = script ''
      ${http-server} . &
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
  update-npm-packages = {
    description = "Update NPM packages";
    exec = script ''
      ${npm} update
    '';
  };
  update-spago-packages = {
    description = "Update Spago packages";
    exec = script ''
      ${spago} build
    '';
  };
}
