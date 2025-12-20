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
      ${spago} bundle --bundle-type module --platform browser
    '';
  };
  app-preview = {
    category = categories.previews;
    description = "Preview website";
    exec = script ''
      ${http-server} . &
      ${watchexec} --exts html,js,purs,yaml --print-events -- run build-app
      kill %1
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
