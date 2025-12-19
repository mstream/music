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
  app-test = {
    description = "Test application";
    category = categories.checks;
    exec = script ''
      ${spago} test 
    '';
  };
  compile = {
    description = "Compile code";
    exec = script ''
      ${spago} bundle --bundle-type module --platform browser
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
  build-app = {
    category = categories.builds;
    description = "Build application";
    exec = script ''
      ${spago} bundle --bundle-type module --platform browser
    '';
  };
  preview-app = {
    category = categories.previews;
    description = "Preview website";
    exec = script ''
      ${http-server} . &
      ${watchexec} --exts html,js,purs,yaml --print-events -- run build-app
      kill %1
    '';
  };
}
