{
  categories,
  npm,
  script,
  spago,
  ...
}:
{
  app-spec-check = {
    category = categories.checks;
    description = "Check all files";
    exec = script ''
      run app-spec-test
    '';
  };
  app-spec-test-local = {
    category = categories.checks;
    description = "Run UI tests locally";
    exec = script ''
      ${npm} ci 
      ${npm} run install-dependencies
      run app-preview &
      trap "kill %1" EXIT
      ${spago} test --main Test.MainLocal
    '';
  };
  app-spec-test-prod = {
    category = categories.checks;
    description = "Run UI tests against production";
    exec = script ''
      ${npm} ci 
      ${npm} run install-dependencies
      ${spago} test 
    '';
  };
}
