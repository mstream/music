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
  app-spec-test = {
    category = categories.checks;
    description = "Run UI tests";
    exec = script ''
      ${npm} ci 
      ${npm} run install-dependencies
      ${spago} test
    '';
  };
}
