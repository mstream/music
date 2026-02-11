{
  categories,
  httpServer,
  jq,
  mkTemp,
  nix,
  npm,
  script,
  spago,
  watchexec,
  ...
}:
let
  npmExec =
    package: command: "${npm} exec --package=${package} -- ${command}";
  c8 = npmExec "c8@10.1.3" "c8";
  psSuggest = npmExec "purescript-suggest@2.2.0" "ps-suggest";
in
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
      ${spago} build --pedantic-packages --pure --strict
      run app-test
      ${nix} build .#music
    '';
  };
  app-format = {
    category = categories.formats;
    description = "Format application source code";
    exec = script ''
      rm -rf output
      ${spago} build --json-errors | ${psSuggest} --apply
    '';
  };
  app-list-modules = {
    description = "List all PureScript modules belonging to this project.";
    exec =
      let
        jqExpr = "with_entries(select(.value.path | startswith(\".spago\") | not)) | keys | .[]";
      in
      script ''
        ${spago} graph modules --json | ${jq} -r '${jqExpr}'
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
      npm ci
      c8_conf=$(${mkTemp})
      run app-list-modules | ${jq} -Rs '{all: true, clean: true, include: split("\n") | map(select(length > 0) |= "output/" + . + "/*.js"), reporter: ["text"]}' > "''${c8_conf}"
      ${c8} --config "''${c8_conf}" ${spago} test 
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
