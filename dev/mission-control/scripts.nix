deps:
let
  app = import ./scripts/app.nix (
    deps // { script = deps.script "app"; }
  );
  appSpec = import ./scripts/app-spec.nix (
    deps // { script = deps.script "app-spec"; }
  );
  global = import ./scripts/global.nix (
    deps // { script = deps.script "."; }
  );
in
global // app // appSpec
