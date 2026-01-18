deps:
let
  app = import ./scripts/app.nix (
    deps // { script = deps.script "app"; }
  );
  global = import ./scripts/global.nix (
    deps // { script = deps.script "."; }
  );
in
global // app
