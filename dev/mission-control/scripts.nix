{
  categories,
  http-server,
  npm,
  script,
  spago,
  treefmt,
  watchexec,
  ...
}:
let
  app = import ./scripts/app.nix {
    inherit
      categories
      http-server
      npm
      spago
      watchexec
      ;
    script = script "app";
  };
  global = import ./scripts/global.nix {
    inherit categories npm treefmt;
    script = script ".";
  };
in
global // app
