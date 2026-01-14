{
  categories,
  http-server,
  nix,
  npm,
  script,
  spago,
  watchexec,
  ...
}:
let
  app = import ./scripts/app.nix {
    inherit
      categories
      http-server
      nix
      npm
      spago
      watchexec
      ;
    script = script "app";
  };
  global = import ./scripts/global.nix {
    inherit categories nix npm;
    script = script ".";
  };
in
global // app
