{
  perSystem =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      mission-control = {
        scripts = import ./scripts.nix {
          inherit
            (import ./lib.nix {
              inherit (pkgs) mktemp;
              execPaths = "${pkgs.esbuild}/bin";
            })
            categories
            script
            ;
          mktemp = "${pkgs.coreutils}/bin/mktemp";
          npm = "${pkgs.nodejs}/bin/npm";
          http-server = lib.getExe pkgs.http-server;
          spago = lib.getExe pkgs.spago-unstable;
          treefmt = lib.getExe config.treefmt.build.wrapper;
        };
        wrapperName = "run";
      };
    };
}
