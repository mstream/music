{
  perSystem =
    { lib, pkgs, ... }:
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
          git = lib.getExe pkgs.git;
          mktemp = "${pkgs.coreutils}/bin/mktemp";
          nix = lib.getExe pkgs.nix;
          npm = "${pkgs.nodejs}/bin/npm";
          http-server = lib.getExe pkgs.http-server;
          spago = lib.getExe pkgs.spago-unstable;
          watchexec = lib.getExe pkgs.watchexec;
        };
        wrapperName = "run";
      };
    };
}
