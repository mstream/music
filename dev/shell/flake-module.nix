{
  perSystem =
    { config, pkgs, ... }:
    let
      makeShell =
        shellName: packages: inputsFrom:
        pkgs.mkShell {
          inherit inputsFrom packages;
          shellHook = ''
            PS1="music-${shellName}-shell \\w > "
            shopt -s globstar
          '';
        };
    in
    {
      devShells = rec {
        build = makeShell "build" (with pkgs; [ esbuild ]) (
          with config; [ packages.music ]
        );
        default = dev;
        dev =
          makeShell "dev"
            (with pkgs; [
              esbuild
              spago-unstable
            ])
            (
              with config;
              [
                mission-control.devShell
                treefmt.build.devShell
              ]
            );
      };
    };
}
