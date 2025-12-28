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
          '';
        };
    in
    {
      devShells = rec {
        build = makeShell "build" [ ] (with config; [ packages.music ]);
        default = dev;
        dev = makeShell "dev" (with pkgs; [ spago-unstable ]) (
          with config;
          [
            mission-control.devShell
            treefmt.build.devShell
          ]
        );
      };
    };
}
