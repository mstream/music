{
  perSystem =
    { config, pkgs, ... }:
    let
      makeShell =
        shellName: inputsFrom:
        pkgs.mkShell {
          inherit inputsFrom;
          shellHook = ''
            PS1="music-${shellName}-shell \\w > "
          '';
        };
    in
    {
      devShells = rec {
        build = makeShell "build" (with config; [ packages.music ]);
        default = dev;
        dev = makeShell "dev" (
          with config;
          [
            mission-control.devShell
            treefmt.build.devShell
          ]
        );
      };
    };
}
