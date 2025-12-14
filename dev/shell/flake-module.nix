{
  perSystem =
    { config, pkgs, ... }:
    {
      devShells.default = pkgs.mkShell {
        inputsFrom = [ config.mission-control.devShell ];
        shellHook = ''
          PS1="music-shell \\w > "
        '';
      };
    };
}
