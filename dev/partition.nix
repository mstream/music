{
  extraInputsFlake = ./.;
  module =
    { inputs, ... }:
    {
      imports = [
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
        inputs.treefmt-nix.flakeModule
        ./mission-control/flake-module.nix
        ./shell/flake-module.nix
        ./treefmt/flake-module.nix
      ];
    };
}
