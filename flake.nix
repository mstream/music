{
  description = "A very basic flake";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts/main";
    flake-utils.url = "github:numtide/flake-utils?ref=main";
    mk-spago-derivation = {
      url = "github:jeslie0/mkSpagoDerivation";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-25.11-darwin";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      flake-parts,
      flake-utils,
      mk-spago-derivation,
      purescript-overlay,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = { };
      imports = [ flake-parts.flakeModules.partitions ];
      partitionedAttrs = {
        checks = "dev";
        devShells = "dev";
        formatter = "dev";
      };
      partitions.dev = import ./dev/partition.nix;
      perSystem =
        { pkgs, system, ... }:
        let
          buildDependencies = with pkgs; [
            esbuild
            purs-unstable
            spago-unstable
          ];
        in
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              mk-spago-derivation.overlays.default
              purescript-overlay.overlays.default
            ];
          };

          packages = rec {
            default = music;
            music = pkgs.callPackage ./app/package.nix {
              inherit buildDependencies;
            };
          };
        };
      systems = with flake-utils.lib.system; [ aarch64-darwin ];
    };
}
