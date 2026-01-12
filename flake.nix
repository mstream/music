{
  description = "A very basic flake";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts?ref=main";
    flake-utils.url = "github:numtide/flake-utils?ref=main";
    mk-spago-derivation = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        ps-overlay.follows = "ps-overlay";
      };
      url = "github:jeslie0/mkSpagoDerivation?ref=main";
    };
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-25.11-darwin";
    ps-overlay = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:thomashoneyman/purescript-overlay?ref=83335b3796ea1874a324d78253bc15db481c3a55";
    };
  };

  outputs =
    inputs@{
      flake-parts,
      flake-utils,
      mk-spago-derivation,
      ps-overlay,
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
              ps-overlay.overlays.default
            ];
          };
          packages = rec {
            default = music;
            music = pkgs.callPackage ./app/package.nix {
              inherit buildDependencies;
            };
          };
        };
      systems = with flake-utils.lib.system; [
        aarch64-darwin
        x86_64-linux
      ];
    };
}
