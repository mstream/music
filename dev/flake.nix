{
  inputs = {
    flake-root.url = "github:srid/flake-root?ref=master";
    mission-control.url = "github:Platonic-Systems/mission-control?ref=master";
    nixpkgs.url = "github:nixos/nixpkgs?ref=5d6bdbddb4695a62f0d00a3620b37a15275a5093";
    nixpkgs-unstable.url = "github:nixos/nixpkgs?ref=master";
    ps-overlay = {
      url = "github:thomashoneyman/purescript-overlay?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix?ref=4612a4b80eb0422b014720db25a5c0b9ae72f89a";
    };
  };
  outputs = _: { };
}
