{
  inputs = {
    flake-root.url = "github:srid/flake-root/master";
    mission-control.url = "github:Platonic-Systems/mission-control/master";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix/main";
    };
  };
  outputs = _: { };
}
