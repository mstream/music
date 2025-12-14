{
  perSystem = _: {
    treefmt = {
      flakeCheck = true;
      flakeFormatter = true;
      programs = import ./programs.nix;
      projectRootFile = ".git/config";
    };
  };
}
