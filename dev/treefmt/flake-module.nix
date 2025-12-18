let
  documentWidth = 72;
in
{
  perSystem = _: {
    treefmt = {
      flakeCheck = true;
      flakeFormatter = true;
      programs = import ./programs.nix { inherit documentWidth; };
      projectRootFile = ".git/config";
      settings.formatter.purs-tidy.options = [
        "--arrow-first"
        "--import-sort-ide"
        "--import-wrap-auto"
        "--indent"
        "2"
        "--ribbon"
        "1"
        "--threads"
        "4"
        "--unicode-always"
        "--width"
        "${builtins.toString documentWidth}"
      ];
    };
  };
}
