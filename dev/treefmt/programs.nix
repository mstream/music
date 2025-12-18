{ documentWidth, ... }:
{
  actionlint.enable = true;
  deadnix = {
    enable = true;
    no-lambda-arg = false;
    no-lambda-pattern-names = false;
    no-underscore = false;
  };
  dos2unix.enable = true;
  jsonfmt.enable = true;
  keep-sorted.enable = true;
  nixfmt = {
    enable = true;
    strict = true;
    width = documentWidth;
  };
  purs-tidy = {
    enable = true;
    includes = [ "*.purs" ];
  };
  statix.enable = true;
  typos = {
    binary = false;
    enable = true;
    excludes = [
      "*.js"
      "*.png"
    ];
    hidden = true;
    isolated = false;
    locale = "en";
    noIgnoreGlobal = false;
    noCheckFilenames = false;
    noIgnore = false;
    noIgnoreDot = false;
    noIgnoreParent = false;
    noIgnoreVCS = false;
    noUnicode = true;
  };
}
