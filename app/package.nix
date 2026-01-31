{
  buildDependencies,
  mkSpagoDerivation,
  nodejs,
}:
mkSpagoDerivation {
  buildNodeModulesArgs = {
    inherit nodejs;
    npmRoot = ./.;
  };
  buildPhase = ''
    set -x
    spago bundle \
      --bundle-type module \
      --minify \
      --module Main\
      --package app \
      --pedantic-packages \
      --platform browser \
      --pure \
      --strict \
      --verbose \
      --verbose-stats
  '';
  checkPhase = ''
    set -x
    export PATH=''${PATH}:${nodejs}/bin
    spago test
  '';
  doCheck = true;
  installPhase = ''
    set -x
    mkdir $out
    cp ${./index.css} $out/index.css
    cp ${./index.html} $out/index.html
    cp index.js $out
  '';
  nativeBuildInputs = buildDependencies;
  spagoLock = ./spago.lock;
  spagoYaml = ./spago.yaml;
  src = ./.;
  version = "0.1.0";
}
