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
    spago bundle --bundle-type module --ensure-ranges --minify --pedantic-packages --platform browser --pure --strict --verbose --verbose-stats
  '';
  installPhase = ''
    set -x
    mkdir $out
    cp ${./index.html} $out/index.html
    cp index.js $out
  '';
  nativeBuildInputs = buildDependencies;
  spagoLock = ./spago.lock;
  spagoYaml = ./spago.yaml;
  src = ./.;
  version = "0.1.0";
}
