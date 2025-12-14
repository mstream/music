{
  buildDependencies,
  isDev,
  mkSpagoDerivation,
  nodejs,
}:
let
  bundleDevScript = ''
    set -x
    spago bundle --bundle-type module --platform browser
  '';
  bundleProdScript = ''
    set -x
    spago bundle --bundle-type module --pedantic-packages --platform browser --strict
  '';
in
mkSpagoDerivation {
  buildNodeModulesArgs = {
    inherit nodejs;
    npmRoot = ./.;
  };
  buildPhase = if isDev then bundleDevScript else bundleProdScript;
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
