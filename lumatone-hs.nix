{ mkDerivation, base, colour, ini, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "ltn-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base colour ini text unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
