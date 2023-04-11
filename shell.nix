{ pkgs ? import <unstable> {}
}:

pkgs.stdenv.mkDerivation rec {
  name = "quotes-api";

  nativeBuildInputs = [
  ];

  buildInputs = [
    pkgs.zlib
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.cabal-plan
    pkgs.haskellPackages.haskell-language-server
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    export LANG=en_US.UTF-8
  '';
}
