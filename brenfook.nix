{ mkDerivation, base, mtl, cabal-install, stdenv }:
mkDerivation {
  pname = "brenfook";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base mtl ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  buildDepends = [ cabal-install ];
  homepage = "https://github.com/githubuser/brenfook#readme";
  license = stdenv.lib.licenses.bsd3;
}
