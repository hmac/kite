{ mkDerivation, base, containers, directory, extra, filepath
, hedgehog, hlint, hpack, hspec, hspec-megaparsec
, hw-hspec-hedgehog, megaparsec, mtl, optparse-generic
, pretty-simple, prettyprinter, prettyprinter-ansi-terminal
, QuickCheck, semigroupoids, stdenv, text, transformers
}:
mkDerivation {
  pname = "lam";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory extra filepath hedgehog hspec
    hspec-megaparsec hw-hspec-hedgehog megaparsec mtl optparse-generic
    pretty-simple prettyprinter prettyprinter-ansi-terminal QuickCheck
    semigroupoids text transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base containers directory extra filepath hedgehog hspec
    hspec-megaparsec hw-hspec-hedgehog megaparsec mtl optparse-generic
    pretty-simple prettyprinter prettyprinter-ansi-terminal QuickCheck
    semigroupoids text transformers
  ];
  testHaskellDepends = [
    base containers directory extra filepath hedgehog hlint hspec
    hspec-megaparsec hw-hspec-hedgehog megaparsec mtl optparse-generic
    pretty-simple prettyprinter prettyprinter-ansi-terminal QuickCheck
    semigroupoids text transformers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/hmac/lam#readme";
  description = "A Haskell-like language for scripting and web apps";
  license = stdenv.lib.licenses.bsd3;
}
