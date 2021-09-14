# To run with ghc864 visible:
# nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/596739026bbe0c693708119e5cd3e1b3aa88fd2d.tar.gz

{ mkDerivation, base, hidden-char, hpack, lens, lib, parallel
, random, split, utility-ht, vector
}:
mkDerivation {
  pname = "tetriskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base hidden-char lens parallel random split utility-ht vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base hidden-char lens parallel random split utility-ht vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/harryaskham/tetriskell#readme";
  license = lib.licenses.bsd3;
}
