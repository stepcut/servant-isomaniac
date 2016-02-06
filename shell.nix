{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, isomaniac, servant, stdenv
      , text, transformers, exceptions, safe, http-api-data, hsx2hs, hsp
      }:
      mkDerivation {
        pname = "servant-isomaniac";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring isomaniac servant text transformers exceptions safe http-api-data hsx2hs hsp
        ];
        buildTools = [ pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghc ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
