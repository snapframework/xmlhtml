{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-builder, blaze-html, blaze-markup
      , bytestring, containers, directory, HUnit, parsec, stdenv
      , test-framework, test-framework-hunit, text, unordered-containers
      }:
      mkDerivation {
        pname = "xmlhtml";
        version = "0.2.3.6";
        src = ./.;
        libraryHaskellDepends = [
          base blaze-builder blaze-html blaze-markup bytestring containers
          parsec text unordered-containers
        ];
        testHaskellDepends = [
          base blaze-builder blaze-html blaze-markup bytestring directory
          HUnit test-framework test-framework-hunit text
        ];
        homepage = "https://github.com/snapframework/xmlhtml";
        description = "XML parser and renderer with HTML 5 quirks mode";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
