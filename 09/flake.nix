{
  description = "hackvmc";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable";
    nand2tetris.url = "github:0x5a4/nand2tetris-flake";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    nand2tetris,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      defaultPackage = pkgs.stdenv.mkDerivation {
        name = "diggy-diggy-hole";
        src = ./.;

        nativeBuildInputs = [
          nand2tetris.defaultPackage.${system}
        ];

        buildPhase = ''
          JackCompiler.sh  
        '';

        installPhase = ''
          mkdir $out
          cp *.vm $out/
        '';
      };
    
      apps.vmemu = nand2tetris.apps.${system}.vmemulator;
      apps.jackc = nand2tetris.apps.${system}.jackcompiler;
    });
}
