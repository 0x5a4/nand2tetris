{
  description = "Nand2Tetris";

  inputs = {
    nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nand2tetris = {
      url = "github:0x5a4/nand2tetris-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = {
    self,
    flake-utils,
    nand2tetris,
    nixpkgs,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
      in rec {
        packages = rec {
          submit = pkgs.writeScriptBin "run.sh" ''
            #!/usr/bin/env bash   

            if (( $# == 0)); then
              echo "usage: submit <projectnumber>"
              exit
            fi

            OLDPWD=$PWD
            
            cd $@
            ${pkgs.zip}/bin/zip -q9r "$OLDPWD/wienstroer$@.zip" .
          '';
        };

        apps = {
          submit = flake-utils.lib.mkApp {
            drv = packages.submit;
          };
          
          hardwaresim = nand2tetris.apps.${system}.hardwaresimulator;
          cpusim = nand2tetris.apps.${system}.cpusimulator;
          nandasm = nand2tetris.apps.${system}.assembler;
          nandvmemu = nand2tetris.apps.${system}.vmemulator;
          nandcompiler = nand2tetris.apps.${system}.jackcompiler;
        };
      }
    );
}
