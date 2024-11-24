{
  description = "Nand2Tetris";

  inputs = {
    nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    zig2nix.url = "github:Cloudef/zig2nix";
    nand2tetris-tools = {
      url = "github:0x5a4/nand2tetris-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      nand2tetris-tools,
      zig2nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        zig-env = zig2nix.outputs.zig-env.${system} {
          zig = pkgs.zig_0_12;
        };
        system-triple = zig-env.lib.zigTripleFromString system;

        buildZigPackage =
          src:
          zig-env.packageForTarget system-triple {
            src = zig-env.pkgs.lib.cleanSource src;

            zigPreferMusl = true;
            zigDisableWrap = true;
          };
      in
      {
        packages = {
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

          hackas = buildZigPackage ./06;
          week06 = self.packages.${system}.hackas;

          hackvmc-part1 = buildZigPackage ./07;
          week07 = self.packages.${system}.hackvmc-part1;
          hackvmc = buildZigPackage ./08;
          week08 = self.packages.${system}.hackvmc;

          diggy-diggy-hole = pkgs.stdenv.mkDerivation {
            name = "diggy-diggy-hole";
            src = ./09;

            nativeBuildInputs = [ nand2tetris-tools.packages.${system}.default ];

            buildPhase = ''
              JackCompiler.sh  
            '';

            installPhase = ''
              mkdir $out
              cp *.vm $out
            '';
          };
          week09 = self.packages.${system}.diggy-diggy-hole;

          jackc-part1 = buildZigPackage ./10;
          week10 = self.packages.${system}.jackc-part1;

          jackc = buildZigPackage ./10;
          week11 = self.packages.${system}.jackc;
        };

        apps = {
          apps.default = zig-env.app [ ] "zig build run -- \"$@\"";
          apps.zig-build = zig-env.app [ ] "zig build \"$@\"";
          apps.zig-test = zig-env.app [ ] "zig build test -- \"$@\"";

          hackas = flake-utils.lib.mkApp {
            drv = self.packages.${system}.hackas;
          };

          hackvmc = flake-utils.lib.mkApp {
            drv = self.packages.${system}.hackvmc;
          };

          jackc = flake-utils.lib.mkApp {
            drv = self.packages.${system}.jackc;
          };
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.zig_0_12
            nand2tetris-tools.packages.${system}.default
          ];
        };
      }
    );
}
