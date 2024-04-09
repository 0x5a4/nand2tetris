{
  description = "Nand2Tetris";

  inputs = {
    nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
      in rec {
        packages = rec {
          nand2tetristools = pkgs.stdenv.mkDerivation {
            name = "nand2tetristools";
            src = builtins.fetchTarball {
              url = "https://drive.google.com/uc?export=download&id=1xZzcMIUETv3u3sdpM_oTJSTetpVee3KZ";
              sha256 = "sha256:0n31p1kn4by3wmf1wqf40q7wnywxr8b1nxi49y9d08nniiricc3s";
            };

            installPhase = ''
              mkdir -p $out/bin
              cp -r ./tools/* $out/bin
              chmod a+x $out/bin/*.sh
            '';

            postInstallPhase = ''
            '';
          };

          hardwaresim = pkgs.symlinkJoin {
            name = "hardwaresim";
            paths = [nand2tetristools] ++ (with pkgs; [
              openjdk 
            ]);
            buildInputs = [pkgs.makeWrapper];
            postBuild = "wrapProgram $out/bin/HardwareSimulator.sh --prefix PATH : $out/lib/openjdk/bin";
          };
        };

        apps = {
          hardwaresim = flake-utils.lib.mkApp {
            drv = self.packages.${system}.hardwaresim;
            exePath = "/bin/HardwareSimulator.sh";
          };
        };
      }
    );
}
