{
  description = "jackc";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    zig2nix.url = "github:Cloudef/zig2nix";
    nand2tetris.url = "github:0x5a4/nand2tetris-flake";
  };

  outputs = {
    zig2nix,
    flake-utils,
    nand2tetris,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      env = zig2nix.outputs.zig-env.${system} {};
      system-triple = env.lib.zigTripleFromString system;
    in
      with builtins;
      with env.lib;
      with env.pkgs.lib; rec {
        # nix build .#target.{zig-target}
        # e.g. nix build .#target.x86_64-linux-gnu
        packages.target = genAttrs allTargetTriples (target:
          env.packageForTarget target {
            src = cleanSource ./.;

            # Wont be usable from nix tho
            zigPreferMusl = true;
            zigDisableWrap = true;
          });

        # nix build .
        packages.default = packages.target.${system-triple}.override {
          # Prefer nix friendly settings.
          zigPreferMusl = false;
          zigDisableWrap = false;
        };

        # nix run .
        apps.default = env.app [] "zig build run -- \"$@\"";

        # nix run .#build
        apps.build = env.app [] "zig build \"$@\"";

        # nix run .#test
        apps.test = env.app [] "zig build test -- \"$@\"";

        apps.jackc = nand2tetris.apps.${system}.jackcompiler;
        apps.vmemu = nand2tetris.apps.${system}.vmemulator;
      });
}
