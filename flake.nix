{
  description = "Fretboard Theory development environment";

  inputs = {
    devenv-root = {
      url = "file+file:///dev/null";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    mcp-hls = {
      url = "github:o1lo01ol1o/mcp-haskell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, devenv-root, mcp-hls, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devenv.flakeModule ];
      systems = [
        "x86_64-linux"
        "i686-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem = { config, pkgs, system, ... }:
        let
          ghcVersion = "ghc98";
          haskellPackages = pkgs.haskell.packages.${ghcVersion};
          ghcWithHMatrix =
            haskellPackages.ghcWithPackages (ps: with ps; [ hmatrix ]);

          mcpPackages = inputs."mcp-hls".packages;
          mcpGhcid =
            if builtins.hasAttr system mcpPackages then
              let perSystemPackages = mcpPackages.${system};
              in if builtins.hasAttr "mcp-ghcid" perSystemPackages then
                perSystemPackages."mcp-ghcid"
              else
                throw "mcp-ghcid package not available for system ${system}"
            else
              throw "mcp-haskell flake does not expose packages for system ${system}";

          fretboardThoeryDrv =
            haskellPackages.callCabal2nix "fretboard-thoery"
              ./packages/fretboard-thoery { };
          fretboardDiagramDrv =
            haskellPackages.callCabal2nix "fretboard-diagram"
              ./packages/fretboard-diagram { };

          devenvRootPath = inputs."devenv-root".outPath;
          devenvRootContent =
            if builtins.pathExists devenvRootPath then
              builtins.readFile devenvRootPath
            else
              "";

          devenvShell = {
            devenv.root = pkgs.lib.mkIf (devenvRootContent != "") devenvRootContent;

            packages = [
              pkgs.git
              mcpGhcid
              pkgs.openblasCompat
              pkgs.cabal-install
              pkgs.haskellPackages.cabal-fmt
              pkgs.llvmPackages_18.clang
              pkgs.llvm_18
              pkgs.nixfmt
            ];

            stdenv = pkgs.llvmPackages_18.stdenv;

            languages.nix.enable = true;
            languages.typescript.enable = true;
            languages.javascript = {
              enable = true;
              bun = {
                enable = true;
                install.enable = true;
              };
            };

            languages.haskell = {
              enable = true;
              package = ghcWithHMatrix;
            };

            languages.python = {
              enable = true;
              package = pkgs.python310;
              uv.enable = true;
            };

            difftastic.enable = true;

            scripts.full-stack.exec = ''
              set -euo pipefail
              : ''${DEVENV_ROOT:="$(pwd)"}
              cd "''${DEVENV_ROOT}"
              cabal run exe:harmonic-function-server &
              SERVER_PID=$!
              (
                cd web/harmonic-analyzer
                if [ ! -d node_modules ]; then
                  bun install
                fi
                bun run dev
              ) &
              FRONT_PID=$!
              trap 'kill $SERVER_PID $FRONT_PID 2>/dev/null' EXIT
              wait -n $SERVER_PID $FRONT_PID
            '';
          };
        in {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              allowBroken = true;
            };
          };

          packages = {
            default = fretboardThoeryDrv;
            "fretboard-thoery" = fretboardThoeryDrv;
            "fretboard-diagram" = fretboardDiagramDrv;
            "mcp-ghcid" = mcpGhcid;
          };

          devenv.shells.default = devenvShell;

          apps = {
            "mcp-ghcid" = {
              type = "app";
              program = "${mcpGhcid}/bin/mcp-ghcid";
            };
          };
        };

      flake = { };
    };
}
