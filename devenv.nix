{ pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [ pkgs.git pkgs.blas pkgs.lapack pkgs.cabal-install pkgs.haskellPackages.cabal-fmt pkgs.llvmPackages_15.clang pkgs.llvm_15 ];
  stdenv =  pkgs.llvmPackages_15.stdenv;

  # https://devenv.sh/scripts/
  scripts.hello.exec = "echo hello from $GREET";

  enterShell = ''
    hello
    git --version
  '';

  # https://devenv.sh/languages/
  languages.nix.enable = true;
  difftastic.enable = true;

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc982;
  };

  languages.python = {
    enable = true;
    version = "3.10.15";
    uv.enable = true;

  };

  pre-commit.hooks = {
    # lint shell scripts
    shellcheck.enable = true;
    markdownlint.enable = true;
    # lint nix
    # nixfmt.enable = true;
    # deadnix.enable = true;
    # nil.enable = true;
    # statix.enable = true;
    # format haskell
    ormolu.enable = false;
    cabal-fmt.enable = true;
    # lint haskell
    # hlint.enable = true;
  };
  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";

  # See full reference at https://devenv.sh/reference/options/
}
