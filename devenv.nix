{ pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [ pkgs.git pkgs.haskellPackages.cabal-fmt pkgs.difftastic ];

  # https://devenv.sh/scripts/
  scripts.hello.exec = "echo hello from $GREET";

  enterShell = ''
    hello
    git --version
  '';

  # https://devenv.sh/languages/
  languages.nix.enable = true;

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc948;
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
    ormolu.enable = true;
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
