{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
  };

  nixConfig = {
    extra-trusted-public-keys =
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, devenv, ... }@inputs:
    let pkgs = nixpkgs.legacyPackages."aarch64-darwin";
    in {
      devShell.aarch64-darwin = devenv.lib.mkShell {
        inherit inputs pkgs;
        modules = [ (import ./devenv.nix { inherit pkgs; }) ];
      };
    };
}
