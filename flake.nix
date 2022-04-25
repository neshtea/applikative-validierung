{
  description = "A flake for the example 'Applikative Validiering' blog-post.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          # Overlay a haskellPackages that contains our `validation`
          # project.
          (final: prev: {
            haskellPackages = prev.haskellPackages.override (old: {
              overrides =
                prev.lib.composeExtensions (old.overrides or (_: _: { }))
                (hfinal: hprev: {
                  validation = hfinal.callCabal2nix "validation" ./. { };
                });
            });
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
      in {
        packages = {
          validation = pkgs.haskellPackages.validation;
          default = self.packages.${system}.validation;
        };

        apps = {
          default = {
            type = "app";
            program = "${self.packages.${system}.validation}/bin/validation";
          };
        };

        devShells = {
          default = pkgs.haskellPackages.shellFor {
            packages = p: [ p.validation ];
            nativeBuildInputs = with pkgs; [ cabal-install ghc ];
          };
        };
      });
}
