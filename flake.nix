{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      flake-utils,
      nixpkgs,
      self,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        config = { };

        overlays = [
          # This is an overlay we apply on top of Nixpkgs with some of our
          # own packages defined.
          (final: prev: {
            # A Haskell package set with our own overrides and packages defined.
            myHaskellPackages = final.haskellPackages.override {
              overrides = hfinal: hprev: {
                # This is our local Haskell package.
                aoc2024 = hfinal.callCabal2nix "aoc2024" ./. { };
              };
            };

            # This is just a convenient shortcut to our package from the
            # top-level of Nixpkgs.  We're also applying the
            # justStaticExecutables function to our package in order to
            # reduce the size of the output derivation.
            aoc2024 = final.haskell.lib.compose.justStaticExecutables final.myHaskellPackages.aoc2024;

            # A Haskell development shell for our package that includes
            # things like cabal and HLS.
            myDevShell = final.myHaskellPackages.shellFor {
              packages = p: [ p.aoc2024 ];

              nativeBuildInputs = [
                final.cabal-install
                final.haskellPackages.haskell-language-server
              ];
            };
          })
        ];

        # Our full Nixpkgs with the above overlay applied.
        pkgs = import nixpkgs { inherit config overlays system; };
      in
      {
        packages.default = pkgs.aoc2024;

        devShells.default = pkgs.myDevShell;
      }
    );
}
