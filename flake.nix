{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      # The platforms which can use the flake
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      # Filter out supportedSystems from all the system attributes 
      # available in nixpkgs
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      # Select the legacyPackages attribute for the supportedSystems
      # Note: legacyPackages is just a placeholder for all the packages
      #       in the package manager because naming it as packages will
      #       force the evaluation of all of them (80000+) during `nix flake show`
      pkgs = forAllSystems (system: nixpkgs.legacyPackages.${system});
      # Overrides for the haskellPackages. It will replace the current
      # version of the packages in haskellPackages with the one mentioned
      # below.
      overlay = self: super: {
        # `nix flake show` and `nix flake check` won't work because of
        # IFD (https://nixos.wiki/wiki/Import_From_Derivation).
        # Might be possible to use in the near future, check this out:
        # https://github.com/cdepillabout/cabal2nixWithoutIFD
        todo-app = self.callCabal2nix "todo-app" ./. { };
      };
      # Extend the `pkgs.haskellPackages` attrset using an overlay.
      haskellPackages' = forAllSystems (system: pkgs.${system}.haskellPackages.extend overlay);
    in
    {
      # This is what generates the todo-app executable 
      packages = forAllSystems ( system: {
        inherit (haskellPackages'.${system}) todo-app;
        default = haskellPackages'.${system}.todo-app;
      });
      # Define what your shell using `nix develop` should comprise of 
      devShells = forAllSystems ( system: {
        default = haskellPackages'.${system}.shellFor {
          packages = p : [
            # If this is not specified, `cabal build` in devShell will not
            # be able to utilise the derivation built using callCabal2nix.
            # In such a case `cabal build` will try to build the pacakge
            # from scratch, including downloading dependencies. It will 
            # eventually fail because it can't find `zlib`.
            p.todo-app
          ];
          # These packages will be installed and their `/bin` path is 
          # added to PATH env of the devShell
          buildInputs = with haskellPackages'.${system}; [
            cabal-install
          ];
        };
      });
    };
}
