{ self', pkgs, ... }:
let
  overlay = final: prev: {
    # `nix flake show` and `nix flake check` won't work because of
    # IFD (https://nixos.wiki/wiki/Import_From_Derivation).
    # Might be possible to use in the near future, check this out:
    # https://github.com/cdepillabout/cabal2nixWithoutIFD
    todo-app = final.callCabal2nix "todo-app" ../. { };
  };
  # Extend the `pkgs.haskellPackages` attrset using an overlay.
  myHaskellPackages = pkgs.haskellPackages.extend overlay;
in
{
  # This is what generates the todo-app executable
  packages = {
    todo-app = myHaskellPackages.todo-app;
    default = myHaskellPackages.todo-app;
  };

  # Define what your shell using `nix develop` should comprise of
  devShells.default = myHaskellPackages.shellFor {
    packages = p: [
      # If this is not specified, `cabal build` in devShell will not
      # be able to utilise the derivation built using callCabal2nix.
      # In such a case `cabal build` will try to build the pacakge
      # from scratch, including downloading dependencies. It will
      # eventually fail because it can't find `zlib`.
      p.todo-app
    ];
    # These packages will be installed and their `/bin` path is
    # added to PATH env of the devShell
    buildInputs = with myHaskellPackages; [
      cabal-install
      ghcid
      haskell-language-server
    ];
  };

  services = {
    postgres.enable = true;
    postgrest.enable = true;
  };
  scripts."createdb" = {
    packages = [ pkgs.postgresql ];
    text =
      ''
        # Create a database of your current user
        if ! psql -h "$PWD"/data -lqt | cut -d \| -f 1 | grep -qw "$(whoami)"; then
          createdb -h "$PWD"/data "$(whoami)"
        fi

        # Load DB dump
        # TODO: check if schema already exists
        psql -h "$PWD"/data < db.sql
      '';
  };
}
