{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      # The platforms which can use the flake
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      # A function that takes an attrset and returns the values
      # for corresponding keys in supportedSystems (in other words, filter)
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      # Select the legacyPackages attribute for the supportedSystems
      # Note: legacyPackages is just a placeholder for all the packages
      #       in the package manager because naming it as packages will
      #       force the evaluation of all of them (80000+) during `nix flake show`
      pkgs = forAllSystems (system: nixpkgs.legacyPackages.${system});
      # Define a function that takes two arguments and return a attrset
      # of overrides to nixpkgs. Read more about Overlays here: https://nixos.wiki/wiki/Overlays
      overlay = final: prev: {
        # `nix flake show` and `nix flake check` won't work because of
        # IFD (https://nixos.wiki/wiki/Import_From_Derivation).
        # Might be possible to use in the near future, check this out:
        # https://github.com/cdepillabout/cabal2nixWithoutIFD
        todo-app = final.callCabal2nix "todo-app" ./. { };
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
            todo-app
          ];
        };
      });
    # Define apps that is triggered by `nix run` command. For example,
    # `nix run .#postgres` will run the script for postgres below
    apps = forAllSystems ( system: {
      postgres =
      let
        pgsql = pkgs.${system}.postgresql;
      in
      {
        # `type` and `program` are required attributes.
        #  TODO: include explanation of `type`
        # `program` denotes the path of the executable to run
        type = "app";
        program = toString ( 
          pkgs.${system}.writeShellScript "pg" ''
            
            # Initialize a database with data stored in current project dir
            ${pgsql}/bin/initdb --no-locale -D ./data/db

            # Start your postgres server
            ${pgsql}/bin/pg_ctl -D ./data/db -l ./data/logfile -o "--unix_socket_directories='$PWD/data'" start

            # Create a database of your current user
            ${pgsql}/bin/createdb -h $PWD/data $(whoami)
            
            ${pkgs.${system}.coreutils}/bin/mkdir data 

            # Create configuration file for postgrest
            ${pkgs.${system}.coreutils}/bin/echo "db-uri = \"postgres://authenticator:mysecretpassword@localhost:5432/$(whoami)\"
            db-schemas = \"api\"
            db-anon-role = \"todo_user\"" > data/db.conf

            # Load DB dump
            ${pgsql}/bin/psql -h $PWD/data < db.sql
          ''
          );
      };
      postgres_stop = {
        type = "app";
        program = toString (
          pkgs.${system}.writeShellScript "pgStop" ''
            # Stop postgres server
            ${pkgs.${system}.postgresql}/bin/pg_ctl -D ./data/db stop
          ''
        );
      };
      
      postgrest = {
        type = "app";
        program = toString (
          pkgs.${system}.writeShellScript "pgREST" ''
            # Run postgrest using the configuration
            ${nixpkgs.lib.getExe haskellPackages'.${system}.postgrest} -- ./data/db.conf
          ''
        );
      };
    });
    };
}
