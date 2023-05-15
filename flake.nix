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
      myHaskellPackages = forAllSystems (system: pkgs.${system}.haskellPackages.extend overlay);
    in
    {
      # This is what generates the todo-app executable 
      packages = forAllSystems (system: {
        inherit (myHaskellPackages.${system}) todo-app;
        default = myHaskellPackages.${system}.todo-app;
      });
      # Define what your shell using `nix develop` should comprise of 
      devShells = forAllSystems (system: {
        default = myHaskellPackages.${system}.shellFor {
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
          buildInputs = with myHaskellPackages.${system}; [
            cabal-install
            todo-app
          ];
        };
      });
      # Define apps that is triggered by `nix run` command. For example,
      # `nix run .#postgres` will run the script for postgres below
      apps = forAllSystems (system: {
        postgres =
          {
            # `type` and `program` are required attributes.
            # The type attribute determines how the program should be executed, For example, "shell" for a shell script,
            # "python" for a Python script, or "app" for an executable.
            # `program` denotes the path of the executable to run
            type = "app";
            program =
              let
                script = pkgs.${system}.writeShellApplication {
                  name = "pg_start";
                  runtimeInputs = [ pkgs.${system}.postgresql ];
                  text =
                    ''
                      # Initialize a database with data stored in current project dir
                      [ ! -d "./data/db" ] && initdb --no-locale -D ./data/db

                      postgres -D ./data/db -k "$PWD"/data
                    '';
                };
              in
              "${script}/bin/pg_start";
          };
        createdb = {
          type = "app";
          program =
            let
              script = pkgs.${system}.writeShellApplication {
                name = "create_db";
                runtimeInputs = [ pkgs.${system}.postgresql ];
                text =
                  ''
                    # Create a database of your current user
                    if ! psql -h "$PWD"/data -lqt | cut -d \| -f 1 | grep -qw "$(whoami)"; then
                      createdb -h "$PWD"/data "$(whoami)"
                    fi

                    # Load DB dump
                    # TODO: check if schema already exists
                    psql -h "$PWD"/data < db.sql
                
                    # Create configuration file for postgrest
                    echo "db-uri = \"postgres://authenticator:mysecretpassword@localhost:5432/$(whoami)\"
                    db-schemas = \"api\"
                    db-anon-role = \"todo_user\"" > data/db.conf
                  '';
              };
            in
            "${script}/bin/create_db";
        };

        postgrest = {
          type = "app";
          program =
            let
              script = pkgs.${system}.writeShellApplication {
                name = "pg_rest";
                runtimeInputs = [ myHaskellPackages.${system}.postgrest ];
                text =
                  ''
                    postgrest ./data/db.conf
                  '';
              };
            in
            "${script}/bin/pg_rest";
        };
      });
    };
}
