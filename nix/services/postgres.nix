{ pkgs, lib, config, ... }:
{
  options = {
    services.postgres = {
      enable = lib.mkEnableOption "postgres";
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.postgresql;
        description = "The postgresql package to use";
      };
    };
  };
  config = lib.mkIf config.services.postgres.enable {
    apps.postgres = {
      # `type` and `program` are required attributes.
      # The type attribute determines how the program should be executed, For example, "shell" for a shell script,
      # "python" for a Python script, or "app" for an executable.
      # `program` denotes the path of the executable to run
      type = "app";
      program =
        let
          script = pkgs.writeShellApplication {
            name = "pg_start";
            runtimeInputs = [ config.services.postgres.package ];
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
  };
}
