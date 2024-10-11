{ pkgs, lib, config, ... }:
{
  options = {
    services.postgrest = {
      enable = lib.mkEnableOption "postgrest";
      config = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = {
          db-uri = "postgres://authenticator:mysecretpassword@localhost:5432/$(whoami)";
          db-schemas = "api";
          db-anon-role = "todo_user";
        };
      };
    };
  };
  config = lib.mkIf config.services.postgrest.enable {
    apps.postgrest = {
      type = "app";
      program =
        let
          script = pkgs.writeShellApplication {
            name = "pg_rest";
            runtimeInputs = [ pkgs.haskellPackages.postgrest ];
            text =
              ''
                PGRST_DB_URI="${config.services.postgrest.config.db-uri}";
                PGRST_DB_SCHEMAS="${config.services.postgrest.config.db-schemas}";
                PGRST_DB_ANON_ROLE="${config.services.postgrest.config.db-anon-role}";
                # Have to export explicitly: https://www.shellcheck.net/wiki/SC2155
                export PGRST_DB_URI;
                export PGRST_DB_SCHEMAS;
                export PGRST_DB_ANON_ROLE;
                postgrest
              '';
          };
        in
        "${script}/bin/pg_rest";
    };
  };
}
