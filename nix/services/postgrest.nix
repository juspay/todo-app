{ pkgs, lib, config, ... }:
{
  options = {
    services.postgrest = {
      enable = lib.mkEnableOption "postgrest";
      config = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        # TODO: use https://github.com/srid/flake-root for `db-uri` and `server-unix-socket`
        default = {
          # Note: socket path in connection uri cannot contain `/`, so we need to URL-encode it.
          # see https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING-KEYWORD-VALUE
          db-uri = "postgres://\${PWD//\//%2F}%2Fdata/$(whoami)";
          db-schemas = "api";
          db-anon-role = "todo_user";
          server-unix-socket = "./data/pgrst.sock";
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
                PGRST_SERVER_UNIX_SOCKET="${config.services.postgrest.config.server-unix-socket}";
                # Have to export explicitly: https://www.shellcheck.net/wiki/SC2155
                export PGRST_DB_URI;
                export PGRST_DB_SCHEMAS;
                export PGRST_DB_ANON_ROLE;
                export PGRST_SERVER_UNIX_SOCKET;
                postgrest
              '';
          };
        in
        "${script}/bin/pg_rest";
    };
  };
}
