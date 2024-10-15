# Custom service (not provided by `services-flake`) configuration for `postgrest`.
# See https://community.flake.parts/services-flake/custom-service
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
          # TODO: re-use `config.services.postgres."pg".dataDir`
          db-uri = "postgres://\${PWD//\//%2F}%2Fdata%2Fpg/todo";
          db-schemas = "api";
          db-anon-role = "todo_user";
          server-unix-socket = "./data/pgrst.sock";
        };
      };
    };
  };
  config = lib.mkIf config.services.postgrest.enable {
    settings.processes.postgrest = {
      environment = {
        PGRST_DB_SCHEMAS = config.services.postgrest.config.db-schemas;
        PGRST_DB_ANON_ROLE = config.services.postgrest.config.db-anon-role;
        PGRST_SERVER_UNIX_SOCKET = config.services.postgrest.config.server-unix-socket;
      };
      command = pkgs.writeShellApplication {
        name = "pg_rest";
        runtimeInputs = [ pkgs.haskellPackages.postgrest ];
        text =
          ''
            # Can't be set in `settings.processes.postgrest.environment` because the value has to be evaluated in the shell
            PGRST_DB_URI="${config.services.postgrest.config.db-uri}";
            # Have to export explicitly: https://www.shellcheck.net/wiki/SC2155
            export PGRST_DB_URI;
            exec postgrest
          '';
      };
      readiness_probe = {
        exec.command = "${lib.getExe pkgs.curl} --unix-socket ${config.services.postgrest.config.server-unix-socket} http://localhost";
      };
    };
  };
}
