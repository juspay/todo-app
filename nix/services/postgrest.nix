# Custom service (not provided by `services-flake`) configuration for `postgrest`.
# See https://community.flake.parts/services-flake/custom-service
{ pkgs, lib, config, ... }:
let
  inherit (lib) types;
in
{
  options = {
    services.postgrest = {
      enable = lib.mkEnableOption "postgrest";
      unixSocket = lib.mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "The path to the socket to bind to";
        example = "/tmp/pgrst.sock";
      };
      config = lib.mkOption {
        type = types.attrsOf types.str;
        # TODO: use https://github.com/srid/flake-root for `db-uri` and `server-unix-socket`
        default = {
          # Note: socket path in connection uri cannot contain `/`, so we need to URL-encode it.
          # see https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING-KEYWORD-VALUE
          # TODO: re-use `config.services.postgres."pg".dataDir`
          db-uri = "postgres://\${PWD//\//%2F}%2Fdata%2Fpg/todo";
          db-schemas = "api";
          db-anon-role = "todo_user";
        };
      };
    };
  };
  config = lib.mkIf config.services.postgrest.enable {
    settings.processes.postgrest = {
      environment = {
        PGRST_DB_SCHEMAS = config.services.postgrest.config.db-schemas;
        PGRST_DB_ANON_ROLE = config.services.postgrest.config.db-anon-role;
      } // lib.optionalAttrs (config.services.postgrest.unixSocket != null) {
        PGRST_SERVER_UNIX_SOCKET = config.services.postgrest.unixSocket;
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
        # `http://localhost` is to tell curl to use the HTTP protocol and `localhost` is just a dummy hostname
        exec.command = if config.services.postgrest.unixSocket != null then
          "${lib.getExe pkgs.curl} --unix-socket ${config.services.postgrest.unixSocket} http://localhost"
        else
          # TODO: configurable hostname and port
          "${lib.getExe pkgs.curl} http://localhost:3000";
      };
    };
  };
}
