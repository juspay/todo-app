{ config, ... }:
{
  imports = [
    ./postgrest.nix
  ];
  services = {
    postgrest.enable = true;
    # Start postgres using Unix socket and disable listening on TCP port.
    postgres."pg" = {
      enable = true;
      # Listening on TCP port is disabled by setting `listen_addresses` to empty string.
      listen_addresses = "";
      socketDir = config.services.postgres."pg".dataDir;
      initialDatabases = [
        {
          name = "todo";
          schemas = [ ../../db.sql ];
        }
      ];
    };
  };

  settings.processes.postgrest.depends_on."pg".condition = "process_healthy";
}