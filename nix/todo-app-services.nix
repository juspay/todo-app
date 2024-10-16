# `process-compose` configuration for `todo-app` external services (namely `postgresql` and `postgrest`).
{ inputs, ... }:
{
  perSystem =  {
    # Both postgres and postgrest bind to unix socket -- useful for local development and in CI.
    process-compose.todo-app-services = {
      imports = [
        inputs.services-flake.processComposeModules.default
        ./services
      ];
      services.postgrest.unixSocket = "./data/pgrst.sock";
    };

    # Only postgres binds to unix socket, while postgrest binds to TCP port.
    process-compose.todo-app-services-tcp = {
      imports = [
        inputs.services-flake.processComposeModules.default
        ./services
      ];
    };
  };
}