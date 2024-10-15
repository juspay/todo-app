# `process-compose` configuration for `todo-app` external services (namely `postgresql` and `postgrest`).
{ inputs, ... }:
{
  perSystem =  {
    process-compose.todo-app-services = {
      imports = [
        inputs.services-flake.processComposeModules.default
        ./services
      ];
    };
  };
}