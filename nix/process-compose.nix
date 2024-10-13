{ inputs, ... }:
{
  perSystem = {
    process-compose.services = {
      imports = [
        inputs.services-flake.processComposeModules.default
        ./services
      ];
    };
  };
}