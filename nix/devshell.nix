{ pkgs, config, ... }:
{
  devShells.default = pkgs.mkShell {
    inputsFrom = [
      config.haskellProjects.default.outputs.devShell
    ];
    PGRST_SERVER_UNIX_SOCKET = config.services.postgrest.config.server-unix-socket;
  };
}