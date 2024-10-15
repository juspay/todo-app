{ pkgs, config, ... }:
{
  devShells.default = pkgs.mkShell {
    inputsFrom = [
      config.haskellProjects.default.outputs.devShell
    ];
    PGRST_SERVER_UNIX_SOCKET = with config.process-compose.todo-app-services.services;
      postgrest.config.server-unix-socket;
    packages = with pkgs; [
      just
      ghciwatch
    ];
    shellHook = ''
      echo
      echo "🍎🍎 Run 'just <recipe>' to get started"
      just
    '';
  };
}
