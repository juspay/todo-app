# Check `todo-app` integration with external services.
{ inputs, ... }:
{
  perSystem = { self', pkgs, ... }: {
    process-compose.todo-app-integ-test = { config, ... }: {
      imports = [
        inputs.services-flake.processComposeModules.default
        ./services
      ];
      # `test` is a reserved name in `process-compose-flake`.
      # This process is disabled by default and is only enabled when `nix flake check` is run,
      # which is only done in CI.
      settings.processes.test = {
        environment = {
          # If not set, `todo-app` will try to connect to `postgrest` using TCP.
          PGRST_SERVER_UNIX_SOCKET = config.services.postgrest.config.server-unix-socket;
        };
        command = pkgs.writeShellApplication {
          name = "todo-app-integration-test";
          runtimeInputs = [ self'.packages.default ];
          # TODO: Capture output and check for expected strings.
          text = ''
            # Function to print a separator
            print_separator() {
              echo "----------------------------------------"
              echo "  $1"
              echo "----------------------------------------"
            }
            print_separator "Test viewing all todos"
            todo-app viewAll

            print_separator "Test viewing pending todos"
            todo-app view

            print_separator "Test adding a todo"
            todo-app add "Buy milk"

            print_separator "Test completing a todo"
            # Workaround for https://www.shellcheck.net/wiki/SC1010
            todo-app "done" 1

            print_separator "Test deleting a todo"
            todo-app delete 1

            print_separator "Test resetting the todo list"
            todo-app reset
          '';
        };
        depends_on."postgrest".condition = "process_healthy";
      };
    };
  };
}