# List all the just commands
default:
    @just --list

# Run external services (postgres and postgrest)
services:
    nix run .#todo-app-services

alias s := services

# Watch and recompile on changes
watch:
    # Executes the main subcommand with the `view` argument after recompiling
    ghciwatch --test-ghci ":main view"

alias w := watch
