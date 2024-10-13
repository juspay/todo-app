# List all the just commands
default:
    @just --list

# Run external services (postgres and postgrest)
services:
    nix run .#todo-app-services

alias s := services

# Watch and recompile on changes
watch:
    ghcid -c cabal repl

alias w := watch

# Run the application (ex: `just run add '"Organise desk"'`)
run *ARGS:
    cabal run todo-app -- {{ ARGS }}
