# List all the just commands
default:
    @just --list

# Run external services (postgres and postgrest)
services:
    nix run .#todo-app-services

alias s := services

# Watch and recompile on changes
watch:
    ghciwatch

alias w := watch
