# WIP: todo-app

A simple TODO app in Haskell that provides a persistent way to manage TODO list, by making http requests to postgrest (postgrest is a webserver that converts your PostgreSQL database into a RESTful API).\
\
**This app serves as a hello-world to ulitmately demonstrate how a Haskell project can benefit with the use of Nix (to be precise, [haskell-flake](https://haskell.flake.page) and [flake-parts](https://flake.parts)).**

## Prerequisite

- Install Nix<sup>[(*)](https://determinate.systems/posts/determinate-nix-installer)</sup>, enable Flakes (avoid using global installations)
- Stop any global postgres server on port 5432.
- Run `nix run .#postgres` to start a postgres server in `./data/db` dir.
- Run `nix run .#postgrest` to start postgREST webserver.

## Getting Started

- Run `nix develop` to enter nix shell.
- Run `todo-app view` to see the current list of pending tasks.
- Run `todo-app viewAll` to see the list of completed and pending tasks.
- Run `todo-app add "do something"` to add an item to your list.
- Run `todo-app done 1` to mark the first item as completed.
- Run `todo-app delete 1` to delete the first item from the list.
- Run `todo-app reset` to remove all the items from the list.

## Tips

- Run `nix run .#postgres_stop` to stop the postgres server

## TODO

- [x] Build the project executable using flake
- [x] Add devShell support
- [x] Start postgres server using flakes
- [x] Start postgrest service using flakes

