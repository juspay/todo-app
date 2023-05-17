# todo-app

A simple TODO app in Haskell that provides a persistent way to manage the TODO list, by making HTTP requests to Postgrest (postgrest is a web server that converts your PostgreSQL database into a RESTful API).


**This app serves as a hello-world to ultimately demonstrate how a Haskell project can benefit from the use of Nix (to be precise, [haskell-flake](https://haskell.flake.page) and [flake-parts](https://flake.parts)).**

## Prerequisite

- Install Nix<sup>[(*)](https://determinate.systems/posts/determinate-nix-installer)</sup>, enable Flakes (avoid using global installations)
- Stop any global postgres server on port 5432.
- Run `nix run .#postgres` to start a postgres server in `./data/db` dir.
- Run `nix run .#createdb` to create DB user, load the dump and create the DB configuration for Postgrest.
- Run `nix run .#postgrest` to start postgREST web server.

## Getting Started

- Run `nix build` that will symlink the executable at `./result/bin/todo-app`.
- `cd result/bin`.
- Run `./todo-app view` to see the current list of pending tasks.
- Run `./todo-app viewAll` to see the list of completed and pending tasks.
- Run `./todo-app add "do something"` to add an item to your list.
- Run `./todo-app done 1` to mark the first item as completed.
- Run `./todo-app delete 1` to delete the first item from the list.
- Run `./todo-app reset` to remove all the items from the list.
