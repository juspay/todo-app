# todo-app

Refer to [todo-app.cabal](todo-app.cabal) for description.

## Prerequisite

- Postgres server (Tested with v12 & 14)
- Postgrest (https://postgrest.org/en/stable/install.html) (Tested with v10)
- Run `./db.sh` to create `tutorial.conf` and load the db dump from `db.sql`.
- `cabal-install` to build the project. (Tested with 

## Getting Started

- Run `cabal install --overwrite-policy="always"` that symlinks `todo-app` executable to your `$HOME/.cabal/bin`
- Run `todo-app view` to see the current list of pending tasks.
- Run `todo-app viewAll` to see the list of completed and pending tasks.
- Run `todo-app add "do something"` to add an item to your list.
- Run `todo-app done 1` to mark the first item as completed.
- Run `todo-app delete 1` to delete the first item from the list.
- Run `todo-app reset` to remove all the items from the list.

