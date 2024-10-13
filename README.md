# todo-app

A simple TODO app in Haskell that provides a persistent way to manage the TODO list, by making HTTP requests to Postgrest (postgrest is a web server that converts your PostgreSQL database into a RESTful API).


**This app serves as a hello-world to ultimately demonstrate how a Haskell project can benefit from the use of Nix (to be precise, [haskell-flake] and [flake-parts]).**

[haskell-flake]: https://community.flake.parts/haskell-flake
[flake-parts]: https://flake.parts

## Tutorial

Learn with [our blog post series](https://nixos.asia/en/nixify-haskell):

- [Introduction](https://nixos.asia/en/nixify-haskell-nixpkgs)
- [Switch to flake-parts](https://nixos.asia/en/nixify-haskell-parts)
- [Simplify Nix using haskell-flake](https://nixos.asia/en/nixify-haskell-flake)
- ...

## Prerequisite

- [Install Nix](https://nixos.asia/en/install)
- Run application services
  - Run `nix run .#postgres` to start a postgres server with data dir in `./data/db`.
    - Run (once) `nix run .#createdb` to create DB user, load the dump and create the DB configuration for PostgREST.
  - Run `nix run .#postgrest` to start PostgREST web server.

## Getting Started

- Run `nix build` that will symlink the executable at `./result/bin/todo-app`.
- Run `./result/bin/todo-app --help` to see the list of actions you can perform.
