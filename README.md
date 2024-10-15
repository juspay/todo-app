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
- [Integrate external services using services-flake](https://nixos.asia/en/nixify-services-flake)
- ...

## Getting Started

[Install Nix](https://nixos.asia/en/install)

## Development

```sh
nix develop
```
To enter development shell on `cd`, use [direnv](https://nixos.asia/en/direnv)

```sh
# Start postgres and postgrest
just services
# Make the Haskell code change and run
cabal run todo-app -- <args>
```

## Run through Nix

- Run external services `nix run github:juspay/todo-app#todo-app-services`
- View pending tasks: `nix run github:juspay/todo-app -- view` (*Note: run the command in the same directory where external services are running*)
