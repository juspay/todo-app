# todo-app

A simple TODO app in Haskell using [PostgREST](https://postgrest.org/) as a RESTful API for [PostgreSQL](https://www.postgresql.org/).

> [!NOTE]
> This app is a demo to showcase how developer experience in a Haskell project can be improved using Nix. We primarily use [haskell-flake], [services-flake] and [flake-parts] to achieve this.

[haskell-flake]: https://community.flake.parts/haskell-flake
[services-flake]: https://community.flake.parts/services-flake
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

> [!NOTE]
> `just services` starts postgres and postgrest, both of which bind to Unix socket.

```sh
# Start postgres and postgrest
just services
# Watch for changes in the Haskell code
just watch
```

## Run through Nix

> [!NOTE]
> `todo-app-services-tcp` starts postgres, which listens on Unix socket, while postgrest listens on TCP port 3000.

```sh
nix run github:juspay/todo-app#todo-app-services-tcp
nix run github:juspay/todo-app -- view
```
