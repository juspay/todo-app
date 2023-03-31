# Nixify your haskell project

<!-- This is the first part of the series that will help you improve the dev workflow of your haskell project in every part.  -->
This is the first part of a series of blog posts, with the end-goal of having a better dev workflow for your haskell project. We are aiming for a "works out-of-the-box" experience. [Nix](https://nixos.org) will be used during this process. [Read here](https://haskell.flake.page/nix-rapid) for a rapid introduction to Nix. Throughout this series we assume the reader to have an understanding of Nix and [Nix flakes](https://nixos.wiki/wiki/Flakes).\

The series is designed in a way that each blog post will work on the previous blog post to improve the development experience. First blog post will introduce you to the pain points of using a haskell project without Nix, then we will bring in Nix flakes. The Haskell project of choice here is, [todo-app](https://github.com/juspay/todo-app/tree/tutorial/part-0). This app was chosen for two main reasons: 1) Keep things simple 2) Demonstrate how to manage runtime dependencies like postgres and postgREST without having to set them up manually.
<!-- This is a multi-part series, with the following objectives: -->
<!-- - Step-by-step demonstration to use nix in a haskell project -->

## Why Nixify?

Let's answer this by trying to setup the pre-requisites for todo-app. The steps are as follows:
- Install Postgres and start the server
- Run the script `db.sh` to load the DB dump
- Install PostgREST and start the web server
- Install `cabal-install`, `ghc`
- Build the project using `cabal`\
\
The steps mentioned above are brief, refer to the [README](https://github.com/juspay/todo-app/blob/tutorial/part-0/README.md) for details.\
\
These are some of the problems with the steps mentioned above:
- Postgres and PostgREST are installed globally: This will create problems when you want to work on multiple different versions of these dependencies.
- If the project doesn't specify the versions of the dependencies used, then you might end up in what's called a "Dependency hell".
- Developer time spent setting up all of those dependencies, when all they want is to quickly make changes and test them.
\
How is Nix going to solve the above issues?
- Postgres server will run using the data directory in the current project directory.
- Both Postgres and PostgREST are only present in the Nix shell and since they are installed in Read-only filesystem `/nix`, having multiple different versions will not cause any issues as there is no chance of any rewrites by mistake.
- The `flake.lock` file will contain the locked content hash, revision and source of "inputs" which will ensure the same source is used on further builds.\
\
All these benefits are listed as per the Nixification that will be happening in this blog post. More benefits will be depmonstrated in upcoming blog posts.
## Introduce Flake

Let's start by creating a `flake.nix` file in the root directory. The first step after this will be to write the basic template for the flake, which consists of `inputs` and `outputs`. 
```nix
{
  inputs = {

  };
  outputs = {self, ...}: {

  };
}

```

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = {self, nixpkgs}: {
    
  };
}

```

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = inputs@{self, ...}: {
    

  };
}

```
Syntactic sugar ^

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = {self, nixpkgs}: 
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgs = forAllSystems (system: nixpkgs.legacyPackages.${system});
    in
  {};
}

```

```nix
{
  # Inside outputs' `let` block
  overlay = final: prev: {
    todo-app = self.callCabal2nix "todo-app" ./. { };
  };
}

```
Talk about final, prev and self, super ^ (although a preference, I would like to say why final, prev helps me understand this better)

```nix
{
  # Inside outputs' `let` block
  haskellPackages' = forAllSystems (system: pkgs.${system}.haskellPackages.extend overlay);
}
```

```nix
{
  ...
  outputs = {self, nixpkgs}:
    let
      ...
    in
  {
    packages = forAllSystems ( system: {
      inherit (haskellPackages'.${system}) todo-app;
      default = haskellPackages'.${system}.todo-app;
    });
  };
}

```
Talk about how `inherit` above is same as `todo-app=haskellPackages'.${system}.todo-app` and how that can be also built using `nix build .#todo-app`


## DevShell support

```nix
{
  # Inside outputs
  devShells = forAllSystems ( system: {
    default = ...
    shell1 = ...
    shell2 = ...
  });
}

```
```nix
{
  # Inside outputs
  devShells = forAllSystems ( system: {
    default = haskellPackages'.${system}.shellFor {
      packages = p : [
        p.todo-app
      ];
      buildInputs = with haskellPackages'.${system}; [
        cabal-install
        todo-app
      ];
    };
  });
}

```
Talk about why shellFor ^

## Flakify postgres and postgREST

```nix
{
  # Inside outputs
  apps = forAllSystems ( system: {
    app1 = ...
    app2 = ...
    default = ...
  });
}

```
```nix
{
  # Inside outputs
  apps = forAllSystems ( system: {
    hello = {
      type = "app";
      program = toString pkgs.${system}.hello;
    }
    default = self.apps.hello;
  });
}

```
Talk about `type` and the output of `nix run .#hello`
```nix
{
  # Inside outputs
  apps = forAllSystems ( system: {
    postgres =
      let
        pgsql = pkgs.${system}.postgresql;
      in
      {
        type = "app";
        program = toString ( 
          pkgs.${system}.writeShellScript "pg" ''
            
            # Initialize a database with data stored in current project dir
            ${pgsql}/bin/initdb --no-locale -D ./data/db
            
            # Start your postgres server
            ${pgsql}/bin/pg_ctl -D ./data/db -l ./data/logfile -o "--unix_socket_directories='$PWD/data'" start
            
            # Create a database of your current user
            ${pgsql}/bin/createdb -h $PWD/data $(whoami)
            
            ${pkgs.${system}.coreutils}/bin/mkdir data 
            
            # Create configuration file for postgrest
            ${pkgs.${system}.coreutils}/bin/echo "db-uri = \"postgres://authenticator:mysecretpassword@localhost:5432/$(whoami)\"
            db-schemas = \"api\"
            db-anon-role = \"todo_user\"" > data/db.conf
            
            # Load DB dump
            ${pgsql}/bin/psql -h $PWD/data < db.sql
          ''
          );
      };
  });
}
```
```nix
{
  # Inside outputs
  apps = forAllSystems ( system: {
    postgrest = {
      type = "app";
      program = toString (
        pkgs.${system}.writeShellScript "pgREST" ''
          # Run postgrest using the configuration
          ${nixpkgs.lib.getExe haskellPackages'.${system}.postgrest} -- ./data/db.conf
        ''
      );
    };
  });
}
```
```nix
{
  # Inside outputs
  apps = forAllSystems ( system: {
    postgres_stop = {
      type = "app";
      program = toString (
        pkgs.${system}.writeShellScript "pgStop" ''
          # Stop postgres server
          ${pkgs.${system}.postgresql}/bin/pg_ctl -D ./data/db stop
        ''
      );
    };
  });
}
```

## Before vs After

- Installing postgres, setting up server and loading db dump replaced with -> `nix run .#postgres`
- Installing postgREST webserver, running it replaced with -> `nix run .#postgrest`
- Installing cabal-install, building the project replaced with -> `nix develop`, `nix build` and `nix run`
