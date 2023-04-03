# Nixify your haskell project

This blog post series is meant to make developing your Haskell project easier and more convenient right from the beginning. We'll be using a tool called [Nix](https://nixos.org) to achieve this, but if you're new to Nix, we have [resources](https://haskell.flake.page/nix-rapid) to help you get started quickly or [here](https://zero-to-nix.com) if you have time to spare. Please note that some basic knowledge of Nix and [Nix flakes](https://nixos.wiki/wiki/Flakes) is assumed for this series. We'll start by discussing the challenges of working on a Haskell project without Nix and introduce Nix flakes as the solution. Throughout the series, we'll be using ["todo-app"](https://github.com/juspay/todo-app/tree/903c769d4bda0a8028fe3775415e9bdf29d80555), a simple Haskell project, to demonstrate how to build the project and manage runtime dependencies like [postgres](https://www.postgresql.org) and [postgREST](https://postgrest.org/en/stable) automatically without manual setup, and to highlight the benefits of Nix.

## Why Nixify?

To set up the pre-requisites for todo-app, we need to follow a series of steps including installing Postgres, running the `db.sh` script, installing PostgREST, `cabal-install`, and `ghc`, and then building the project using cabal. However, this process can be time-consuming and not very developer-friendly, and can lead to dependency conflicts and versioning issues.

Nix provides a solution to these problems by allowing developers to simply run `nix develop` to set up the environment with all the required dependencies, making it easy to reproduce the same configuration across different platforms like Linux, Darwin, and WSL. This eliminates the need for manual installation of dependencies and helps avoid dependency hell.

## Introduce Flake

To begin, we'll create a file named `flake.nix` in the main directory. The first thing we'll do is create a basic template for the flake, which includes defining the inputs and outputs.
```nix
{
  inputs = {

  };
  outputs = {self, ...}: {

  };
}

```
Next, we include `nixpkgs` as one of our inputs to access the packages we need. Specifically, we're using `nixpkgs-unstable`, which is named as such because it receives frequent updates, but this doesn't mean that it's unsafe.
```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = {self, nixpkgs}: {

  };
}

```
To refer to the `nixpkgs` input, there are two ways:

- You can directly add it as a parameter to the outputs attribute set, like this: `outputs = { nixpkgs }: ...`
- Alternatively, you can bind all the input parameters to a variable, usually called `inputs`, and then refer to `nixpkgs` using `inputs.nixpkgs` as shown below.

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = inputs@{self, ...}: {

  };
}

```
We can define some local variables in a "Let" block, which we will later refer to in the attributes of the `outputs`.

Firstly, we define `supportedSystems`, which specifies all the platforms that this flake can run on. Then we use `forAllSystems` to filter out attributes with key names matching the ones in `supportedSystems`. Lastly, we define `pkgs` as a filtered-out set with attributes of `legacyPackages`. It's worth noting that the term "legacyPackages" doesn't imply that the packages are outdated; rather, it's named this way to avoid evaluating all the 80000+ packages during certain commands, such as "nix flake show".
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
Inside the "Let" block, we can define a function called `overlay` that returns an attribute set containing our Haskell project. We can use the `callCabal2nix` function from nixpkgs to determine the value of this attribute. This function internally uses ["cabal2nix"](https://github.com/NixOS/cabal2nix), a Haskell tool that generates Nix build instructions from a cabal file.

When you use Nix to build your Haskell project, Nix will use these build instructions to build your project and its dependencies in a sandboxed environment, ensuring that all the dependencies are available and that there are no conflicts between different versions of libraries.

```nix
{
  # Inside outputs' `let` block
  overlay = final: prev: {
    todo-app = final.callCabal2nix "todo-app" ./. { };
  };
}

```
When defining the input parameters of the overlay function, they can be given any name. However, conventionally they are given one of two names: `self` and `super`, or `final` and `prev`. The former names refer to the attribute set after applying the overrides and the later attribute set before applying the overrides, respectively.

I personally prefer to use final: prev as it seems more intuitive. You can read more about this convention and how it works in the [Nix manual](https://nixos.wiki/wiki/Overlays).

Continuing from the previous block of code, we will now define a new local variable called `haskellPackages'` in the "Let" block. This variable serves two purposes:

- Firstly, it adds the `todo-app` attribute from overlay to the `haskellPackages` attribute of each of the systems defined in `supportedSystems`.
- Secondly, it assigns the overridden `haskellPackages` to a new attribute called `haskellPackages'`, which is filtered out using `forAllSystems`.

```nix
{
  # Inside outputs' `let` block
  haskellPackages' = forAllSystems (system: pkgs.${system}.haskellPackages.extend overlay);
}
```
We have finished defining all the local variables in the "Let" block. We can now add `todo-app` to the packages attribute of outputs. We will also set `todo-app` as the `default` so that we can run `nix build` instead of `nix build .#todo-app`. We use `forAllSystems` to add both `todo-app` and the `default` package to all the supported systems. Additionally, `inherit (haskellPackages'.${system}) todo-app` is the same as writing `todo-app = haskellPackages'.${system}.todo-app`.

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

## DevShell support

Up to this point, we have seen how to build our package, but now it's time to enter a development shell where we can quickly test our changes. To start this shell, we use the command `nix develop`. In the same way as with `packages`, we can have multiple attributes in `devShells` that can be activated by running `nix develop .#<attribute-name>`. For example, if we have a `shell1` attribute defined in our flake, we can activate it by running `nix develop .#shell1`.

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

We will be using `shellFor` provided by `haskellPackages'` to set up the default shell for our project. There are two main reasons for choosing haskellPackages' `shellFor` over `mkShell`: 1) it is simpler to use, and 2) it is more suitable for Haskell development shell. We only need to define two attributes, `packages` and `buildInputs`. `packages` is used to provide the Nix build instructions that we defined earlier for todo-app, so that the shell doesn't start building todo-app again. `buildInputs` is used to ensure that the specified packages are present in the `PATH` of the isolated development environment.

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

## Flakify postgres and postgREST

In the same way as devShells, we can use the default attribute in apps to run our apps. To do this, we use the `nix run` command. If we want to run a specific app, we can use `nix run .#app1`.
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

Let's take an example of using the [GNU hello](https://www.gnu.org/software/hello) package from nixpkgs. In this case, we define an attribute named `hello` with two required attributes - `type` and `program`. The `type` attribute determines how the program should be executed, and it can be set to "shell" for a shell script, "python" for a Python script, and "app" for an executable. On the other hand, the `program` attribute is a string that represents the path in the Nix store where the executable is located. When we run the command `nix run .#hello`, the executable specified by the `program` attribute is executed.

```nix
{
  # Inside outputs
  apps = forAllSystems ( system: {
    hello = {
      type = "app";
      program = "${nxpkgs.lib.getExe pkgs.${system}.hello}";
    }
  });
}

```
We will now define an app for the PostgreSQL server which can be started using the command `nix run .#postgres`. To create this app, we will use `writeShellApplication` which generates a shell script that runs in an isolated environment with packages from `runtimeInputs`. One of the advantages of using `writeShellApplication` is that it includes ["shellcheck"](https://github.com/koalaman/shellcheck) which checks the shell script for errors.
`writeShellApplication` writes a text file to a path in the Nix store. To refer to the actual executable, we need to append `"/bin/pg"` to the end of the path. Additionally, `with pkgs.${system}` is used to avoid writing `[ pkgs.${system}.postgresql pkgs.${system}.coreutils ]`. 
```nix
{
  # Inside outputs
  apps = forAllSystems ( system: {
    postgres =
    {
      type = "app";
      program = 
        toString 
          (pkgs.${system}.writeShellApplication {
            name = "pg";
            runtimeInputs = with pkgs.${system}; 
              [ 
                postgresql
                coreutils
              ];
            text = 
            ''
              # Initialize a database with data stored in current project dir if data dir doesn't exist
              [ ! -d "./data/db" ] && initdb --no-locale -D ./data/db

              # Start your postgres server if not already
              if ! pg_ctl -D ./data/db status; then
                pg_ctl -D ./data/db -l ./data/logfile -o "--unix_socket_directories='$PWD/data'" start
              fi
              # Create a database of your current user if not already
              if ! psql -h "$PWD"/data -lqt | cut -d \| -f 1 | grep -qw "$(whoami)"; then
                createdb -h "$PWD"/data "$(whoami)"
              fi
              
              # Create directory data if not already 
              [ ! -d "data" ] && mkdir data 

              # Create configuration file for postgrest
              echo "db-uri = \"postgres://authenticator:mysecretpassword@localhost:5432/$(whoami)\"
              db-schemas = \"api\"
              db-anon-role = \"todo_user\"" > data/db.conf

              # Load DB dump
              psql -h "$PWD"/data < db.sql
            '';
          }) + "/bin/pg";
    };
  });
}
```
The following can be used to create an app to run the postgrest webserver using the database configuration created during `nix run .#postgres`:
```nix
{
  # Inside outputs
  apps = forAllSystems ( system: {
    postgrest = {
      type = "app";
      program = 
        toString 
          (pkgs.${system}.writeShellApplication {
            name = "pgREST";
            runtimeInputs = [ haskellPackages'.${system}.postgrest ];
            text = 
            ''
              if [ -f "./data/db.conf" ]; then  
                postgrest ./data/db.conf
              else
                echo "execute 'nix run .#postgres' to setup DB and create the postgrest configuration"
              fi
            '';
          }) + "/bin/pgREST";
    };
  });
}
```
To stop the postgres server post development, use the following app:
```nix
{
  # Inside outputs
  apps = forAllSystems ( system: {
    postgres_stop = {
      type = "app";
      program = 
        toString 
          (pkgs.${system}.writeShellApplication {
            name = "pgStop";
            runtimeInputs = with pkgs.${system}; 
              [ 
                postgresql
              ];
            text = 
            ''
              [ -d "./data/db" ] && if pg_ctl -D ./data/db status; then
                pg_ctl -D ./data/db stop
              fi
            '';
          }) + "/bin/pgStop";
    };
  });
}
```

## Before vs After

- Installing postgres, starting server, loading db dump and creating database configuration for postgREST is replaced with -> `nix run .#postgres`
- Installing and running postgREST webserver is replaced with -> `nix run .#postgrest`
- Installing cabal-install, building the project is replaced with -> `nix develop`(for development shell), `nix build` (to build the executable) and `nix run` (to run the todo-app)
