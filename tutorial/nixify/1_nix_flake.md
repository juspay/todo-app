# Nixify your haskell project

This blog post series is meant to simplify Haskell development workflow for you. We'll start by discussing the challenges of working on a Haskell project without Nix and introduce [Nix flakes](https://nixos.wiki/wiki/Flakes) as the solution. If you're new to Nix, we have [resources](https://haskell.flake.page/nix-rapid) to help you get started quickly or [here](https://zero-to-nix.com) if you have time to spare. Please note that some basic knowledge of Nix expression language is assumed for this series. Throughout the series, we'll be using [todo-app](https://github.com/juspay/todo-app/tree/903c769d4bda0a8028fe3775415e9bdf29d80555), a simple Haskell project, to demonstrate how to build the project and manage runtime dependencies like [postgres](https://www.postgresql.org) and [postgREST](https://postgrest.org/en/stable) automatically without manual setup, and to highlight the benefits of Nix.

## Why Nixify?

- **No more assumptions**: `Prerequisite: Postgres server` doesn't answer the questions, which version and how? likewise with other prerequisites mentioned in the project like postgREST, cabal-install and GHC. Nix locks these versions in flake.lock, hence you will use the same version as the developer.
- **Productivity**: More time spent on the code change as Nix does the setup for you with `nix develop`.
- **Multi-platform**: Same configuration works on your MacOS, Linux and WSL.

## Introduce Flake

To begin, we'll clone [todo-app](https://github.com/juspay/todo-app/tree/903c769d4bda0a8028fe3775415e9bdf29d80555) and checkout to the redirected commit. Then create a file named `flake.nix` in the project root directory. The first thing we'll do is create a basic template for the flake, which includes:
- Define `inputs` and `outputs`
- Specify the `system` which the flake supports.

Tl;dr This is how your `flake.nix` will look:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default = pkgs.hello;
      apps.${system}.default = {
        type = "app";
        program = "${pkgs.hello}/bin/hello";
      };
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.hello
        ];
      };
    };
}
```
In the upcoming subsections we will break down each part of the code block above.

### Inputs [^1]

[^1]: There are two ways to access the attributes of `inputs` within `outputs`:
      - Adding the attribute as a parameter to `outputs`, like `outputs = { self, <attribute> }`. This allows you to use the `<attribute>` without requiring any prefix. 
      - Bind a variable to all the parameters of `outputs`, like `outputs = inputs@{self, ...}`. This enables you to access any attribute from `inputs` in this fashion: `inputs.<attribute>`.
[^2]: `nixpkgs-unstable` branch is named as such because of the frequent updates it receives and doesn't imply that it is unsafe.
[^3]: `self` refers to the final state of attributes in the `outputs`. For example, `self.packages.${system}.default` refers to the attribute after assigning `pkgs.hello` to it.

Nix Flake uses two different ways to specify the location of an input flake, attribute set representation and URL-like syntax. In this post we will be using the latter for the sake of simplicity, to read more about these representations [refer here](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html#url-like-syntax).

For this example we will use [GNU hello](https://www.gnu.org/software/hello) package from `nixpkgs-unstable`[^2] branch of [`nixpkgs`](https://github.com/NixOS/nixpkgs) repository. 

### Outputs

- A function that accepts a required parameter `self`[^3] and optionally attributes from `inputs` as parameters. [Refer here](https://nixos.wiki/wiki/Flakes#Output_schema) for a detailed schema of `outputs`. 
- We will include `nixpkgs` as parameter to use its attributes to define our attributes in the sections below.
- In the `let` block we also define two things, `system` ("aarch64-darwin" here) and `pkgs` consits of all the packages for `system`. In our example `system` is hardcoded for one system, [forAllSystems](https://zero-to-nix.com/concepts/flakes#system-specificity) can be used to define packages for an array of systems.

#### Packages

- Attributes in `packages.${system}` points to [derivation](https://nixos.org/manual/nix/stable/language/derivations.html) that can be used to build the package.
- `nix build` will build the derivation of the package with the name `default`. Run `nix build .#<packageName>` to build a pacakge with a specific name.

#### Apps

- `apps.${system}.<appName>` refers to an attrset with two required attributes `type` and `program`. The `type` attribute determines how the program should be executed, For example, "shell" for a shell script, "python" for a Python script, or "app" for an executable. On the other hand, the `program` attribute is a string that represents the path in the Nix store where the executable is located.
- `nix run` just takes the `executable` and based on `type` executes the `program` given by `apps.${system}.default`. Run `nix run .#<appName>` to run a specific app.

#### DevShells

- This puts you in a development shell with only the packages you need.
- You can define how your shell will look using `pkgs.mkShell`.
- `pkgs.mkShell` creates a derivation that is evaluated by the command `nix develop`
- Derivation given by `devShells.${system}.default` is evaluated by default, you can also define custom `devShell` like `devShells.${system}.mydevShell` and run it using `nix develop .#mydevShell`

It's time to now run the following commands and see the `flake.nix` in action:
- `nix build`
- `nix run`
- `nix develop`

## Nixify Haskell package

We saw how to use `pkgs.hello` from `nixpkgs` in the basic example above, now it is time to define build instructions to build a package for the haskell project `todo-app`.\
Tl;dr Here's the `flake.nix` for this section:
```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      overlay = final: prev: {
        todo-app = final.callCabal2nix "todo-app" ./. { };
      };
      myHaskellPackages = pkgs.haskellPackages.extend overlay;
    in
    {
      packages.${system}.default = myHaskellPackages.todo-app;
      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/todo-app";
      };
    };
}
```
Let's break it down!
### haskellPackages

This is an attribute set that consists of all the haskell projects maintained in Nixpkgs. Since our local project will not be part of this, we have to add it. Technically you can just use  `packages.${system}.default = pkgs.${system}.haskellPackages.callCabal2nix "todo-app" ./. { };` but adding it to `haskellPackages` keeps it all in one place and consuming flakes will not have to think about where to find this package. 

### Overlay

This is a function that will output an attribute consisting of the packages that you want to either add into `haskellPackages` or replace if they already exist. It takes two parameters, first parameter being the final state of `haskellPackages` after applying all the overrides and the second being the state of `haskellPackages` before applying the overrides.

### callCabal2nix

A function that takes the `name` of package (`todo-app`), `src` of cabal file (`./.`, i.e current directory), and overrides (`{ }`, i.e none) as parameters and returns a derivation that will build your package. This function internally uses ["cabal2nix"](https://github.com/NixOS/cabal2nix), a Haskell tool that generates Nix build instructions from a cabal file.

### Time to run!
After replacing the previous `flake.nix` with the current one, run the following:
- `nix build` builds the haskell project and symlinks your executable in `./result/bin`
- `nix run` builds (if not already) and runs the `todo-app` executable

## Nixify DevShell

We can now build the `todo-app`! What if you want to make changes in the project and have a quick feedback on the change? [`ghcid`](https://github.com/ndmitchell/ghcid) or [`cabal repl`](https://cabal.readthedocs.io/en/3.4/cabal-commands.html#cabal-v2-repl) can be used here. These tools require a GHC loaded with packages mentioned in `build-depends` of your cabal file, which is what a `devShell` will provide (an isolated environment with custom GHC).\
Tl;dr Here's the `flake.nix` for this section:
```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      overlay = final: prev: {
        todo-app = final.callCabal2nix "todo-app" ./. { };
      };
      myHaskellPackages = pkgs.haskellPackages.extend overlay;
    in
    {
      devShells.${system}.default = myHaskellPackages.shellFor {
          packages = p : [
            p.todo-app
          ];
          buildInputs = with myHaskellPackages; [
            ghcid
            cabal-install
            todo-app
          ];
        };
    };
}
```

### shellFor

We will be using `shellFor`, a function defined in `haskellPackages` to set up the default shell for our project. `shellFor` is tailor made for Haskell projects, hence we choose this over `mkShell`. We only need to define two attributes, `packages` and `buildInputs`. `packages` is used to provide the Nix build instructions that we defined earlier for todo-app, so that the shell doesn't start building todo-app again. `buildInputs` is used to ensure that the specified packages are present in the `PATH` of the isolated development environment.

### Let's run!

- `nix develop` this will start a development shell
- `cabal repl` loads all project modules
- `ghcid -c "cabal repl exe:todo-app"` reloads your repl session everytime there is a code change

## Nixify external dependencies

Most of real-world applications will rely on external dependencies like Postgres, MySQL, Redis and so on. In this section we will look at how you can start a postgres server using `nix run .#postgres` and also create the data directory local to the project.\
Tl;dr Here's the `flake.nix`:
```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs }:
  let
    system = "aarch64-darwin";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    apps.${system}.postgres = {
      type = "app";
      program = 
        toString 
          (pkgs.writeShellApplication {
            name = "pg_start";
            runtimeInputs = [ pkgs.postgresql ];
            text = 
            ''
              # Initialize a database with data stored in current project dir
              [ ! -d "./data/db" ] && initdb --no-locale -D ./data/db

              postgres -D ./data/db -k "$PWD"/data
            '';
          }) + "/bin/pg_start";
    };
  };
}
```
### writeShellApplication

- A function that generates a derivation to run the shell script in an isolated environment. 
- `runtimeInputs` are provided to the application.
- `writeShellApplication` comes with [shellcheck](https://github.com/koalaman/shellcheck) which checks the shell script for errors.
- `toString` function applied to `writeShellApplication` gives the path in the `nix/store` where this is located.

### Run it!

- `nix run .#postgres` will start postgres server in your current shell.


## Before vs After

- Installing postgres, starting server, loading db dump and creating database configuration for postgREST is now, `nix run .#postgres`
- Installing and running postgREST webserver is now, `nix run .#postgrest`
- Installing cabal-install, building the project is now, `nix develop`(for development shell), `nix build` (to build the executable) and `nix run` (to run the todo-app)
