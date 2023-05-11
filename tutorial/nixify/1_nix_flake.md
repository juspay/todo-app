# Nixify your haskell project

This blog post series is meant to simplify Haskell development workflow for you. We'll start by discussing the challenges of working on a Haskell project without Nix and introduce [Nix flakes](https://nixos.wiki/wiki/Flakes) as the solution. If you're new to Nix, we have [resources](https://haskell.flake.page/nix-rapid) to help you get started quickly or [here](https://zero-to-nix.com) if you have time to spare. Please note that some basic knowledge of Nix expression language is assumed for this series. Throughout the series, we'll be using [todo-app](https://github.com/juspay/todo-app/tree/903c769d4bda0a8028fe3775415e9bdf29d80555), a simple Haskell project, to demonstrate how to build the project and manage runtime dependencies like [postgres](https://www.postgresql.org) and [postgREST](https://postgrest.org/en/stable) automatically without manual setup, and to highlight the benefits of Nix.

## Why Nixify?

Why use Nix to develop a Haskell project rather than something like Stack or GHCup?


- **No more assumptions**: `Prerequisite: Postgres server` doesn't answer the questions, which version and how? likewise with other prerequisites mentioned in the project like postgREST, cabal-install and GHC. Nix locks these versions in flake.lock, hence you will use the same version as the developer.
- **Productivity**: More time spent on writing Haskell as Nix gives a fully working development environment with `nix develop`.
- **Multi-platform**: Same configuration generally works on macOS,[^nm] Linux and WSL.

[^nm]: Although macOS doesn't have first-class support in nixpkgs, [it is getting there](https://github.com/NixOS/nixpkgs/issues/116341).

In the rest of the blog post, we will show step-by-step how to Nixify the todo-app project.

## Introduce Flake

To begin, clone the [todo-app](https://github.com/juspay/todo-app/tree/903c769d4bda0a8028fe3775415e9bdf29d80555) repository and checkout the specified commit.

```sh
git clone https://github.com/juspay/todo-app.git
cd todo-app
git checkout 903c769d4bda0a8028fe3775415e9bdf29d80555
```

Then create a file named `flake.nix` in the project root directory. The first thing we'll do is create a basic template for the flake, which includes:
- Define `inputs` and `outputs`
- Specify the `system` corresponding to your machine.

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
Let's break down each part of this `flake.nix`:

### Inputs [^1]

[^1]: There are two ways to access the attributes of `inputs` within `outputs`:
      - Adding the attribute as a parameter to `outputs`, like `outputs = { self, <attribute> }`. This allows you to use the `<attribute>` without requiring any prefix. 
      - Bind a variable to all the parameters of `outputs`, like `outputs = inputs@{self, ...}`. This enables you to access any attribute from `inputs` in this fashion: `inputs.<attribute>`.
[^2]: `nixpkgs-unstable` branch is named as such because of the frequent updates it receives and doesn't imply that it is unsafe.
[^3]: `self` refers to the final state of attributes in the `outputs`. For example, `self.packages.${system}.default` refers to the attribute after assigning `pkgs.hello` to it.

Nix Flake uses two different ways to specify the location of an input flake, attribute set representation and URL-like syntax. In this post we will be using the latter for the sake of simplicity, to read more about these representations [refer here](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html#url-like-syntax).

For this example we will use [GNU hello](https://www.gnu.org/software/hello) package from `nixpkgs-unstable`[^2] branch of [`nixpkgs`](https://github.com/NixOS/nixpkgs) repository. 

### Outputs

- A function that accepts a required parameter `self`[^3] and optionally attributes from `inputs` as parameters (we are using the `nixpkgs` input here). [Refer here](https://nixos.wiki/wiki/Flakes#Output_schema) for a detailed schema of `outputs`. 
- In the `let` block we define two values -- `system` ("aarch64-darwin" here, assuming we are on an ARM mac) and `pkgs` (referring to nixpkgs packages for `system`). 
  - In our example, as `system` is hardcoded for one system, [forAllSystems](https://zero-to-nix.com/concepts/flakes#system-specificity) can be used to define packages for an array of systems.

#### Packages

- Attributes in `packages.${system}` points to [derivation](https://nixos.org/manual/nix/stable/language/derivations.html) that can be used to build the package.
- `nix build` will build the derivation of the package with the name `default`. Run `nix build .#<packageName>` to build the package named "<packageName>".

#### Apps

- `apps.${system}.<appName>` refers to a flake app that can be run using `nix run`. It is attrset containing two keys `type` and `program`. The `type` attribute determines how the program should be executed, For example, "shell" for a shell script, "python" for a Python script, or "app" for an executable. On the other hand, the `program` attribute is a string that represents the path in the Nix store where the executable is located.
- `nix run` just takes the `executable` and based on `type` executes the `program` given by `apps.${system}.default`. Run `nix run .#<appName>` to run a specific app.

#### DevShells

- This puts you in a development shell with only the packages you need.
- You can define how your shell will look using `pkgs.mkShell`.
- `pkgs.mkShell` creates a derivation that is evaluated by the command `nix develop`
- Derivation given by `devShells.${system}.default` is evaluated by default, you can also define custom `devShell` like `devShells.${system}.mydevShell` and run it using `nix develop .#mydevShell`

Now you can run the following commands and see this `flake.nix` in action:
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

`pkgs.haskellPackages` is an attribute set consisting of all haskell packages maintained in nixpkgs. Since our local package (`todo-app`) is not already in it, we will have to add it manually. Technically you can just use  `packages.${system}.default = pkgs.${system}.haskellPackages.callCabal2nix "todo-app" ./. { };` but adding it to `haskellPackages` keeps it all in one place and any consuming flake will not have to think about where to find this package. 

### Overlay

[Overlays](https://nixos.wiki/wiki/Overlays) are used to override an existing package set, such as `pkgs.haskellPackages`, and produce a new package set containing the changes we want. These changes could be either about overriding a single package in the package set (the second argument `super` references the original package set), or it could be about adding new packages to it. 

### callCabal2nix

The `callCabal2nix` function produces a Haskell package derivation given its source. This function internally uses ["cabal2nix"](https://github.com/NixOS/cabal2nix), a Haskell tool that generates Nix build instructions from a cabal file.

### Time to run!
After replacing the previous `flake.nix` with the current one, run the following:
- `nix build` builds the haskell project and symlinks your executable in `./result/bin`
- `nix run` builds (if not already) and runs the `todo-app` executable

## Nixify DevShell

Our current flake enables us to *build* the `todo-app`. However, what if want to develop it, by adding a feature or fixing a bug? For Haskell development, we normally use [cabal](https://cabal.readthedocs.io/) and tools like [ghcid](https://github.com/ndmitchell/ghcid). These tools require a GHC loaded with packages mentioned in `build-depends` of your cabal file, which is what we want a`devShell` to provide (ie., an isolated environment with custom GHC).
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

In the above flake, we are using [`shellFor`](https://nixos.wiki/wiki/Haskell#Using_shellFor_.28multiple_packages.29) (a function defined in `haskellPackages` attrset) to set up the default shell for our project. `shellFor` is an abstraction over [`mkShell`](https://ryantm.github.io/nixpkgs/builders/special/mkshell/) geared specifically for Haskell development shells. Generally, we only need to define two keys `packages` and `nativeBuildInputs`. `packages` marks which of the packages in the package set are *local* packages (to be compiled by cabal). `nativeBuildInputs` is used to ensure that the specified packages are present in the `PATH` of the isolated development environment.

### Let's run!

- `nix develop` this will start a development shell
- `cabal repl` loads all project modules
- `ghcid -c "cabal repl exe:todo-app"` reloads your repl session everytime there is a code change

## Nixify external dependencies

So far we nixified the Haskell part of our project. But a project can also have non-Haskell dependencies, like Postgres, MySQL and redis. In this section we will specifically look at how you can start a postgres server using Nix without relying or mutating global state (outside of project directory).
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
        let
          script = pkgs.writeShellApplication {
            name = "pg_start";
            runtimeInputs = [ pkgs.postgresql ];
            text = 
            ''
              # Initialize a database with data stored in current project dir
              [ ! -d "./data/db" ] && initdb --no-locale -D ./data/db

              postgres -D ./data/db -k "$PWD"/data
            '';
          };
        in "${script}/bin/pg_start";
    };
  };
}
```
### writeShellApplication

- A function that generates a derivation of the shell script provided as `text`. 
- `runtimeInputs`: packages to be made available to the shell application's PATH.
- `writeShellApplication` uses [shellcheck](https://github.com/koalaman/shellcheck) to statically analyze your bash script for issues.
- `toString` function applied to `writeShellApplication` gives the path in the `nix/store` where this is located.

### Run it!

- `nix run .#postgres` will start postgres server in your current shell.


## Conclude
| Before                                                                                                  | After                                                                                                            |
|---------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------|
| Installing postgres, starting server, loading db dump and creating database configuration for postgREST | `nix run .#postgres`                                                                                             |
| Installing and running postgREST webserver                                                              | `nix run .#postgrest`                                                                                            |
| Installing cabal-install, building the project is now                                                   | `nix develop` (for development shell), `nix build` (to build the executable) and `nix run` (to run the todo-app) |
