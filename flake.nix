{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    # `process-compose` from `nixpkgs` is not compatible with latest `process-compose-flake`
    # FIXME: Use `process-compose` from nixpkgs after https://github.com/juspay/todo-app/issues/12
    nixpkgs-latest.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
  };

  outputs = inputs:
    # mkFlake takes two inputs, `args` and `mod`. Internally it calls `lib.evalModules`(https://nixos.org/manual/nixpkgs/unstable/#module-system-lib-evalModules)
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      imports = [
        # This is where we import the haskell-flake module. See ./nix/todo-app.nix for how we use it.
        inputs.haskell-flake.flakeModule
        # See ./nix/{todo-app-services,integration-test}.nix for how we use it.
        inputs.process-compose-flake.flakeModule
        ./nix/todo-app-services.nix
        ./nix/integration-test.nix
        # This is where we override `nixpkgs` to use `process-compose` from `nixpkgs-latest`.
        ./nix/nixpkgs.nix
      ];
      perSystem = { self', pkgs, ... }: {
        imports = [
          ./nix/todo-app.nix
          ./nix/devshell.nix
        ];
      };
    };
}
