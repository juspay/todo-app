{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs:
    # mkFlake takes two inputs, `args` and `mod`. Internally it calls `lib.evalModules`(https://nixos.org/manual/nixpkgs/unstable/#module-system-lib-evalModules)
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      perSystem = { self', pkgs, ... }: {
        imports = [
          ./nix/services/postgres.nix
          ./nix/services/postgrest.nix
          ./nix/scripts.nix
          ./nix/todo-app.nix
        ];
      };
    };
}
