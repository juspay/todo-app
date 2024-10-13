{ inputs, ... }:
{
  perSystem = { inputs', system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (_: _: {
          inherit (inputs'.nixpkgs-latest.legacyPackages) process-compose;
        })
      ];
    };
  };
}