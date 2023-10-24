{ pkgs, lib, config, ... }:
{
  options = {
    scripts = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          packages = lib.mkOption {
            type = lib.types.listOf lib.types.package;
            default = [ ];
            description = "The packages to include as runtime dependencies.";
          };
          text = lib.mkOption {
            type = lib.types.str;
            default = "";
            description = "The script to run.";
          };
        };
      });
    };
  };
  config = {
    apps =
      let
        scripts' = lib.mapAttrs
          (name: cfg: {
            type = "app";
            program =
              let
                script = pkgs.writeShellApplication {
                  name = name;
                  text = cfg.text;
                  runtimeInputs = cfg.packages;
                };
              in
              "${script}/bin/${name}";
          })
          config.scripts;
      in
      scripts';
  };
}
