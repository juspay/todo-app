{ pkgs, lib, config, ... }:
{
  options = {
    scripts = lib.mkOption {
      description = ''
        A set of scripts to define as flake apps.
      '';
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          packages = lib.mkOption {
            type = lib.types.listOf lib.types.package;
            default = [ ];
            description = ''
              Dependencies of the script.
            '';
          };
          text = lib.mkOption {
            type = lib.types.str;
            description = ''
              The bash script to execute.
            '';
          };
        };
      });
    };
  };
  config = {
    apps =
      lib.mapAttrs
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
  };
}
