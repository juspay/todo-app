{ self', pkgs, ... }:
{
  # `haskellProjects` is the top-level submodule provided by haskell-flake, which we import in `flake.nix`
  haskellProjects.default = {
    # The default haskell-flake configuration is sufficient for our purposes. It provides both packages and devShell automatically based on the Haskell package, as defined in our .cabal file.
    #
    # For customization, see
    # https://github.com/srid/haskell-flake
    autoWire = [ "packages" ];
  };
  packages = {
    default = self'.packages.todo-app;
  };
}
