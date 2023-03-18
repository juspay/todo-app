# haskell-flake-hello

Refer to [haskell-flake-hello.cabal](haskell-flake-hello.cabal) for description.

## Prerequisite

- Install Nix<sup>[(*)](https://determinate.systems/posts/determinate-nix-installer)</sup>, enable Flakes (avoid using global installations)

TODO: Add instructions to run postgres in the local directory
TODO: Add instructions to start postgrest along with creating the schema.

## Getting Started

TODO: Write about how to use the app

## Tips

- Run `nix run nixpkgs#ormolu -- --mode inplace $(git ls-files '*.hs')` to format all the `.hs` files in your worktree.
- Run `nix run nixpkgs#haskellPackages.cabal-fmt -- --inplace haskell-flake-hello.cabal` to format the `.cabal` file.

