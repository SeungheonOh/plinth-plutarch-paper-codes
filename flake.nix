{
  description = "Plinth & Plutarch paper code examples";

  inputs = {
    haskell-nix = {
      url =
        "github:input-output-hk/haskell.nix/3c9e83afafb0f40b67b89ed98b356a4922a65760";
      inputs.hackage.follows = "hackage";
    };
    nixpkgs.follows = "haskell-nix/nixpkgs";
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    iohk-nix = {
      url =
        "github:input-output-hk/iohk-nix/a704b93ea51ee1a8a7e456659e0b28ddba280a95";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem
    (system: import ./nix/outputs.nix { inherit inputs system; });

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
