{ inputs, pkgs, lib, project, utils, ghc, system }:

let
  allTools = {
    "ghc966".cabal = project.projectVariants.ghc966.tool "cabal" "latest";
    "ghc966".cabal-fmt =
      project.projectVariants.ghc966.tool "cabal-fmt" "latest";
    "ghc966".haskell-language-server =
      project.projectVariants.ghc966.tool "haskell-language-server" "latest";
    "ghc966".fourmolu = project.projectVariants.ghc966.tool "fourmolu" "latest";
    "ghc966".hlint = project.projectVariants.ghc966.tool "hlint" "latest";
  };

  tools = allTools.${ghc};

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {
    src = lib.cleanSources ../.;
    hooks = {
      nixfmt-classic = {
        enable = true;
        package = pkgs.nixfmt-classic;
      };
      cabal-fmt = {
        enable = true;
        package = tools.cabal-fmt;
      };
      fourmolu = {
        enable = true;
        package = tools.fourmolu;
      };
      hlint = {
        enable = true;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
    };
  };

  shell = project.shellFor {
    name = "plinth-plutarch-paper-code-${project.args.compiler-nix-name}";
    buildInputs = [
      tools.haskell-language-server
      tools.fourmolu
      tools.cabal
      tools.hlint
      tools.cabal-fmt
      pkgs.nixfmt-classic
      pkgs.git
      pkgs.ghcid
    ];
    withHoogle = false;
    exactDeps = false;
    shellHook = ''
      ${preCommitCheck.shellHook}
    '';
  };

in shell
