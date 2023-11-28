{
  description = "A nix flake for hydra-auction-offchain.";

  nixConfig = {
    extra-experimental-features = [ "flakes" "nix-command" ];
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]hydra-auction-offchain@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix";
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib/5a560eb9ac1bef94d718b90f5ac2a541515127dd";
    nixpkgs-ctl.follows = "cardano-transaction-lib/nixpkgs";
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } ({ self, ... }: {
    imports = [ inputs.liqwid-nix.flakeModule ];
    systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
    perSystem = { lib, pkgs, ... }: {
      offchain.default = {
        src = builtins.path {
          path = self.outPath;
          name = "hydra-auction-offchain-filtered-src";
          filter = path: ftype: !(lib.hasSuffix ".md" path) && (builtins.baseNameOf path != "flake.nix");
        };

        packageLock = ./package-lock.json;
        packageJson = ./package.json;

        ignoredWarningCodes = [
          "ImplicitImport"
          "ImplicitQualifiedImport"
          "UserDefinedWarning"
        ];

        shell = {
          shellHook = ''
            ln -sfn $NODE_PATH node_modules
          '';
        };

        enableFormatCheck = true;
        enableJsLintCheck = false;

        # plutip.testMain = "Test.Contract.Spec";
      };

      pre-commit.settings.hooks.nixpkgs-fmt = {
        enable = true;
        excludes = [ "spago-packages.nix" ];
      };
    };
  });
}
