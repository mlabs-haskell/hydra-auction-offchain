{
  description = "A nix flake for hydra-auction-offchain.";

  nixConfig = {
    extra-experimental-features = [ "flakes" "nix-command" ];
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]hydra-auction-offchain@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    liqwid-nix.url = "github:mlabs-haskell/liqwid-nix/aciceri/fix-new-ctl";
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib/v9.2.0";
    nixpkgs-ctl.follows = "cardano-transaction-lib/nixpkgs";
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    hydra-auction-onchain.url = "github:mlabs-haskell/hydra-auction-onchain/dshuiski/delegate-info";
    hydra.url = "github:input-output-hk/hydra/4fed4625f321d89b483c82f55252d24da63191c7";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } ({ self, ... }: {
    imports = [ inputs.liqwid-nix.flakeModule ];
    systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
    perSystem = { system, lib, pkgs, ... }: {
      offchain.default = {
        src = builtins.path {
          path = self.outPath;
          name = "hydra-auction-offchain-filtered-src";
          filter = path: ftype: !(lib.hasSuffix ".md" path) && (builtins.baseNameOf path != "flake.nix");
        };

        nodejsPackage = pkgs.nodejs-18_x;
        packageLock = ./package-lock.json;
        packageJson = ./package.json;

        ignoredWarningCodes = [
          "ImplicitImport"
          "ImplicitQualifiedImport"
          "ImplicitQualifiedImportReExport"
          "UserDefinedWarning"
        ];

        shell = {
          shellHook = ''
            mkdir -p scripts
            cp -rf ${inputs.hydra-auction-onchain}/compiled/*.plutus scripts
          '';
          extraCommandLineTools = [
            inputs.hydra.packages.${system}.hydra-node
          ];
        };

        enableFormatCheck = true;
        plutip = {
          testMain = "Test.Main";
          buildInputs = [
            inputs.hydra.packages.${system}.hydra-node
          ];
        };
      };

      pre-commit.settings.hooks.nixpkgs-fmt = {
        enable = true;
        excludes = [ "spago-packages.nix" ];
      };
    };
  });
}
