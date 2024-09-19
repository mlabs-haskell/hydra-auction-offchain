{
  description = "A nix flake for hydra-auction-offchain.";

  nixConfig = {
    extra-experimental-features = [ "flakes" "nix-command" ];
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]hydra-auction-offchain@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    nixpkgs.follows = "ctl/nixpkgs";
    cardano-node.url = "github:input-output-hk/cardano-node/9.2.0";
    ctl.url = "github:Plutonomicon/cardano-transaction-lib/566e7153c88fdab7005a3b4b02e6fec41c7d5c94";
    ctl.inputs.cardano-node.follows = "cardano-node";
    hydra.url = "github:input-output-hk/hydra/0.19.0";
    hydra-auction-onchain.url = "github:mlabs-haskell/hydra-auction-onchain/dshuiski/delegate-info";
  };

  outputs = { self, nixpkgs, ctl, hydra, hydra-auction-onchain, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          ctl.overlays.purescript
          ctl.overlays.runtime
          ctl.overlays.spago
        ];
      };

      psProjectFor = system: pkgs:
        pkgs.purescriptProject rec {
          inherit pkgs;
          projectName = "hydra-auction-offchain";
          src = builtins.path {
            path = ./.;
            name = "${projectName}-src";
            filter = path: ftype: !(pkgs.lib.hasSuffix ".md" path) && (builtins.baseNameOf path != "flake.nix");
          };
          packageJson = ./package.json;
          packageLock = ./package-lock.json;
          shell = {
            withRuntime = true;
            packageLockOnly = true;
            shellHook = ''
              mkdir -p scripts
              cp -rf ${hydra-auction-onchain}/compiled/*.plutus scripts
              for script in scripts/*.plutus; do
                jq '. | {cborHex: .cborHex, description: .description, type: "PlutusScriptV2"}' "$script" \
                  > "$script.tmp"
                mv -f "$script.tmp" "$script"
              done
            '';
            packages = with pkgs; [
              fd
              hydra.packages.${system}.hydra-node
              nixpkgs-fmt
              nodePackages.prettier
              nodePackages.purs-tidy
            ];
          };
        };
    in
    {
      packages = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          project = psProjectFor system pkgs;
          hydra-auction-offchain = project.buildPursProject {
            strictComp = true;
            censorCodes = [
              "ImplicitImport"
              "ImplicitQualifiedImport"
              "ImplicitQualifiedImportReExport"
              "UserDefinedWarning"
            ];
          };

        in
        {
          delegate-server = pkgs.writeShellApplication {
            name = "delegate-server";
            text = ''
              TEMPD=$(mktemp -d)
              cd "$TEMPD"
              cp -r ${hydra-auction-offchain}/* .
              ln -sfn ${project.nodeModules}/lib/node_modules node_modules
              mkdir -p scripts && cp -r ${hydra-auction-onchain}/compiled/*.plutus scripts
              node --enable-source-maps -e 'import("./output/DelegateServer.Main/index.js").then(m => m.main())' \
                -- delegate-server "$@"
            '';
          };
        }
      );

      devShells = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          default = (psProjectFor system pkgs).devShell;
        }
      );

      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          project = psProjectFor system pkgs;
          builtProject = project.buildPursProject {
            strictComp = true;
            censorCodes = [
              "ImplicitImport"
              "ImplicitQualifiedImport"
              "ImplicitQualifiedImportReExport"
              "UserDefinedWarning"
            ];
          };
        in
        {
          hydra-auction-offchain-tests = project.runLocalTestnetTest {
            inherit builtProject;
            buildInputs = [ hydra.packages.${system}.hydra-node ];
            name = "hydra-auction-offchain-tests";
            testMain = "Test.Main";
          };

          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = with pkgs; [
                fd
                easy-ps.purs-tidy
                nixpkgs-fmt
                nodePackages.prettier
              ];
            }
            ''
              cd ${self}
              purs-tidy check $(fd -epurs)
              nixpkgs-fmt --check $(fd -enix --exclude='spago*')
              prettier -c $(fd -ejs)
              touch $out
            '';
        }
      );
    };
}
