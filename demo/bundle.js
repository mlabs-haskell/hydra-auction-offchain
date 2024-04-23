import * as esbuild from "esbuild";

esbuild.build({
  entryPoints: ["src/index.ts"],
  outfile: "index.js",
  define: {
    "process.env.BLOCKFROST_API_KEY": `"${process.env.BLOCKFROST_API_KEY}"`
  },
  plugins: [
    {
      name: "replace-hydra-auction-offchain-import",
      setup(build) {
        const skipResolve = "skipResolve";
        build.onResolve({ filter: /^hydra-auction-offchain$/ }, async (args) => {
          if (args.pluginData === skipResolve) return;
          const isNixEnv = process.env.NPM_ENV !== "1";
          if (isNixEnv) {
            args.path = args.path.replace("hydra-auction-offchain", "../../dist");
          }
          const resolveOptions = {
            importer: args.importer,
            namespace: args.namespace,
            resolveDir: args.resolveDir,
            kind: args.kind,
            pluginData: skipResolve
          };
          return build.resolve(args.path, resolveOptions);
        });
      }
    }
  ],
  bundle: true,
  platform: "browser",
  format: "esm",
  treeShaking: true,
  logLevel: "error"
});
