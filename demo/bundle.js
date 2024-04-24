import * as esbuild from "esbuild";

esbuild.build({
  entryPoints: ["src/index.ts"],
  outfile: "index.js",
  define: {
    "process.env.BLOCKFROST_API_KEY": `"${process.env.BLOCKFROST_API_KEY}"`
  },
  bundle: true,
  platform: "browser",
  format: "esm",
  treeShaking: true,
  logLevel: "error"
});
