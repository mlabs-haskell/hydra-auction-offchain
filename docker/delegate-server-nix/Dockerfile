# syntax=docker/dockerfile:1

FROM nixos/nix:latest AS hydra

RUN git clone https://github.com/input-output-hk/hydra.git
WORKDIR /hydra
RUN git checkout 1ffe7c6b505e3f38b5546ae5e5b97de26bc70425
RUN nix build .#hydra-node-static \
  --extra-experimental-features "nix-command flakes" \
  --accept-flake-config
 
FROM node:18
WORKDIR /app

COPY package.json package-lock.json .
RUN npm clean-install --production=false --loglevel=verbose
RUN npm install purescript@0.15.8
RUN npm install spago@0.21.0

COPY packages.dhall spago.dhall .
RUN npx --no-install spago install

COPY scripts scripts
COPY src src
COPY app/delegate-server app/delegate-server
COPY protocol-parameters.json .
RUN npx --no-install spago build

COPY --from=hydra /hydra/result/bin/hydra-node /usr/local/bin/hydra-node
RUN chmod +x /usr/local/bin/hydra-node

ENTRYPOINT ["npx", "--no-install", "spago", "-q", "run", "--main", "DelegateServer.Main", "--exec-args"]
CMD ["--help"]
