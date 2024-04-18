# syntax=docker/dockerfile:1

FROM node:18
WORKDIR /app

COPY package.json package-lock.json ./
RUN npm clean-install --loglevel=verbose
RUN npm install purescript@0.15.8
RUN npm install spago@0.21.0

COPY app/delegate-server app/delegate-server
COPY src src
COPY scripts scripts
COPY packages.dhall spago.dhall protocol-parameters.json .
RUN npx spago build

RUN curl -LO https://github.com/input-output-hk/hydra/releases/download/0.15.0/hydra-x86_64-linux-0.15.0.zip
RUN unzip -d /usr/local/bin/ hydra-x86_64-linux-0.15.0.zip
RUN chmod +x /usr/local/bin/hydra-node

ENTRYPOINT ["npx", "spago", "-q", "run", "--main", "DelegateServer.Main", "--exec-args"]
CMD ["--help"]
