# syntax=docker/dockerfile:1

FROM node:18
WORKDIR /app

COPY package.json package-lock.json .
RUN npm clean-install --production=false --loglevel=verbose
RUN npm install purescript@0.15.8
RUN npm install spago@0.21.0

COPY packages.dhall spago.dhall .
RUN npx --no-install spago install

COPY scripts scripts
COPY api api
COPY app/delegate-server app/delegate-server
COPY src src

RUN npx --no-install spago build

COPY bundle.js .
RUN node ./bundle.js

COPY tsconfig.json .
RUN npx --no-install tsc --emitDeclarationOnly
