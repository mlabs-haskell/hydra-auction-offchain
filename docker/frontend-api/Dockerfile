# syntax=docker/dockerfile:1

FROM node:18
WORKDIR /app

COPY api api
COPY app/delegate-server app/delegate-server
COPY src src
COPY scripts scripts
COPY package.json packages.dhall spago.dhall .

RUN npx handpick --target=bundleDependencies
RUN npx spago build

COPY bundle.js .
RUN node ./bundle.js

COPY tsconfig.json .
RUN npx tsc --emitDeclarationOnly
