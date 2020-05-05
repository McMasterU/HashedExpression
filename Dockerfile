# FROM ubuntu:18.04

# RUN apt-get update
# RUN apt-get install -y tzdata
# RUN apt-get install -y haskell-stack
# RUN apt-get install -y coinor-libipopt1v5
# RUN apt-get install -y coinor-libipopt-dev
# RUN apt-get install -y netbase
# RUN apt-get install -y zlib1g-dev

# RUN stack upgrade
# ENV PATH="/root/.local/bin:${PATH}"

# WORKDIR /home/HashedExpresion
# COPY . ./

# # FIXME stack build fails first time, but succeeds second? (needs to download keys??)
# RUN $(stack build || stack build) && stack install
FROM haskell:8.8.3

WORKDIR /home/HashedExpression
COPY . ./

# RUN stack upgrade


# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack install
RUN mkdir /target

ENTRYPOINT cd /target && symphony *.sp
