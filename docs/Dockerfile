FROM haskell:8.8.3

WORKDIR /home/HashedExpression
COPY . ./


# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack clean

ENTRYPOINT stack haddock --fast --haddock-arguments "--odir=/home/HashedExpression/docs/"
