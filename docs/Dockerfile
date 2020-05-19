FROM haskell:8.8.3

WORKDIR /home/HashedExpression
COPY . ./

RUN stack build

ENTRYPOINT stack haddock --haddock-arguments "--odir=/home/HashedExpression/docs/" --allow-different-user
