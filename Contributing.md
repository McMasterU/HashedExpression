# Contributing

## Generating Haddock (with Docker)
Build the docker image located in docs (it's important you do this from the root of the repo), with
```terminal
docker build -t hashed-docker -f docs/Dockerfile .
```
-Then run the docker container to generate the haddock documentation (NOTE: every time you alter the 
-code base you'll have to rebuild the image)
```terminal
-docker run -v /some/path:/home/HashedExpression/docs hashed-docker
```
-this will generate all the haddock documentation (in html) into */some/path* on your local system

