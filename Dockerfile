FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y tzdata
RUN apt-get install -y haskell-stack
RUN apt-get install -y coinor-libipopt1v5
RUN apt-get install -y coinor-libipopt-dev
RUN apt-get install -y netbase
RUN apt-get install -y zlib1g-dev

RUN stack upgrade
ENV PATH="/root/.local/bin:${PATH}"

COPY . /home/HashedExpression

# FIXME stack build fails first time, but succeeds second? (needs to download keys??)
RUN cd /home/HashedExpression && $(stack build || stack build) && stack install
