FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y tzdata
RUN apt-get install -y pkg-config
RUN apt-get install -y libglpk-dev
RUN apt-get install -y libhdf5-dev
RUN apt-get install -y libfftw3-dev
RUN apt-get install -y build-essential

WORKDIR /home/glpk
COPY . ./
ENV CPATH /usr/include/coin:/usr/include/hdf5/serial
ENV LIBRARY_PATH /usr/lib/x86_64-linux-gnu/hdf5/serial

RUN make

ENTRYPOINT [ "./glpk" ]
