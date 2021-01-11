# Stage 0: get Idris 2 source and build it using Racket
FROM ubuntu:latest AS build

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get install -y chezscheme git make gcc libc-dev emacs
RUN apt-get install -y sudo

RUN useradd -ms /bin/bash -U builder
USER builder
ENV HOME="/home/builder"

WORKDIR "${HOME}"

RUN git clone https://github.com/idris-lang/Idris2.git && \
     cd ./Idris2

RUN cd ./Idris2 && make bootstrap SCHEME=scheme

ENV IDRIS2="${HOME}/.idris2"
ENV PATH="${IDRIS2}/bin:${PATH}"
ENV IDRIS_PREFIX="${IDRIS2}"

RUN cd ./Idris2 && make install
#RUN cd ./Idris2 && make install-api

# # # Stage 1: copy Idris 2 executable and libraries to the minimal Ubuntu image (with Racket)
FROM ubuntu:latest
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get install -y chezscheme git make gcc libc-dev emacs

RUN useradd -ms /bin/bash -U builder
USER builder
ENV HOME="/home/builder"
WORKDIR "${HOME}"

ENV IDRIS2="${HOME}/.idris2"
ENV PATH="${IDRIS2}/bin:${PATH}"
ENV IDRIS_PREFIX="${IDRIS2}"

RUN git clone https://github.com/idris-lang/Idris2.git 

COPY --from=build "${IDRIS2}" "${IDRIS2}" 

ENV LD_LIBRARY_PATH="${IDRIS2}/lib:${LD_LIBRARY_PATH}"

RUN cd Idris2 && make all install install-api


