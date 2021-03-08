#!/bin/bash
#docker create -it --storage-opt size=10G  build-idris2 /bin/bash
docker build --no-cache --rm -t build-idris2 .
