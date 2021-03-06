FROM nixos/nix

RUN adduser -D builder
RUN addgroup builder wheel


ENV HOME="/home/builder"

WORKDIR "${HOME}"

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

RUN nix-env -iA nixpkgs.ghc
RUN nix-env -iA nixpkgs.cabal-install
RUN nix-env -iA nixpkgs.idris2
RUN nix-env -iA nixpkgs.git
RUN nix-env -iA nixpkgs.gnumake
RUN nix-env -iA nixpkgs.clang
RUN nix-env -iA nixpkgs.chez
RUN nix-env -iA nixpkgs.bash
RUN nix-env -iA nixpkgs.less
RUN nix-env -iA nixpkgs.emacs
RUN nix-env -iA nixpkgs.wget


USER builder

ENV PATH="${PATH}:/nix/store/cp1sa3xxvl71cypiinw2c62i5s33chlr-binutils-2.35.1/bin/"

RUN git clone https://github.com/idris-lang/Idris2.git
RUN cd Idris2 && git checkout -b v0.3.0
RUN cat Idris2/idris2api.ipkg |grep -v version > Idris2/idris2api.ipkg
RUN cd Idris2 && make all
RUN cd Idris2 && make install

ENV IDRIS2="${HOME}/.idris2"
ENV PATH="${IDRIS2}/bin:${PATH}"
ENV IDRIS_PREFIX="${IDRIS2}"

RUN cabal update
RUN ls -la "${HOME}/" > "${HOME}/ls"
RUN cd Idris2 && make clean install-api


RUN git clone https://github.com/ywata/apidris2.git
RUN cd apidris2 && cabal v2-configure && cabal v2-build

