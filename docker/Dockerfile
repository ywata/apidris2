FROM nixos/nix

RUN adduser -D builder
RUN addgroup builder wheel


RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

RUN nix-env -iA nixpkgs.ghc
RUN nix-env -iA nixpkgs.cabal-install
#RUN nix-env -iA nixpkgs.idris2
RUN nix-env -iA nixpkgs.git
RUN nix-env -iA nixpkgs.gnumake
RUN nix-env -iA nixpkgs.clang
RUN nix-env -iA nixpkgs.chez
RUN nix-env -iA nixpkgs.bash
RUN nix-env -iA nixpkgs.less
RUN nix-env -iA nixpkgs.emacs
RUN nix-env -iA nixpkgs.wget
ENV HOME="/home/builder"
RUN mkdir -p "${HOME}/.config/nixpkgs"
ADD config.nix "${HOME}/.config/nixpkgs"
#RUN nix-env -iA nixpkgs.apidris2pkgs

USER builder
WORKDIR "${HOME}"
RUN cabal update


# ar
ENV PATH="${PATH}:/nix/store/cp1sa3xxvl71cypiinw2c62i5s33chlr-binutils-2.35.1/bin/"

Run git clone https://github.com/idris-lang/Idris2.git
RUN cd Idris2 && git checkout -b v0.3.0
#RUN cat Idris2/idris2api.ipkg |grep -v version > Idris2/idris2api.ipkg


RUN cd Idris2 && make distclean
RUN cd Idris2 && make bootstrap SCHEME=scheme
RUN cd Idris2 && make install
ENV IDRIS2_PREFIX="${HOME}/.idris2"
ENV PATH="${IDRIS2_PREFIX}/bin:${PATH}"
RUN cd Idris2 && make install-api

RUN git clone https://github.com/ywata/apidris2.git
ADD .bashrc "${HOME}"

RUN cd apidris2 && make conv
RUN cd apidris2 && cabal v2-configure
RUN cd apidris2 && cabal v2-build
RUN cd apidris2 && make


#unset IDRIS2_PREFIX
#set PATH
#make bootstrap SCHEME=scheme
