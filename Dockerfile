FROM alpine:3.14
ARG GHC_VERSION=9.0.2
ARG STACK_VERSION=2.7.3
RUN apk --no-cache add \
    binutils-gold \
    curl \
    docker \
    gcc \
    g++ \
    git \
    gmp-dev \
    ncurses-dev \
    ncurses-static \
    postgresql-dev \
    openrc \
    libffi-dev \
    make \
    xz \
    tar \
    perl \
    zlib-dev \
    zlib-static \
    zip \
    bash \
    sudo
RUN mkdir -p ~/.ghcup/bin && \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > ~/.ghcup/bin/ghcup && \
    chmod +x ~/.ghcup/bin/ghcup
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"
RUN ghcup install ghc ${GHC_VERSION}
RUN ghcup set ghc ${GHC_VERSION}
RUN ghcup install cabal
RUN curl -sSLo /usr/local/bin/stack https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64-bin && \
    chmod +x /usr/local/bin/stack
RUN ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.6 && \
    stack config set system-ghc --global true
