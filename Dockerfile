FROM debian:stable-slim as build

# Install dependencies *You don't need all of them
RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y git jq bc make automake \
    && apt-get install -y libnuma-dev \
    && apt-get install -y rsync htop curl build-essential \
    && apt-get install -y pkg-config libffi-dev libgmp-dev \
    && apt-get install -y libssl-dev libtinfo-dev libsystemd-dev liblzma-dev \
    && apt-get install -y zlib1g-dev make g++ wget libncursesw5 libtool autoconf \
    && apt-get clean

# install libsodium
WORKDIR /build/libsodium
RUN git clone https://github.com/input-output-hk/libsodium
RUN cd libsodium && \
    git checkout 66f017f1 && \
    ./autogen.sh && ./configure && make && make install
ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"

# Install ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
RUN bash -c "curl -sSL https://get.haskellstack.org/ | sh"

# Add ghcup to PATH
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin

# Install cabal
RUN bash -c "ghcup upgrade"
RUN bash -c "ghcup install cabal 3.4.0.0"
RUN bash -c "ghcup set cabal 3.4.0.0"

# Install GHC
RUN bash -c "ghcup install ghc 8.10.7"
RUN bash -c "ghcup set ghc 8.10.7"

# Update Path to include Cabal and GHC exports
RUN bash -c "echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc"
RUN bash -c "echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc"
RUN bash -c "source $HOME/.bashrc"

# Update cabal
RUN bash -c "cabal update"

WORKDIR /app
COPY . .

RUN cabal install --install-method=copy --installdir=/app
FROM debian:stable-slim as runner

# Install dependencies *You don't need all of them
RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y git jq bc make automake \
    && apt-get install -y rsync htop curl build-essential \
    && apt-get install -y pkg-config libffi-dev libgmp-dev \
    && apt-get install -y libssl-dev libtinfo-dev libsystemd-dev liblzma-dev \
    && apt-get install -y zlib1g-dev make g++ wget libncursesw5 libtool autoconf \
    && apt-get clean

# install libsodium
WORKDIR /build/libsodium
RUN git clone https://github.com/input-output-hk/libsodium
RUN cd libsodium && \
    git checkout 66f017f1 && \
    ./autogen.sh && ./configure && make && make install
ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"

COPY --from=build /app/compile-validators /usr/local/bin/
COPY --from=build /app/mint-borrower-nft /usr/local/bin/
COPY --from=build /app/mint-lender-nft /usr/local/bin/
COPY --from=build /app/mint-time-nft /usr/local/bin/
COPY --from=build /app/generate-example-jsons /usr/local/bin/
COPY --from=build /app/mint-oracle-nft /usr/local/bin/
WORKDIR /app

#CMD ["/usr/local/bin/compile-validators"]
#CMD ["/usr/local/bin/mint-borrower-nft", "ff"0"]
#CMD ["/usr/local/bin/mint-lender-nft", "ff"0"]
#CMD ["/usr/local/bin/mint-time-nft"]
