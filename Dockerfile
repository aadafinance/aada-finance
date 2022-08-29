FROM nixos/nix

RUN nix-channel --update

WORKDIR /app
COPY . .

RUN  nix build .#aada-lend:exe:compile-validators  --extra-experimental-features nix-command --extra-experimental-features flakes
RUN cp /app/result/bin/compile-validators /usr/bin/
RUN  nix build .#aada-lend:exe:mint-aada-nft  --extra-experimental-features nix-command --extra-experimental-features flakes
RUN cp /app/result/bin/mint-aada-nft /usr/bin/
RUN  nix build .#aada-lend:exe:mint-oracle-nft  --extra-experimental-features nix-command --extra-experimental-features flakes
RUN cp /app/result/bin/mint-oracle-nft /usr/bin/
RUN  nix build .#aada-lend:exe:generate-example-jsons  --extra-experimental-features nix-command --extra-experimental-features flakes
RUN cp /app/result/bin/generate-example-jsons /usr/bin/