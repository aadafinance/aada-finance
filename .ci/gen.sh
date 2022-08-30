#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: $0 <branch>"
    exit 1
fi

BRANCH=$1
ARF_PATH="/gz/aada-app"
WRITE_PATH="${ARF_PATH}/version_details.json"

echo "Getting the version ..."
VERSION=$(docker-compose exec -T aada-finance "compile-validators --version" | \
    cut -d '"' -f 2)
echo "Version: $VERSION"
NODE_ARF_PATH="${ARF_PATH}/${VERSION}"

# cd to docker-compose directory
echo "Go to the docker-compose directory ..."

echo "Create the version path ..."
docker-compose exec -T aada-finance "mkdir -p $VERSION || true"
export CARDANO_NODE_SOCKET_PATH=/gz/cardano/cardano-my-node/db/socket

echo "Getting stake hash ..."
HASH=$(cardano-cli stake-address \
    key-hash \
    --stake-verification-key-file $ARF_PATH/stake.vkey)

echo "Running needed on chain actions ..."
docker-compose exec -T aada-finance compile-validators \
    --hash $HASH\
    -k \
    -l ${VERSION}/liquidation.plutus \
    -i ${VERSION}/interest.plutus \
    -c ${VERSION}/collateral.plutus \
    -r ${VERSION}/request.plutus
docker-compose exec -T aada-finance mint-aada-nft \
    minting-policy -l \
    -p ${VERSION}/lender.nft
docker-compose exec -T aada-finance mint-aada-nft \
    minting-policy \
    -b \
    -p ${VERSION}/borrower.nft

if [[ "${BRANCH}" == "master" ]]; then
  MAGIC="--mainnet"
else
  MAGIC="--testnet-magic 1"
fi

LIQUIDATION_ADDRESS=$(cardano-cli address \
    build \
    --payment-script-file $NODE_ARF_PATH/liquidation.plutus  \
    --stake-verification-key-file ${ARF_PATH}/stake.vkey $MAGIC)
REQUEST_ADDRESS=$(cardano-cli address \
    build \
    --payment-script-file $NODE_ARF_PATH/request.plutus  \
    --stake-verification-key-file ${ARF_PATH}/stake.vkey $MAGIC)
COLLATERAL_ADDRESS=$(cardano-cli address \
    build \
    --payment-script-file $NODE_ARF_PATH/collateral.plutus  \
    --stake-verification-key-file ${ARF_PATH}/stake.vkey $MAGIC)
INTEREST_ADDRESS=$(cardano-cli address \
    build \
    --payment-script-file $NODE_ARF_PATH/interest.plutus  \
    --stake-verification-key-file ${ARF_PATH}/stake.vkey $MAGIC)
LENDER_POLICY=$(cardano-cli transaction \
    policyid \
    --script-file $NODE_ARF_PATH/lender.nft)
BORROWER_POLICY=$(cardano-cli transaction \
    policyid \
    --script-file $NODE_ARF_PATH/borrower.nft)

echo "Writing the version details to ${WRITE_PATH} ..."
cat > ${WRITE_PATH} <<EOF
{
  "${VERSION}": {
    "STAKE_HASH":"${HASH}",
    "COLLATERAL_ADDRESS": "${COLLATERAL_ADDRESS}",
    "REQUEST_ADDRESS": "${REQUEST_ADDRESS}",
    "INTEREST_ADDRESS": "${INTEREST_ADDRESS}",
    "LIQUIDATION_ADDRESS": "${LIQUIDATION_ADDRESS}",
    "LENDER_POLICY": "${LENDER_POLICY}",
    "BORROWER_POLICY": "${BORROWER_POLICY}"
  }
}
EOF
