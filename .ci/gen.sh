#!/bin/bash

if [ -z "$2" ]; then
    echo "Usage: $0 <branch> <hash>"
    exit 1
fi


ARF_PATH="/aada/aada-app"
NODE_ARF_PATH="${ARF_PATH}/${HEAD_HASH}"
BRANCH=$1
HEAD_HASH=$2
WRITE_PATH="${ARF_PATH}/version_details.json"

docker-compose exec -T aada-finance mkdir ${HEAD_HASH}
docker-compose exec -T aada-finance compile-validators --hash $(cardano-cli stake-address key-hash --stake-verification-key-file stake.vkey) -k -l ${HEAD_HASH}/liquidation.plutus -i ${HEAD_HASH}/interest.plutus -c ${HEAD_HASH}/collateral.plutus -r ${HEAD_HASH}/request.plutus
docker-compose exec -T aada-finance mint-aada-nft minting-policy -l -p ${HEAD_HASH}/lender.nft
docker-compose exec mint-aada-nft minting-policy -b -p ${HEAD_HASH}/borrower.nft

if [[ "${BRANCH}" == "master" ]]; then
  MAGIC="--mainnet"
else
  MAGIC="--testnet-magic 1"
fi

export CARDANO_NODE_SOCKET_PATH=/gz/cardano/cardano-my-node/db/socket

LIQUIDATION_ADDRESS=$(cardano-cli address build --payment-script-file $NODE_ARF_PATH/liquidation.plutus  --stake-verification-key-file stake.vkey $MAGIC)
REQUEST_ADDRESS=$(cardano-cli address build --payment-script-file $NODE_ARF_PATH/request.plutus  --stake-verification-key-file stake.vkey $MAGIC)
COLLATERAL_ADDRESS=$(cardano-cli address build --payment-script-file $NODE_ARF_PATH/collateral.plutus  --stake-verification-key-file stake.vkey $MAGIC)
INTEREST_ADDRESS=$(cardano-cli address build --payment-script-file $NODE_ARF_PATH/interest.plutus  --stake-verification-key-file stake.vkey $MAGIC)
LENDER_POLICY=$(cardano-cli transaction policyid --script-file $NODE_ARF_PATH/lender.nft)
BORROWER_POLICY=$(cardano-cli transaction policyid --script-file $NODE_ARF_PATH/borrower.nft)

cat > ${WRITE_PATH} <<EOF
{
  "${HEAD_HASH}": {
    "STAKE_HASH":"${HEAD_HASH}",
    "COLLATERAL_ADDRESS": "${COLLATERAL_ADDRESS}",
    "REQUEST_ADDRESS": "${REQUEST_ADDRESS}",
    "INTEREST_ADDRESS": "${INTEREST_ADDRESS}",
    "LIQUIDATION_ADDRESS": "${LIQUIDATION_ADDRESS}",
    "LENDER_POLICY": "${LENDER_POLICY}",
    "BORROWER_POLICY": "${BORROWER_POLICY}"
  }
}
EOF
