#!/bin/bash

address=$(cat ../lender/payment.addr)
MAGIC="--testnet-magic 1097911063"

utxo=$(cardano-cli-balance-fixer collateral --address ${address} ${MAGIC})
let deadline=600
echo "utxo: $utxo"
echo "deadline: $deadline"
aada-core 4 lenders.policy $utxo $deadline

cardano-cli transaction policyid --script-file lenders.policy > lendersPolicyID
