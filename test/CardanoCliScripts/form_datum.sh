#!/bin/bash

echo "constructing datum for SC1 funds lock"

address=$(cat ../borrower/payment.addr)
MAGIC="--testnet-magic 1097911063"

utxo=$(cardano-cli-balance-fixer collateral --address ${address} ${MAGIC})

aada-core 3 borrowers.policy $utxo

cardano-cli transaction policyid --script-file borrowers.policy > borrowersPolicyID
cardano-cli transaction policyid --script-file ../always-succeed.script > mintingPolicy
mintingpolicy=$(cat mintingPolicy)
policyid=$(cat borrowersPolicyID)
#let deadline=$(date +%s)+600 # add 10 minutes
borrowerspkh="5c0fc15429970b2ebf23f32be8b960e02b79c26b0db86af76e5b4efc" # Where should loan be paid to?
loantn=$(echo -n "CONYMONY" | xxd -b -ps -c 80 | tr -d '\n')
loancs=$mintingpolicy
loanamnt=150
interesttn=$(echo -n "MONY" | xxd -b -ps -c 80 | tr -d '\n')
interestcn=$mintingpolicy
interestamnt=50
datum_name="borrower"
collateralcs=$mintingpolicy
repayint=0
liquidatenft=???

printf "{\n\t\"constructor\":0,\n\t\"fields\":\n\t[\n\t\t{\n\t\t\t\"bytes\":\"${policyid}\"\n\t\t},\n\t\t{\n\t\t\t\"bytes\":\"${borrowerspkh}\"\n\t\t},\n\t\t{\n\t\t\t\"bytes\":\"${loantn}\"\n\t\t},\n\t\t{\n\t\t\t\"bytes\":\"${loancs}\"\n\t\t},\n\t\t{\n\t\t\t\"int\":${loanamnt}\n\t\t},\n\t\t{\n\t\t\t\"bytes\":\"${interesttn}\"\n\t\t},\n\t\t{\n\t\t\t\"bytes\":\"${interestcn}\"\n\t\t},\n\t\t{\n\t\t\t\"int\":${interestamnt}\n\t\t},\n\t\t{\n\t\t\t\"bytes\":\"${collateralcs}\"\n\t\t},\n\t\t{\n\t\t\t\"int\":${repayint}\n\t\t},\n\t\t{\n\t\t\t\"bytes\":\"${liquidatenft}\"\n\t\t}\n\t]\n}\n" > ${datum_name}.datum
