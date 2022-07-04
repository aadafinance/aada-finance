#!/bin/bash

MAGIC="--testnet-magic 1097911063"

sc3address=$(cat ../sc3.addr)
lenderaddress=$(cat ../lender/payment.addr)

SOURCELENDER=$(cardano-cli-balance-fixer input --address ${lenderaddress} ${MAGIC})
SOURCESC3=$(cardano-cli-balance-fixer input --address ${sc3address} ${MAGIC})
collat=$(cardano-cli-balance-fixer collateral --address ${lenderaddress} ${MAGIC})

tokennameL=$(echo -n "L" | xxd -b -ps -c 80 | tr -d '\n')
tokennameMONY=$(echo -n "MONY" | xxd -b -ps -c 80 | tr -d '\n')
tokennameCONYMONY=$(echo -n "CONYMONY" | xxd -b -ps -c 80 | tr -d '\n')


lenderspolicy=$(cat lendersPolicyID)
policyid=$(cat mintingPolicy)

cardano-cli transaction build \
	${MAGIC} \
	${SOURCELENDER} \
	${SOURCESC3} \
	--tx-in-script-file ../sc3.plutus \
	--tx-in-datum-value 0 \
	--tx-in-redeemer-value 0 \
	--tx-in-collateral $collat \
	--tx-out $lenderaddress+2000000+"150 $policyid.$tokennameCONYMONY + 50 $policyid.$tokennameMONY" \
	--change-address $lenderaddress \
	--mint="-2 $lenderspolicy.$tokennameL" \
	--mint-script-file lenders.policy \
	--mint-redeemer-value 0 \
	--invalid-before 0 \
	--protocol-params-file ../protocol.json \
	--out-file matx.raw

cardano-cli transaction sign  \
	--signing-key-file ../lender/payment.skey  \
	--testnet-magic 1097911063 --tx-body-file matx.raw  \
	--out-file matx.signed

cardano-cli transaction submit --tx-file matx.signed --testnet-magic 1097911063
