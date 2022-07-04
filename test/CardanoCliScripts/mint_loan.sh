#!/bin/bash

echo "minting CONYMONY coins"

address=$(cat ../lender/payment.addr)
MAGIC="--testnet-magic 1097911063"
cardano-cli transaction policyid --script-file ../always-succeed.script > policyID
policyid=$(cat policyID)

SOURCE=$(cardano-cli-balance-fixer input --address ${address} ${MAGIC})
collat=$(cardano-cli-balance-fixer collateral --address ${address} ${MAGIC})

output=2000000
tokenamount=150
tokenname=$(echo -n "CONYMONY" | xxd -b -ps -c 80 | tr -d '\n')

cardano-cli transaction build \
	${MAGIC} \
	${SOURCE} \
	--tx-in-collateral $collat \
	--tx-out $address+$output+"$tokenamount $policyid.$tokenname" \
	--change-address $address \
	--mint="$tokenamount $policyid.$tokenname" \
	--mint-script-file ../always-succeed.script \
	--mint-redeemer-value 0 \
	--protocol-params-file ../protocol.json \
	--out-file matx.raw

cardano-cli transaction sign  \
	--signing-key-file ../lender/payment.skey  \
	--testnet-magic 1097911063 --tx-body-file matx.raw  \
	--out-file matx.signed

cardano-cli transaction submit --tx-file matx.signed --testnet-magic 1097911063

empty=$(cardano-cli query utxo ${MAGIC} --address $(cat ../lender/payment.addr) | grep -v TxHash | grep -v - | grep $tokenname)
while [[ -z $empty ]]; do
	empty=$(cardano-cli query utxo ${MAGIC} --address $(cat ../lender/payment.addr) | grep -v TxHash | grep -v - | grep $tokenname)
	sleep 1
done

echo "Minting CONYMONYies done."
echo
