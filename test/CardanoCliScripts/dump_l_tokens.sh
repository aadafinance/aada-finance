#!/bin/bash


address=$(cat ../lender/payment.addr)
dump_address=$(cat ../wallet1/payment.addr)
echo "dump all lender native tokens to: $dump_address"
MAGIC="--testnet-magic 1097911063"

SOURCE=$(cardano-cli-balance-fixer input --address ${address} ${MAGIC})

tokenschange1=$(cardano-cli-balance-fixer change --address $(cat ../lender/payment.addr) ${MAGIC})

cardano-cli transaction build \
	${MAGIC} \
	${SOURCE} \
	--tx-out $dump_address+2000000+"$tokenschange1" \
	--change-address $address \
	--protocol-params-file ../protocol.json \
	--out-file matx.raw

cardano-cli transaction sign  \
	--signing-key-file ../lender/payment.skey  \
	--testnet-magic 1097911063 --tx-body-file matx.raw  \
	--out-file matx.signed

cardano-cli transaction submit --tx-file matx.signed --testnet-magic 1097911063


linesamt=$(cardano-cli query utxo ${MAGIC} --address $address | grep -v TxHash | grep -v - |wc -l )
echo "linesamt: $linesamt"
while [[ $linesamt -gt 1 ]]; do
	linesamt=$(cardano-cli query utxo ${MAGIC} --address $address | grep -v TxHash | grep -v - |wc -l )
	sleep 1
done

echo "clean up done"
echo
