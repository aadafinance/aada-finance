#!/bin/bash
MAGIC="--testnet-magic 1097911063" 
PAYMENT_ADDR=$(cat wallet1/payment.addr)
SIGN_USER=wallet1/payment.skey
tokenname=$(echo -n "TEST" | xxd -b -ps -c 80 | tr -d '\n')

policyid=$(cat policy/policyID)

txinfo=$(cardano-cli query utxo ${MAGIC} --address $(cat staking.addr) | grep -v TxHash | grep -v - | grep $tokenname | grep $policyid)
txhash=$(cut -d ' ' -f1 <<< $txinfo)  

cardano-cli transaction build \
	--alonzo-era \
	--tx-in $txhash#0 \
	--tx-in $txhash#1 \
	--tx-in $txhash#2 \
	--tx-in-script-file staking.plutus \
	--tx-in-datum-file staking.datum \
	--tx-in-redeemer-value 0 \
	--tx-in-collateral $txhash#0 \
	--change-address ${PAYMENT_ADDR} \
	--mint="-2 $policyid.$tokenname" \
	--mint-script-file minting.script \
	--mint-redeemer-value 0 \
	--protocol-params-file protocol.json \
	${MAGIC} \
	--out-file matx.raw

if [[ $? -ne 0 ]]; then
	echo "something went wrong when building transaction for minting"
	exit $?
fi

cardano-cli transaction sign \
	--signing-key-file ${SIGN_USER} \
	${MAGIC} --tx-body-file matx.raw \
	--out-file matx.signed

cardano-cli transaction submit --tx-file matx.signed ${MAGIC}
