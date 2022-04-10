#!/bin/bash
MAGIC="--testnet-magic 1097911063" 
PAYMENT_ADDR=$(cat wallet1/payment.addr)
SIGN_USER=wallet1/payment.skey
tokenname=$(echo -n "TEST" | xxd -b -ps -c 80 | tr -d '\n')

cardano-cli transaction policyid --script-file minting.script > policy/policyID
if [[ $? -ne 0 ]]; then
	echo "something went wrong when constructing transaction policyid"
	exit $?
fi

policyid=$(cat policy/policyID)
txhash="e96c08ef1d1f38566ecb0f12ae9cd38d9de5fce312ba45b929c2741c99441c57"

cardano-cli transaction build \
	--alonzo-era \
	--tx-in $txhash#0 \
	--required-signer-hash "29e6850767153c66dbf2228d601396c8d3c101ba144f38a571f1adc5" \
	--tx-in-collateral $txhash#0 \
	--tx-out ${PAYMENT_ADDR}+2000000+"2 $policyid.$tokenname" \
	--change-address ${PAYMENT_ADDR} \
	--mint="2 $policyid.$tokenname" \
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

empty=$(cardano-cli query utxo ${MAGIC} --address $(cat wallet1/payment.addr) | grep -v TxHash | grep -v - | grep $tokenname)
while [[ -z $empty ]]; do
	empty=$(cardano-cli query utxo ${MAGIC} --address $(cat wallet1/payment.addr) | grep -v TxHash | grep -v - | grep $tokenname)
	sleep 1
done

echo "Minting done. Locking funds to SC"

SOURCE=$(cardano-cli-balance-fixer input --address ${PAYMENT_ADDR} ${MAGIC})
cardano-cli address build --payment-script-file staking.plutus ${MAGIC} --out-file staking.addr
SCRIPT=$(cat staking.addr)

printf "{\n\t\"constructor\":0,\n\t\"fields\":\n\t[\n\t\t{\n\t\t\t\"bytes\":\"${policyid}\"\n\t\t},\n\t\t{\n\t\t\t\"bytes\":\"${tokenname}\"\n\t\t}\n\t]\n}\n" > staking.datum

cardano-cli transaction build \
	--alonzo-era \
	${SOURCE} \
	--tx-out ${PAYMENT_ADDR}+2000000+"1 $policyid.$tokenname" \
	--tx-out ${SCRIPT}+2000000+"1 $policyid.$tokenname" \
	--tx-out-datum-embed-file staking.datum \
	--change-address ${PAYMENT_ADDR} \
	--protocol-params-file protocol.json \
	${MAGIC} \
	--out-file matx.raw

if [[ $? -ne 0 ]]; then
	echo "something went wrong when building transaction for adding nft to sc"
	exit $?
fi

cardano-cli transaction sign \
	--signing-key-file ${SIGN_USER} \
	${MAGIC} --tx-body-file matx.raw \
	--out-file matx.signed

cardano-cli transaction submit --tx-file matx.signed ${MAGIC}

empty=$(cardano-cli query utxo ${MAGIC} --address $(cat staking.addr) | grep -v TxHash | grep -v - )
while [[ -z $empty ]]; do
	empty=$(cardano-cli query utxo ${MAGIC} --address $(cat staking.addr) | grep -v TxHash | grep -v - )
	sleep 1
done

echo "Done. Try to unlock now"
