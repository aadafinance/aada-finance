#!/bin/bash

echo "minting collateral CONY coins"

address=$(cat ../borrower/payment.addr)
MAGIC="--testnet-magic 1097911063"
cardano-cli transaction policyid --script-file oracle.nft > policyID
policyid=$(cat policyID)

SOURCE=$(cardano-cli-balance-fixer input --address ${address} ${MAGIC})
collat=$(cardano-cli-balance-fixer collateral --address ${address} ${MAGIC})

output=2000000
tokenamount=1
tokenname=$(echo -n "ORACLENFT" | xxd -b -ps -c 80 | tr -d '\n')

cardano-cli transaction build \
	${MAGIC} \
	${SOURCE} \
	--tx-in-collateral $collat \
	--tx-out $address+$output+"$tokenamount $policyid.$tokenname" \
	--change-address $address \
	--mint="$tokenamount $policyid.$tokenname" \
	--mint-script-file oracle.nft \
	--mint-redeemer-file redeemer-of-oracleNft.json \
	--protocol-params-file ../protocol.json \
	--out-file matx.raw

cardano-cli transaction build \
	${MAGIC} \
	${SOURCE} \
	--tx-in-collateral $collat \
	--tx-out $address+$output+"$tokenamount $policyid.$tokenname" \
	--change-address $address \
	--mint="$tokenamount $policyid.$tokenname" \
	--mint-script-file oracle.nft \
	--mint-redeemer-file redeemer-of-oracleNft.json \
	--protocol-params-file ../protocol.json \
	--cddl-format \
	--out-file cddl.raw

cardano-cli transaction sign  \
	--signing-key-file ../borrower/payment.skey  \
	--testnet-magic 1097911063 --tx-body-file matx.raw  \
	--out-file matx.signed


cardano-cli transaction submit --tx-file matx.signed --testnet-magic 1097911063

empty=$(cardano-cli query utxo ${MAGIC} --address $(cat ../borrower/payment.addr) | grep -v TxHash | grep -v - | grep $tokenname)
while [[ -z $empty ]]; do
	empty=$(cardano-cli query utxo ${MAGIC} --address $(cat ../borrower/payment.addr) | grep -v TxHash | grep -v - | grep $tokenname)
	sleep 1
done

echo "Minting CONYies done."
echo
