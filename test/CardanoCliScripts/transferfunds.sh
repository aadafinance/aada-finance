#!/bin/bash

echo "trying to transfer funds to SC2"

MAGIC="--testnet-magic 1097911063"

cardano-cli address build --payment-script-file ../sc1.plutus ${MAGIC} --out-file ../sc1.addr
cardano-cli address build --payment-script-file ../sc2.plutus ${MAGIC} --out-file ../sc2.addr

lenderaddress=$(cat ../lender/payment.addr)
borroweraddress=$(cat ../borrower/payment.addr)
sc1address=$(cat ../sc1.addr)
sc2address=$(cat ../sc2.addr)

SOURCELENDER=$(cardano-cli-balance-fixer input --address ${lenderaddress} ${MAGIC})
SOURCESC1=$(cardano-cli-balance-fixer input --address ${sc1address} ${MAGIC})
collat=$(cardano-cli-balance-fixer collateral --address ${lenderaddress} ${MAGIC})

conytn=$(echo -n "CONY" | xxd -b -ps -c 80 | tr -d '\n')
monytn=$(echo -n "MONY" | xxd -b -ps -c 80 | tr -d '\n')
conymonytn=$(echo -n "CONYMONY" | xxd -b -ps -c 80 | tr -d '\n')
tokennameL=$(echo -n "L" | xxd -b -ps -c 80 | tr -d '\n')

source lender_nft.sh
echo

lenderspolicy=$(cat lendersPolicyID)
collatamnt=100
interestamnt=50
policyid=$(cat mintingPolicy)
alwayssucceedpid=$(cat policyID)
sc1change=$(cardano-cli-balance-fixer change --address ${sc1address} ${MAGIC})

cardano-cli transaction build \
	${MAGIC} \
	${SOURCELENDER} \
	${SOURCESC1} \
	--tx-in-script-file ../sc1.plutus \
	--tx-in-datum-file borrower.datum \
	--tx-in-redeemer-value 0 \
	--tx-in-collateral $collat \
	--tx-out $sc2address+2000000+"$collatamnt $alwayssucceedpid.$conytn + 1 $lenderspolicy.$tokennameL" \
	--tx-out-datum-embed-file borrower.datum \
	--tx-out $borroweraddress+2000000+"150 $alwayssucceedpid.$conymonytn" \
	--tx-out $lenderaddress+2000000+"1 $lenderspolicy.$tokennameL" \
	--change-address $address \
	--mint="2 $lenderspolicy.$tokennameL" \
	--mint-script-file lenders.policy \
	--mint-redeemer-value 0 \
	--protocol-params-file ../protocol.json \
	--out-file matx.raw

if [[ $? -ne 0 ]]; then
	echo "something went wrong when building transaction"
	exit $?
fi

cardano-cli transaction sign  \
	--signing-key-file ../lender/payment.skey  \
	--testnet-magic 1097911063 --tx-body-file matx.raw  \
	--out-file matx.signed

cardano-cli transaction submit --tx-file matx.signed --testnet-magic 1097911063

empty=$(cardano-cli query utxo ${MAGIC} --address $(cat ../sc2.addr) | grep -v TxHash | grep -v -)
while [[ -z $empty ]]; do
	empty=$(cardano-cli query utxo ${MAGIC} --address $(cat ../sc2.addr) | grep -v TxHash | grep -v -)
	sleep 1
done

echo "Funds locked in SC2"
echo
