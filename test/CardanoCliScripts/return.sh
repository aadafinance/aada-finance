#!/bin/bash

MAGIC="--testnet-magic 1097911063"

cardano-cli address build --payment-script-file ../sc3.plutus ${MAGIC} --out-file ../sc3.addr

sc2address=$(cat ../sc2.addr)
sc3address=$(cat ../sc3.addr)
borroweraddress=$(cat ../borrower/payment.addr)

SOURCEBORROWER=$(cardano-cli-balance-fixer input --address ${borroweraddress} ${MAGIC})
SOURCESC2=$(cardano-cli-balance-fixer input --address ${sc2address} ${MAGIC})
collat=$(cardano-cli-balance-fixer collateral --address ${borroweraddress} ${MAGIC})

conytn=$(echo -n "CONY" | xxd -b -ps -c 80 | tr -d '\n')
monytn=$(echo -n "MONY" | xxd -b -ps -c 80 | tr -d '\n')
conymonytn=$(echo -n "CONYMONY" | xxd -b -ps -c 80 | tr -d '\n')
tokennameB=$(echo -n "B" | xxd -b -ps -c 80 | tr -d '\n')
tokennameL=$(echo -n "L" | xxd -b -ps -c 80 | tr -d '\n')

borrowerspolicy=$(cat borrowersPolicyID)
lenderpolicy=$(cat lendersPolicyID)
interestamnt=50
collatamnt=100
loanamnt=150
policyid=$(cat mintingPolicy)

cardano-cli transaction build \
	${MAGIC} \
	${SOURCEBORROWER} \
	${SOURCESC2} \
	--tx-in-script-file ../sc2.plutus \
	--tx-in-datum-file borrower.datum \
	--tx-in-redeemer-value 0 \
	--tx-in-collateral $collat \
	--tx-out $borroweraddress+2000000+"100 $policyid.$conytn" \
	--tx-out $sc3address+2000000+"150 $policyid.$conymonytn + 50 $policyid.$monytn + 1 $lenderpolicy.$tokennameL" \
	--tx-out-datum-hash-value 0 \
	--change-address $borroweraddress \
	--mint="-1 $borrowerspolicy.$tokennameB" \
	--mint-script-file borrowers.policy \
	--mint-redeemer-value 0 \
	--protocol-params-file ../protocol.json \
	--out-file matx.raw

cardano-cli transaction sign  \
	--signing-key-file ../borrower/payment.skey  \
	--testnet-magic 1097911063 --tx-body-file matx.raw  \
	--out-file matx.signed

cardano-cli transaction submit --tx-file matx.signed --testnet-magic 1097911063


empty=$(cardano-cli query utxo ${MAGIC} --address $(cat ../sc3.addr) | grep -v TxHash | grep -v -)
while [[ -z $empty ]]; do
	empty=$(cardano-cli query utxo ${MAGIC} --address $(cat ../sc3.addr) | grep -v TxHash | grep -v -)
	sleep 1
done

echo "Interest with loan and lenders nft transfered to SC3"
echo
