#!/bin/bash

address=$(cat ../borrower/payment.addr)
MAGIC="--testnet-magic 1097911063"

SOURCE=$(cardano-cli-balance-fixer input --address ${address} ${MAGIC})
collat=$(cardano-cli-balance-fixer collateral --address ${address} ${MAGIC})

conytn=$(echo -n "CONY" | xxd -b -ps -c 80 | tr -d '\n')
monytn=$(echo -n "MONY" | xxd -b -ps -c 80 | tr -d '\n')
tokennameB=$(echo -n "B" | xxd -b -ps -c 80 | tr -d '\n')

tokenschange1=$(cardano-cli-balance-fixer change --address $(cat ../borrower/payment.addr) ${MAGIC})
res=$(egrep -o "([0-9]+[[:space:]]+[0-9a-f]+\.$monytn)" <<<"$tokenschange1")
rmit=$(egrep -o "([0-9]+[[:space:]]+[0-9a-f]+\.$conytn)" <<<"$tokenschange1")
echo "res: $res"
echo "rmit: $rmit"
amountofmony=$(cut -d ' ' -f 1 <<<"$res")
interestamnt=50
remainingmony=$(($amountofmony - $interestamnt))
remaining_tokens=$(sed "s/[0-9]*/${remainingmony}/" <<<"$res")
remaining_tokens1=$(sed "s/${res}/${remaining_tokens}/"  <<<"$tokenschange1")
rmit="${rmit} + "
remaining_tokens2=$(sed "s/${rmit}//"  <<<"$remaining_tokens1")

CHANGE=${address}+2000000+"${remaining_tokens2}"

source form_datum.sh
interestamnt=50

borrowerspolicy=$(cat policyID)
cardano-cli address build --payment-script-file ../sc1.plutus ${MAGIC} --out-file ../sc1.addr
script1=$(cat ../sc1.addr)
collatamnt=100
policyid=$(cat mintingPolicy)

cardano-cli transaction build \
	${MAGIC} \
	${SOURCE} \
	--tx-in-collateral $collat \
	--tx-out $address+2000000+"1 $borrowerspolicy.$tokennameB" \
	--tx-out "${CHANGE}" \
	--tx-out $script1+2000000+"$collatamnt $policyid.$conytn + $interestamnt $policyid.$monytn" \
	--tx-out-datum-embed-file borrower.datum \
	--change-address $address \
	--mint="1 $borrowerspolicy.$tokennameB" \
	--mint-script-file borrowers.policy \
	--mint-redeemer-value 0 \
	--protocol-params-file ../protocol.json \
	--out-file matx.raw

cardano-cli transaction sign  \
	--signing-key-file ../borrower/payment.skey  \
	--testnet-magic 1097911063 --tx-body-file matx.raw  \
	--out-file matx.signed

cardano-cli transaction submit --tx-file matx.signed --testnet-magic 1097911063
