# Markdown
## Mint token
- TxInComesIn
- For Each txOut there is does there exist txIn txOut such as:
  - it has lenderNft such as:
    - there exist txOut such as:
      - txOut contains lenderNft
      - txOut is to SafetyModule
      - txOut contains datum as value minted value

## Burn token
Token is burnt when:
- Liquidation request is canceled and LenderNft is being retrieved from `SafetyModule.hs`
  - there is txIn txOut such as:
    - Address -> SafetyModule
    - assetClassValueOf LenderNft == 1
OR
- there is txIn txOut such as:
  - Address -> `Interest.hs`

## Test SafetyToken
