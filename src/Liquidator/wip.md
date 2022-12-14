# Markdown

### create request transaction
```
           collateralAmnt   ┌──┐       collateralAmnt of   ┌──────────┐
┌────────┐ of collateral +  │  │─────────collateral + ────▶│Request.hs│
│Borrower│─datum + tx fees─▶│  │         datum + 2 Ada     └──────────┘
└────────┘     + 4 Ada      │  │
                            │Tx│
┌────────┐ borrower token   │  │         2 Ada + 1         ┌──────────┐
│AadaNft │─minting policy──▶│  ├───AadaNft.borrowerNftTn──▶│ Borrower │
└────────┘     script       │  │                           └──────────┘
                            └──┘
```

### provide loan transaction
```
                               ┌──┐
┌──────┐   loanAmnt of loan +  │  │         2 Ada + 1         ┌──────┐
│Lender│───tx fees + 6 Ada + ─▶│  │────AadaNft.lenderNftTn───▶│Lender│
└──────┘    datum + redeemer   │  │                           └──────┘
                               │  │
┌──────────┐ collateralAmnt of │  │ collateralAmnt of ┌───────────────┐
│Request.hs│───collateral + ──▶│Tx├───collateral + ──▶│ Collateral.hs │
└──────────┘   datum + 2 Ada   │  │   datum + 2 Ada   └───────────────┘
                               │  │
┌────────┐    lender token     │  │    loanAmnt of         ┌──────────┐
│AadaNft │───minting policy───▶│  ├───loan + 2 Ada────────▶│ Borrower │
└────────┘       script        │  │                        └──────────┘
                               └──┘
```

### opt for liquidator transaction
```
                                     ┌──┐
┌──────┐                             │  │         2 Ada +              ┌──────┐
│Lender│───1 LenderNft + 4 Ada──────▶│  ├──────1 SafetyToken──────────▶│Lender│
└──────┘                             │  │                              └──────┘
                                     │Tx│
┌──────────────┐   MintingPolicy     │  │     2 Ada + 1              ┌───────────────┐
│SafetyToken.hs│───────script───────▶│  ├────LenderNft + ───────────▶│SafetyModule.hs│
└──────────────┘                     │  │    AssetClass Datum        └───────────────┘
                                     └──┘
```

### create liquidation request
```
Liquidate loan

┌─────────┐                               ┌──┐
│Oracle.hs│─────MintingPolicy Script─────▶│  │
└─────────┘                               │  │
┌───────────────┐  2 Ada + 1 LenderNft    │  │     comissions          ┌────────────┐
│SafetyModule.hs│─────────Datum──────────▶│  ├──────+ 2 Ada───────────▶│ Liquidator │
└───────────────┘                         │  │                         └────────────┘
                                          │Tx│
┌─────────────┐    collateralAmnt of      │  │                          ┌───────────┐
│Collateral.hs│─collateral + datum + 2 ──▶│  ├─reminaing collateral +──▶│Interest.hs│
└─────────────┘           Ada             │  │  tokenNameDat + 2 Ada    └───────────┘
                                          │  │
┌────────────┐                            │  │
│ Liquidator │────────2 Ada──────────────▶│  │
└────────────┘                            └──┘
```

#### Collateral comisions calculation
```
collateralFactor      :: !Integer   -- Colalteral factor used for liquidation
liquidationCommission :: !Integer   -- How much % borrower will pay for lender when liquidated (before time passes)
```

## Test cancel liquidation request
```
                                 ┌──┐
┌──────┐            2 Ada +      │  │
│Lender│─────────1 SafetyToken──▶│  │
└──────┘                         │  │
                                 │  │
┌───────────────┐   2 Ada + 1    │Tx│                        ┌──────┐
│SafetyModule.hs│───LenderNft ──▶│  ├──2 Ada + 1 LenderNft──▶│Lender│
└───────────────┘    + Datum     │  │                        └──────┘
                                 │  │
┌──────────────┐ MintingPolicy   │  │
│SafetyToken.hs│─────script─────▶│  │
└──────────────┘                 └──┘
```
