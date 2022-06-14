# aada-tokens-staking

## Prerequisites

Install docker

### For M1 mac users

Plutus uses Haskell and Haskell is compiled using ghc which doesn't have arm support yet. To overcome this one of the possible solutions is to use [this virtualization](https://github.com/lima-vm/lima)

## Usage

### Get repo

```bash
git clone https://github.com/aada-finance/aada-finance.git
cd aada-finance
```

### Build image

```bash
docker build -t aada_lend .
```

### Compile smartcontracts

Compile and receive validators

```bash
docker run -v <host_directory>:/app aada_lend bash -c "/usr/local/bin/compile-validators <validation_script_name>"
```

### Compile minting script

General:
```bash
docker run -v <host_directory>:/app aada_lend bash -c "/usr/local/bin/mint-<option>-nft <minting_script_name> <utxo>"
```

Options:
- `mint-borrower-nft <utxo>`. Script name -> `<utxo>.borrowernft`
- `mint-lender-nft <utxo>`. Script name -> `<utxo>.lendernft`
- `mint-time-nft`. Script name -> `policyscript.timenft`

Example:
```bash
docker run -v /home/user/Programming/aada-finance:/app aada_lend bash -c "/usr/local/bin/mint-lender-nft 0a630191d2ba7fa96ecdc9096b826a1f9c210028e02fabc9c0288e2e37d3e2b8#0.borrowernft"
```

### Compile OracleNft minting script

General:
```bash
docker run -v <host_directory>:/app aada_lend bash -c "/usr/local/bin/mint-oracle-nft <pkh1> <pkh2> <pkh3> <valh> <tn>"
```

Options:
- `pkh1 -> PubKeyHash of party which mush sign to mint this Nft`
- `pkh2 -> PubKeyHash of party which mush sign to mint this Nft`
- `pkh3 -> PubKeyHash of party which mush sign to mint this Nft`
- `valh -> ValidatorHash where Nft Must be sent to be minted`
- `tn -> TokenName for Nft to be minted`

Example:
```bash
docker run -v /home/user/Programming/aada-finance:/app aada_lend bash -c "/usr/local/bin/mint-oracle-nft ff ff ff ff ff"
```