# AADA Lend

## About

This repository hosts on-chain code part of aada-lend project.

### Project structure

- `app/` - Exectuable files
- `src/` - Smartcontracts. Files ending with Nft are minting policies.
- `test/` - Files for testing project.

### System Actors

- Borrower
- Lender
- Oracle

### Use cases

#### Borrower can

- Create loan request
- Cancel loan request
- Return loan
- Retrieve rest of liquidated collateral

#### Lender can

- Lend
- Liquidate borrower loan by taking collateral
- Retrieve loan and loan interest

### Requirements

#### Lending

- Up until loan is given to Borrower Borrower should always be able to cancel his loan request.
- When canceling loan request Borrower should always be able to get all of his collateral back.
- Only Borrowers Nft owner can cancel loan request. Loan request can be canceled only if Borrowers nft is burnt.
- Loan receiver is identified by data encoded with datum.
- Loan request is created by locking collateral funds into `Request.hs` smartcontract together with datum.
- Borrower must provide this information when creating request:
  - **borrowersNftTn** - Borrowers token name encoding utxo to be consumed when minting token. See `AadaNft.hs`.
  - **borrowersAddress** - Who should receive the loan.
  - **loan** - Asset of requested loan.
  - **loanAmnt** - Amount of requested loan.
  - **interest** - Asset of interest to be paid.
  - **interestAmnt** - Amount of interest to be paid for loan.
  - **collateral** - Asset of collateral.
  - **collateralAmnt** - Amount of collateral.
  - **loanDuration** - How long till Lender gains rights to claim collateral. Once loan duration passes borrower must pay full **interestAmnt** to claim **collateral** back.
  - **liquidateNft** - Currency symbol of Nft which when minted grants Lender rights to take **collateral**.
  - **collateralFactor** - Collateral factor used for liquidation.
  - **liquidationComission** - How much % borrower will pay lender when liquidated (before **loanDuration** passes).
  - **requestExpiration** - When does loan request expire. Lender should not be able to provide Loan past this time value.
- Lender can get Vesting rights to borrowers collateral only when exact amount **loanAmnt** of **loan** is given to **borrowersAddress** and collateral funds are transfered to `Collateral.hs` smartcontract or when **liquidateNft** is minted.
- Lender can't get Vesting rights to borrowers collateral if loan is not given to owner of **borrowersAddress** which
is encoded in `Request.hs` smartcontract datum.
- Lender can't get Vesting rights to borrowers collateral if collateral from `Request.hs` smartcontract is not
transfered to `Collateral.hs` smartcontract.
- Lender can't get Vesting rights to borrower collateral if **lenderNftTn** is not minted.
- Lender can't retrieve collateral from `Collateral.hs` if **loanDuration** didn't pass in datum provided liquidate nft is not minted unless liqudation OracleNft is burnt.
- Borrower can't retrieve his collateral if borrowers nft encoded in datum is not burnt.
- Borrower can't retrieve his collateral if loan is not sent to `Interest.hs` smartcontract.
- Borrower can't retrieve his collateral if enough interest (depends on when loan is returned based on **loanDuration**) is not sent to `Interest.hs` smartcontract.
- Borrower can't retrieve his collateral if there are more than 3 different assets in repay transaction.
- If Borrower returns loan sooner than **repayinterval** he only needs to pay `(current time - loan taken time) / repay` interval amount of interest.
- Lender shouldn't be able to provide loan to borrower if loan request has expired.
- Lender shouldn't be able to provide loan if there is incoming utxo in transaction other than from `Request.hs`
- Lender shouldn't be able to provide loan if these correct datum fields are not provided together with datum already locked in `Request.hs`:
  - **lenderTn** - Lenders token name encoding utxo to be consumed when minting token. See `AadaNft.hs`.
  - **lendDate** - upper bound of transaction is valid to submit range.
- Lender shouldn't be able to provide loan if **rqeuestExpiration** is later than the the end of transaction is valid interval.
- Lender shouldn't be able to provide loan if there are more than 3 different types of assets in lend transaction.

#### Nfts minting requirements

- It shouldn't be possible to mint more than one token at a time
- It shouldn't be possible to burn more than one token at a time
- It shouldn't be possible to have two Nfts with same token name
- It shouldn't be possible to mint token unless utxo is consumed which hash is set as a token name

##### Lender Nft

- LenderNft is depicted by `isLender` minting policy parameter in `AadaNft.hs` if it is set to `True`.

##### Borrower nft

- BorrowerNft is depicted by `isLender` minting policy parameter in `AadaNft.hs` if it is set to `False`.

## Installation and usage

### Prerequisites

#### For M1 mac users

M1 Macs uses different instruction set (Arm is RISC) than the usuall Intel or AMD processors (CISC - Complex Instruction Set Computing).
As of writing this readme I haven't managed to make this project work and compile out of the box on M1 Mac and to solve that one solution I have found to work is by using virtualization with 
[lima-vm](https://github.com/lima-vm/lima)

Steps to set up environment for using `lima-vm`:

1. Install `lima-vm`
2. Launch lima and select ubuntu image
3. Edit default lima-vm config. Most import parts are:
  - Memory. I had 4GiB at first and it wasn't enough, but once I set it to 8Gib everything went smoothly
  - Arch. `x86_64`
  - Mounts - writeable: set it to `True`
4. Open lima
5. Dont forget to apt update
6. Install `nix`
7. Download `ghcup` dependencies. (These ghcup steps are probably optional because we are using nix? But its nice to have ghci at hand)
8. Download and install haskell with `ghcup`
9. Set up [nix cache](https://github.com/input-output-hk/plutus-apps#nix-1) inside lima instance
10. Clone [plutus repo](https://github.com/input-output-hk/plutus-apps). `nix-shell`
11. Clone this repository. Cd into it. Cabal build.

It does work slower than native, but it works.

Example `lima-vm` resulting configuration file `lima.yaml`:
```
arch: x86_64
images:
- location: "https://cloud-images.ubuntu.com/releases/21.10/release-20220201/ubuntu-21.10-server-cloudimg-amd64.img"
  arch: "x86_64"
  digest: "sha256:73fe1785c60edeb506f191affff0440abcc2de02420bb70865d51d0ff9b28223"
- location: "https://cloud-images.ubuntu.com/releases/21.10/release-20220201/ubuntu-21.10-server-cloudimg-arm64.img"
  arch: "aarch64"
  digest: "sha256:1b5b3fe616e1eea4176049d434a360344a7d471f799e151190f21b0a27f0b424"
- location: "https://cloud-images.ubuntu.com/releases/21.10/release/ubuntu-21.10-server-cloudimg-amd64.img"
  arch: "x86_64"
- location: "https://cloud-images.ubuntu.com/releases/21.10/release/ubuntu-21.10-server-cloudimg-arm64.img"
  arch: "aarch64"
cpus: null
memory: 12GiB
disk: null
mounts:
- location: "~"
  writable: true
  sshfs:
    cache: null
    followSymlinks: null
- location: "/tmp/lima"
  writable: true
ssh:
  localPort: 0
  loadDotSSHPubKeys: null
  forwardAgent: null
containerd:
  system: null
  user: null
cpuType:
  aarch64: null
  x86_64: null
firmware:
  legacyBIOS: null
video:
  display: null
networks:
propagateProxyEnv: null
hostResolver:
  enabled: null
  ipv6: null
```

#### Docker

Install docker.

### Instalation

#### Get repo

```bash
git clone https://github.com/aada-finance/aada-finance.git
cd aada-finance
```

#### Build image

```bash
docker build -t aada_lend .
```

#### Compile smartcontracts

To compile and create validators use `compile-validators` executable like so:

```bash
docker run -v <host_directory>:/app aada_lend bash -c "/usr/local/bin/compile-validators <options>"
```

Available options:
```
Usage: compile-validators [--hash STRING ((-k|--pubkey) | (-v|--validator)) |
                            --first INTEGER --second INTEGER --third INTEGER]
                          [-i|--interest FILEPATH] [-c|--collateral FILEPATH]
                          [-r|--request FILEPATH] [-l|--liquidation FILEPATH]
                          [--version]

Available options:
  -h,--help                Show this help text
  --hash STRING            Staking credential used to assign rewards. The
                           transaction that spends this output must be signed by
                           the key which hash is provided by this parameter
  -k,--pubkey              Interpret provided staking key hash as PubKeyHash
  -v,--validator           Interpret provided staking key hash as a Validator
                           Hash
  --first INTEGER          first StakingKey pointer parameter
  --second INTEGER         second StakingKey pointer parameter
  --third INTEGER          third StakingKey pointer parameter
  -i,--interest FILEPATH   Enter name of interest validator
                           (default: "interest.plutus")
  -c,--collateral FILEPATH Enter name of collateral validator
                           (default: "collateral.plutus")
  -r,--request FILEPATH    Enter name of request validator
                           (default: "request.plutus")
  -l,--liquidation FILEPATH
                           Enter name of liquidation validator
                           (default: "liquidation.plutus")
  --version                Show project version
```

#### Compile minting scripts

To get minting policies use `mint-aada-nft` executable like so:

General:
```bash
docker run -v <host_directory>:/app aada_lend bash -c "/usr/local/bin/mint-aada-nft minting-policy <options>"
```

Options:
```
Usage: mint-aada-nft minting-policy [-p|--name FILEPATH]
                                    ((-b|--borrower) | (-l|--lender))
  Generate LenderNFT minting policy

Available options:
  -h,--help                Show this help text
  -p,--name FILEPATH       Enter name of aada nft minting policy. Default is
                           aada.nft (default: "aada.nft")
  -b,--borrower            choose borrower policy
  -l,--lender              choose lender policy
```

### Compile OracleNft minting script

Geting example OracleNft minting script:
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

### Usage

#### Get example datum and redeemers

To get example datum and redeemer files use `generate-example-jsons` executable like so:
```bash
docker run -v <host_directory>:/app aada_lend bash -c "/usr/local/bin/generate-example-jsons"
```

#### Getting TokenName for Borrower or Lender token

To get TokenName for minting Borrower or Lender token use `mint-aada-nft` executable like so:
```bash
docker run -v <host_directory>:/app aada_lend bash -c "/usr/local/bin/mint-aada-nft token-name --utxo ff#0"
```

Options:
```
Usage: mint-aada-nft token-name (-u|--utxo UTXO)
  Hash utxo get get Lender NFT token name

Available options:
  -h,--help                Show this help text
  -u,--utxo UTXO           Enter utxo to be consumed when minting LenderNFT.
```

#### Creating Loan request

1. Compile smartcontracts (see Instalation and usage)
2. Get addreesses of compiled smartcontracts: `cardano-cli address build --payment-script-file <script.plutus>  --out-file validator.addr`
3. Form datum following this format:
```json
{
    "fields": [
        {
            "bytes": "6666"
        },
        {
            "fields": [
                {
                    "fields": [
                        {
                            "bytes": "ff"
                        }
                    ],
                    "constructor": 0
                },
                {
                    "fields": [
                        {
                            "fields": [
                                {
                                    "fields": [
                                        {
                                            "bytes": "6666"
                                        }
                                    ],
                                    "constructor": 0
                                }
                            ],
                            "constructor": 0
                        }
                    ],
                    "constructor": 0
                }
            ],
            "constructor": 0
        },
        {
            "fields": [
                {
                    "bytes": "10555a8c6aa2217ed76522758b804a7531d01ff5ee53012ad81ebf23"
                },
                {
                    "bytes": "6c6f616e2d636f696e2d434f4e594d4f4e59"
                }
            ],
            "constructor": 0
        },
        {
            "int": 150
        },
        {
            "fields": [
                {
                    "bytes": "5a1a36a39eb541a7db0e7211ae16ed958c55414ed1ef51e2e7d763bb"
                },
                {
                    "bytes": "696e7465726573742d636f696e2d4d4f4e59"
                }
            ],
            "constructor": 0
        },
        {
            "int": 50
        },
        {
            "fields": [
                {
                    "bytes": "57cbe9798a05a712df222de7defd4b37a0d783bb40710ea63f64f99e"
                },
                {
                    "bytes": "636f6c6c61746572616c2d636f696e2d434f4e59"
                }
            ],
            "constructor": 0
        },
        {
            "int": 100
        },
        {
            "int": 0
        },
        {
            "bytes": "ff"
        },
        {
            "int": 5
        },
        {
            "int": 150
        },
        {
            "int": 0
        },
        {
            "bytes": "6e6674746e"
        },
        {
            "int": 0
        }
    ],
    "constructor": 0
}
```

Important parameters to be provided are:
- **borrowersNftTn** - Borrowers token name encoding utxo to be consumed when minting token. See `AadaNft.hs`.
- **borrowersAddress** - Who should receive the loan.
- **loan** - Asset of requested loan.
- **loanAmnt** - Amount of requested loan.
- **interest** - Asset of interest to be paid.
- **interestAmnt** - Amount of interest to be paid for loan.
- **collateral** - Asset of collateral.
- **collateralAmnt** - Amount of collateral.
- **loanDuration** - How long till Lender gains rights to claim collateral. Once loan duration passes borrower must pay full **interestAmnt** to claim **collateral** back.
- **liquidateNft** - Currency symbol of Nft which when minted grants Lender rights to take **collateral**.
- **collateralFactor** - Collateral factor used for liquidation.
- **liquidationComission** - How much % borrower will pay lender when liquidated (before **loanDuration** passes).
- **requestExpiration** - When does loan request expire. Lender should not be able to provide Loan past this time value.
4. Create transaction like so:
```
           collateralAmnt   ┌──┐       collateralAmnt of    ┌──────────┐
┌────────┐ of collateral +  │  │─────────collateral + ────▶│Request.hs│
│Borrower│─datum + tx fees─▶│  │         datum + 2 Ada      └──────────┘
└────────┘     + 4 Ada      │  │
                            │Tx│
┌────────┐ borrower token   │  │         2 Ada + 1          ┌──────────┐
│AadaNft │─minting policy──▶│  ├───AadaNft.borrowerNftTn──▶│ Borrower │
└────────┘     script       │  │                            └──────────┘
                            └──┘
```
> Warning: Borrower won't be able to claim his collateral back if correct Borrower Token is not minted when locking collateral!
5. Submit transaction

#### Providing loan

1. Compile smartcontracts (see Instalation and usage)
2. Get addreesses of compiled smartcontracts: `cardano-cli address build --payment-script-file <script.plutus>  --out-file validator.addr`
3. Form datum. (See #### Creating Loan Request for example)
Important parameters to be updated are:
- **lenderNftTn** - Lenders token name encoding utxo to be consumed when minting token. See `AadaNft.hs`.
- **lendDate** - Upper bound of transaction is valid to submit range.
4. Form redeemer with **lenderNftTn** like so:
```json
{
    "bytes": "4b3a43f592f577fcfcb5b0e1f42bec5182c9edc414e1f667528f56e7cf0be11d"
}
```
5. Create transaction like so:
```
                               ┌──┐
┌──────┐   loanAmnt of loan +  │  │         2 Ada + 1          ┌──────┐
│Lender│───tx fees + 6 Ada + ─▶│  │────AadaNft.lenderNftTn───▶│Lender│
└──────┘    datum + redeemer   │  │                            └──────┘
                               │  │
┌──────────┐ collateralAmnt of │  │ collateralAmnt of  ┌───────────────┐
│Request.hs│───collateral + ──▶│Tx├───collateral + ──▶│ Collateral.hs │
└──────────┘   datum + 2 Ada   │  │   datum + 2 Ada    └───────────────┘
                               │  │
┌────────┐    lender token     │  │    loanAmnt of          ┌──────────┐
│AadaNft │───minting policy───▶│  ├───loan + 2 Ada────────▶│ Borrower │
└────────┘       script        │  │                         └──────────┘
                               └──┘
```
5. Submit transaction

#### Repaying loan

1. Compile smartcontracts (see Instalation and usage)
2. Get addreesses of compiled smartcontracts: `cardano-cli address build --payment-script-file <script.plutus>  --out-file validator.addr`
3. Form datum with **lenderNftTn** like so:
```json
{"bytes":"4b3a43f592f577fcfcb5b0e1f42bec5182c9edc414e1f667528f56e7cf0be11d"}
```
4. Create transaction like so:
```
           loanAmnt of loan + x   ┌──┐
┌────────┐ of interest + tx fees  │  │
│Borrower│──────────+ 1 ─────────▶│  │
└────────┘ AadaNft.borrowerNftTn  │  │
              + 4 Ada + datum     │  │ loanAmnt of loan + x    ┌───────────┐
                                  │  │─of interest + 2 Ada +─▶│Interest.hs│
                                  │  │         datum           └───────────┘
┌─────────────┐ collateralAmnt of │Tx│
│Collateral.hs│───collateral + ──▶│  │
└─────────────┘   datum + 2 Ada   │  │  collateralAmnt         ┌──────────┐
                                  │  ├──of collateral +──────▶│ Borrower │
┌────────┐      borrower token    │  │       2 Ada             └──────────┘
│AadaNft │──────minting policy───▶│  │
└────────┘          script        │  │
                                  └──┘
```
5. Submit transaction

#### Retrieve interest

1. Compile smartcontracts (see Instalation and usage)
2. Get addreesses of compiled smartcontracts: `cardano-cli address build --payment-script-file <script.plutus>  --out-file validator.addr`
3. Form datum with **lenderNftTn** like so:
```json
{"bytes":"4b3a43f592f577fcfcb5b0e1f42bec5182c9edc414e1f667528f56e7cf0be11d"}
```
4. Create transaction like so:
```
┌───────┐                         ┌──┐
│Lender │───────tx fees + 1 ─────▶│  │
└───────┘ AadaNft.lenderNftTn + 2 │  │
                Ada + datum       │  │
                                  │  │
┌─────────────┐ loanAmnt of loan  │Tx│  loanAmnt of loan +     ┌──────────┐
│ Interest.hs │─+ x of interest +▶│  ├───x of interest + 2 ──▶│  Lender  │
└─────────────┘   2 Ada + datum   │  │          Ada            └──────────┘
                                  │  │
┌────────┐       lender token     │  │
│AadaNft │──────minting policy───▶│  │
└────────┘          script        └──┘
```
5. Submit transaction

#### Liquidate borrower

1. Compile smartcontracts (see Instalation and usage)
2. Get addreesses of compiled smartcontracts: `cardano-cli address build --payment-script-file <script.plutus>  --out-file validator.addr`

##### Deadline has passed

3. Create transaction like so:
```
                                  ┌──┐
┌────────┐       2 Ada + 1        │  │
│ Lender │─AadaNft.lenderNftTn +─▶│  │
└────────┘    tx fees + datum     │  │
                                  │  │
┌─────────────┐ collateralAmnt of │Tx│    collateralAmnt       ┌──────────┐
│Collateral.hs│───collateral + ──▶│  ├────of collateral +────▶│  Lender  │
└─────────────┘   datum + 2 Ada   │  │         2 Ada           └──────────┘
                                  │  │
┌────────┐       lender token     │  │
│AadaNft │──────minting policy───▶│  │
└────────┘          script        └──┘
```

##### Liquidation with Oracle

3. Form datum for `Liquidate.hs`, provide `borrowerNftTn` like so:
```json
{"bytes":"4b3a43f592f577fcfcb5b0e1f42bec5182c9edc414e1f667528f56e7cf0be11d"}
```
4. Create transaction like so:
```
                                  ┌──┐
┌────────┐       2 Ada + 1        │  │
│ Lender │─AadaNft.lenderNftTn +─▶│  │
└────────┘    tx fees + datum     │  │                         ┌──────────┐
                                  │  ├───x of collateral─────▶│  Lender  │
┌─────────────┐ collateralAmnt of │  │       + 2 Ada           └──────────┘
│Collateral.hs│───collateral + ──▶│  │
└─────────────┘   datum + 2 Ada   │Tx│
                                  │  │
┌────────┐       lender token     │  │ borrowerNftTn Datum +
│AadaNft │──────minting policy───▶│  │        2 Ada +       ┌────────────┐
└────────┘          script        │  ├─(collateralAmnt - x) │Liquidate.hs│
                                  │  │     of collateral    └────────────┘
┌────────┐ minting policy + rest  │  │
│ Oracle │─what is required like ▶│  │
└────────┘      signatures        └──┘
```
5. Submit transaction

#### Staking

It is possible to receive staking rewards from the funds kept in smartcontracts.
Smartcontracts can be compiled with wanted staking key by providing it either:
- `--hash STRING ((-k|--pubkey) | (-v|--validator))` with concrete hash
or
- `--first INTEGER --second INTEGER --third INTEGER` with staking key pointer

## aada-tokens-staking

### Build image

```bash
docker build -t aada_staking .
```

### Compile smartcontract

```bash
docker run -v <host_directory>:/app aada_staking bash -c "/usr/local/bin/aada-staking 0 <validation_script_name>"
```

### Compile minting script

General:
```bash
docker run -v <host_directory>:/app aada_staking bash -c "/usr/local/bin/aada-staking 1 <minting_script_name> <PubKeyHash> <utxo>"
```

Example:
```bash
docker run -v /home/user/Programming/aada-finance:/app aada_staking bash -c "/usr/local/bin/aada-staking 1 mint.script 72193caa8e2eaca97c8461f837e7a4d7cd781b0ba6bf626a883cc102 0a630191d2ba7fa96ecdc9096b826a1f9c210028e02fabc9c0288e2e37d3e2b8#0"
```

## Contribution guide

### Have a proposal?

Raise an issue with a title `proposal - <proposal name>`!

Use this template:
```
# <Proposal name>

## What would you like to be different?

## Why do you think this change would improve this project?
```

Raise an issue with a feature request proposal!

### Have a suggested change?

1. Raise a proposal issue
2. Create tests to cover changes
3. Implement changes
4. Make sure nothing breaks, run tests with `cabal test`
5. Create PR
6. Wait for approval

### Found a bug?

Raise an issue with a title `bug - <bug name>`!

1. Use this template:
```
# <bug name>

## Your setup

## What is not working as expected?

## What behaviour did you expect?

## What do you think is causing unexpected behaviour?

## How do you think this problem could be resolved?
```

2. Create branch
3. Implement changes
4. Create tests to cover changes
5. Create pull request

## Contact us

[discord](https://discord.gg/rzVMbFWw)

[telegram](https://t.me/aadacommunity)

[twitter](https://twitter.com/AadaFinance)

[repo manager](https://github.com/tomazvila)

- email: `tomas@aada.finance`
