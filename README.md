# aada-tokens-staking

## About

This repository hosts on-chain code part of aada-lend project.

### Project structure

- `app/` - Exectuable files
- `src/` - Smartcontracts. Files ending with Nft are minting policies.
- `test/` - Files for testing project.

### System Actors

- Borrower
- Lender

### Use cases

#### Borrower can

- Create loan request
- Cancel loan request
- Return loan

#### Lender can

- Lend
- Liquidate borrower loan by taking collateral
- Retrieve loan and loan interest

### Requirements

#### Aada lend smartcontract part

- Up until loan is given Borrower should always be able to cancel his loan request.
- When canceling loan request Borrower should always be able to get all of his collateral back.
- Only Borrowers Nft owner can cancel loan request. Loan request can be canceled only if Borrowers nft is burnt.
- Loan receiver is identified by data encoded with datum.
- Loan request is created by locking collateral funds into  Request.hs  smartcontract together with datum.
- Borrower must provide this information when creating request:

```haskell
data Datum = Datum
    { borrowersNFT  :: !CurrencySymbol     -- collateral provider id
    , borrowersPkh  :: !PaymentPubKeyHash  -- who shall get loan
    , loantn        :: !TokenName          -- loan asset token name
    , loancs        :: !CurrencySymbol     -- loan asset currency symbol
    , loanamnt      :: !Integer            -- amount of loan
    , interesttn    :: !TokenName          -- interest asset token name
    , interestcs    :: !CurrencySymbol     -- interest currency symbol
    , interestamnt  :: !Integer            -- amount of interest
    , collateralcs  :: !CurrencySymbol     -- collateral currency symbol
    , repayinterval :: !POSIXTime          -- repay interval
    , liquidateNft  :: !CurrencySymbol     -- liquidation oracle id
    , requestExp    :: !POSIXTime          -- loan request expiration
    } deriving Show
```


- Lender can get Vesting rights to borrowers collateral only when loan is given to borrower and collateral funds are
transfered to  Collateral.hs  smartcontract.
- Lender can't get Vesting rights to borrowers collateral if loan is not given to owner of PaymentPubKeyHash which
is encoded in  Request.hs  smartcontract datum.
- Lender can't get Vesting rights to borrowers collateral if 1 time NFT is not sent to  Collateral.hs  .
- Lender can't get Vesting rights to borrowers collateral if collateral from  Request.hs   smartcontract is not
transfered to  Collateral.hs  smartcontract.
Lender can't get Vesting rights to borrowers collateral if datum provided with  Request.hs  is different than
datum provided to  Collateral.hs  smartcontract.
- Lender can't get Vesting rights to borrowers collateral if 2 Lender NFTs are not minted and one of them is not
locked into  Collateral.hs  smartcontract.
- Lender can't retrieve collateral from  Collateral.hs  if  repayinterval  didn't pass and 1 timenft is not burnt or
datum provided liquidate nft is not minted/burnt.
- Lender can't retrieve collateral from  Collateral.hs  if 2 Lender Nfts are not burnt and one of them is not from
Collateral.hs  smart contract.
- Borrower can't retrieve his collateral if borrowers nft encoded in datum is not burnt.
- Borrower can't retrieve his collateral if loan is not sent to  Interest.hs  smartcontract.
- Borrower can't retrieve his collateral if enough interest is not sent to  Interest.hs  smartcontract.
- Borrower can't retrieve his collateral if Lender nft is not sent to  Interest.hs  smartcontract.
- Lender can't retrieve his interest from  Interest.hs  smartcontract if 2 Lender Nfts are not burnt and one of them
is not from  Interest.hs  smart contract.
- If Borrower returns laon sooner than  repayinterval  he only needs to pay  (current time - loan taken time) / repay
interval  amount of interest

#### Nfts minting requirements

##### Borrower nft

- It shouldn't be possible to mint more than one Nft at a time
- It shouldn't be possible to have two Nfts with same CurrencySymbol
- It shouldn't be possible to mint Borrower nft with other token name than  66 -> B

##### Lender Nft

- It shouldn't be possible to mint other than 2 Nfts at a time
- It shouldn't be possible to mint Nft if 1 of them is not sent to  Collateral.hs  smartcontract
- Only two Nfts can be burnt at a time
- It shouldn't be possible to mint Lender nft with other token name than  76 -> L

##### Time Nft

- It shouldn't be possible to mint Time Nft with other token name than POSIXTime provided as a NFT parameter
- It shouldn't be possible to mint Time Nft with bigger time than provided with POSIXTime and than the time which is
currently present

## Installation

### For M1 mac users

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

## Prerequisites

If using docker install docker.

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

Compile and create validators:

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

### Run tests

```
cabal test
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

### Have a suggested change?

1. Raise a proposal issue
2. Implement changes
3. Create PR

### Found a bug?

Raise an issue with a title `bug - <bug name>`!

Use this template:
```
# <bug name>

## Your setup

## What is not working as expected?

## What behaviour did you expect?

## What do you think is causing unexpected behaviour?

## How do you think this problem could be resolved?
```
