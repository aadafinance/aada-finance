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
