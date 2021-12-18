#!/usr/bin/env bash

set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/..

NS="$1"
shift

DATUM_DIR="$baseDir/$BLOCKCHAIN_PREFIX/datums/$NS"
DATUM_FILE="$DATUM_DIR/vesting.json"
DATUM_HASH_FILE="${DATUM_FILE%.*}-hash.txt"
INVALID_DATUM_FILE="$DATUM_DIR/invalid.json"

mkdir -p "$DATUM_DIR"

ARGS="--beneficiary $(cat $baseDir/$BLOCKCHAIN_PREFIX/beneficiary-pkh.txt)"

now="$(date +%s)"
nowMill=$(($now+$1))
lovelaces=0

for i in "$@"; do
  timestamp=$(($nowMill+$i))
  ARGS="$ARGS --portion $timestamp:1000000"
  lovelaces="$(($lovelaces + 1000000))"
done

echo $ARGS

cabal run vesting-sc -- datum --output "$DATUM_FILE"  $ARGS

cabal run vesting-sc -- datum --output "$INVALID_DATUM_FILE" \
  --beneficiary $(cat $baseDir/$BLOCKCHAIN_PREFIX/beneficiary-pkh.txt) \
  --portion $nowMill:1000000

cabal run vesting-sc -- write --output scripts/vesting.plutus

$baseDir/hash-plutus.sh

find $baseDir/$BLOCKCHAIN_PREFIX/datums -name "*.json" \
  -exec sh -c 'cardano-cli transaction hash-script-data --script-data-file "$1" > "${1%.*}-hash.txt"' sh {} \;


$baseDir/core/lock-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/benefactor.addr) \
  ~/$BLOCKCHAIN_PREFIX/benefactor.skey \
  $(cat $DATUM_HASH_FILE) \
  "$lovelaces lovelace"
