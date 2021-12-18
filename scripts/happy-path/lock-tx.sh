#!/usr/bin/env bash

set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/..

NS="$1"
shift

DATUM_DIR="$baseDir/$BLOCKCHAIN_PREFIX/datums/$NS"
DATUM_FILE="$DATUM_DIR/vesting.json"
DATUM_HASH_FILE="${DATUM_FILE%.*}-hash.txt"

mkdir -p "$DATUM_DIR"

ARGS="--beneficiary $(cat $baseDir/$BLOCKCHAIN_PREFIX/beneficiary-pkh.txt)"

now="$(date)"
lovelaces=0

for i in "$@"; do
  ARGS="$ARGS --portion $(date -d "$now +$i seconds" '+%s'):1000000"
  lovelaces="$(($lovelaces + 1000000))"
done

echo $ARGS

cabal run vesting-sc -- datum --output "$DATUM_FILE"  $ARGS
cabal run vesting-sc -- write --output scripts/vesting.plutus

$baseDir/hash-plutus.sh

cardano-cli transaction hash-script-data \
  --script-data-file "$DATUM_FILE" \
  > "${DATUM_HASH_FILE}"

$baseDir/core/lock-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/benefactor.addr) \
  ~/$BLOCKCHAIN_PREFIX/benefactor.skey \
  $(cat $DATUM_HASH_FILE) \
  "$lovelaces lovelace"
