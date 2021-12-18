#!/usr/bin/env bash

set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../

DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/unlock-first-successfully-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/beneficiary.addr) \
  ~/$BLOCKCHAIN_PREFIX/beneficiary.skey \
  $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/vesting.json \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/vesting-hash.txt) \
  $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/invalid.json \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/invalid-hash.txt) \
  600000 \
  1400000
