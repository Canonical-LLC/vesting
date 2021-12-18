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
  800000 \
  1200000
