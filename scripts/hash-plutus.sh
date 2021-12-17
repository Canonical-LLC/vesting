#!/usr/bin/env bash

set -eu

thisDir=$(dirname "$0")

cardano-cli address build \
  --payment-script-file $thisDir/vesting.plutus \
  $BLOCKCHAIN \
  --out-file $thisDir/$BLOCKCHAIN_PREFIX/vesting.addr
