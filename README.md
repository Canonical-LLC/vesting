# Vesting Smart Contract

This smart contract validates Ada is unlocked only when the vesting deadline has passed.

## Building

The compile the code to a Plutus smart contract, run:

```bash
cabal run vesting-sc -- write --output scripts/vesting.plutus
```

This will write a file to `scripts/vesting.plutus`

A `shell.nix` is also providing for nix users.

## Creating the Script Address

After compiling the smart contract, it is necessary to make a script address.

First source either the testnet or mainnet environment variables.

For testnet

```
$ source scripts/envars/testnet-env.envvars
```

For mainnet

```
$ source scripts/envars/mainnet-env.envvars
```

The environment variable files set `CARDANO_NODE_SOCKET_PATH` to the path of the appropriate Daedalus socket file (either Testnet Daedalus or the regular mainnet Daedalus). It you run a `cardano-node` on your own you should set this environment variable to your socket file location after sourcing the environment variable file.

Next, run:

```bash
scripts/hash-plutus.sh
```

This will make the files `testnet/vesting.addr` or `mainnet/vesting.addr`.

## Example Transactions

Example transactions can be found in `scripts/core`. The scripts are used by other scripts in `scripts/happy-path` which demonstrates how to create a vesting schedule and unlock vested value after the deadline has passed.

## Example Datums

The datum for a vesting schedule consists of the address of the beneficiary and the vesting schedule - a list of amounts and when they are vested and available to unlock by the beneficiary.

To create a datum file, the `vesting-sc` tool can be used

```
$ cabal run -- vesting-sc datum \
  --beneficiary 67614c1b06ddbb100cb6cbe919594cac31771c25530b6c7f28da242b \
  --portion $(date -d '+1 month' '+%s'):1000000 \
  --portion $(date -d '+2 month' '+%s'):1000000
```

This creates a datum with a vesting schedule for 2 Ada: the 1 Ada vests after one month and the second Ada vests after two months.

## Full System Testing Prerequistes

Before testing you need to make sure you have `cardano-cli` installed and on your path, and it must be version 1.31.0 or greater. You will also need the json utility `jq` as well as `cardano-cli` helper `cardano-cli-balance-fixer` which can be downloaded here: https://github.com/Canonical-LLC/cardano-cli-balance-fixer

## Init (only done once)

First create the wallets and get the protocol parameters.

```
$ ./scripts/wallets/make-all-wallets.sh
$ ./scripts/query-protocol-parameters.sh
```

# Manual Testing

We will walk through the process of manually testing creating a vesting schedule and unlocking vested amounts.

After following the setup steps above, first make sure that the `~/$BLOCKCHAIN_PREFIX/benefactor.addr` and `~/$BLOCKCHAIN_PREFIX/beneficiary.addr` have Ada.

Now create a vesting schedule by calling:

```bash
$ scripts/happy-path/lock-tx.sh 0 300 600
```

This will create a vesting schedule where 1 Ada vests after 300 seconds (5 minutes) and another Ada vests after 600 seconds (10 minutes). The `0` is namespace so we can have more than one auction going at a time.

Wait for the next slot:

```bash
$ scripts/wait/until-next-block.sh
```

You can now view the token at the smart contract address:

```bash
$ scripts/query/sc
```

When the time is right, call close:

```bash
$ scripts/happy-path/unlock-tx.sh
```

Wait for the next slot, and then check that the value was added to `beneficiary`'s wallet:

```bash
$ scripts/query/beneficiary
```

Note that if you wait until everything is vested, the beneficiary will have received all the value.


# Full System Tests

There are three large system tests that cover the main use cases and potential vulnerabilities we are aware of. The tests can be run on mainnet or testnet.

They take a long time to run, around 20 minutes, and can fail if the testnet is overloaded.

Luckily running them is easy:

```bash
$ ./scripts/tests/all.sh
```

The tests will start running. If the script errors one of the tests has failed.

If the scripts pass one must still verify that assets were transfered correctly at each step.

In the `temp/accounts/diff` directory, there will be subdirectories for each test flow. Within these directories are folders for each test step. If assets were transfer, there will be `json` files the account difference.

For instance after the first step to lock assets at the script address, the following `json` files are written:

```bash
$ cat temp/accounts/diffs/start-bid1-bid2-close.sh/0-1/sc.json
{"":{"":1758582},"d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2":{"123456":1}}
$ cat temp/accounts/diffs/start-bid1-bid2-close.sh/0-1/seller.json
{"":{"":-1942827},"d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2":{"123456":-1}}
```

This shows that the smart contract (`sc`), received a non-native token (`d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456`) and 1758582 lovelaces.

As expected, the seller lost _at least_ this much. Notice it lost more Ada, because of fees.
