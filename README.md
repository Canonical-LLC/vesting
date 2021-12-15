# Vesting Smart Contract

This smart contract validates Ada is split correctly between multiple addresses.

During compilation, a list of address and percentage pairs are passed in. When validating the disbursement transaction, the smart contract ensures each address receives the correct percentage that was locked at the script address.

## Building

To build, run:

```bash
$ cabal build
```

A `shell.nix` is also provided for nix users.

## Installing the Smart Contract Generator

This library includes an executable to build the smart contracts. To install it to a specific directory, run:

```
$ cabal install exe:vesting-sc --install-method=copy --installdir=YOUR_INSTALLATION_DIR
```

Where `YOUR_INSTALLATION_DIR` is a directory of your choosing.

## Compiling the Smart Contracts

To compile the smart contract, use the provided executable `vesting-sc`.

Running `vesting-sc --help` gives:

```bash
Usage: vesting-sc COMMAND
  Create a smart contract for sharing

Available options:
  -h,--help                Show this help text

Available commands:
  write
  datum
```

```bash
$ vesting-sc write \
    --output SC_FILEPATH
```

Where `SC_FILEPATH` is the location you would like to store the .plutus cbor.

After the .plutus file is written, one must create a script address using the `cardano-cli`:

```bash
$ cardano-cli address build \
  --payment-script-file vesting.plutus --mainnet \
  --out-file vesting.addr
```

## Example Transactions

To create a vesting schedule for a beneficiary, Ada must be sent to the script address created in the step above with a hash of the datum, which contains the beneficiary and vesting schedule.

Here is an example transaction:

```bash
cardano-cli transaction build \
    --alonzo-era \
    --mainnet \
    --tx-in 2a27d27eb9d32a3c98276eb65fbeba4d0e134679726f7af78521c403de08311e#0 \
    --tx-out "$(cat vesting.addr) + 12000000 lovelace" \
    --tx-out-datum-hash "$(cardano-cli transaction hash-script-data --script-data-file vesting.json)" \
    --change-address addr1v85teypffelqjaa92t6s363qhzcfkdplcfeh6e0pr9k48mc20wq09 \
    --protocol-params-file protocol-parameters.json \
    --out-file locking-tx-body.txt
```

A couple of things to point out. First, we are sending Ada to the address we created earlier, e.g. `vesting.addr`. Next, we included a required datum hash. It's a hash of the datum, which contains the beneficiary and vesting schedule. It looks like this

```
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "67614c1b06ddbb100cb6cbe919594cac31771c25530b6c7f28da242b"
    },
    {
      "list": [
        {
          "constructor": 0,
          "fields": [
            {
              "int": 1642283044000
            },
            {
              "map": [
                {
                  "v": {
                    "map": [
                      {
                        "v": {
                          "int": 1000000
                        },
                        "k": {
                          "bytes": ""
                        }
                      }
                    ]
                  },
                  "k": {
                    "bytes": ""
                  }
                }
              ]
            }
          ]
        },
        {
          "constructor": 0,
          "fields": [
            {
              "int": 1644961444000
            },
            {
              "map": [
                {
                  "v": {
                    "map": [
                      {
                        "v": {
                          "int": 1000000
                        },
                        "k": {
                          "bytes": ""
                        }
                      }
                    ]
                  },
                  "k": {
                    "bytes": ""
                  }
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}
```

This datum is for a vesting schedule for 2 Ada: the 1 Ada vests after one month and the second Ada vests after two months. It can be generated using the `vesting-sc` executable

```
$ vesting-sc datum \
  --beneficiary 67614c1b06ddbb100cb6cbe919594cac31771c25530b6c7f28da242b \
  --portion $(date -d '+1 month' '+%s'):1000000 \
  --portion $(date -d '+2 month' '+%s'):1000000
```

Here is an example of an unlocking transaction corresponding to the above locking transaction:

```bash
cardano-cli transaction build \
    --alonzo-era \
    --mainnet \
    --tx-in 2b81720f2cb268ff0827ba6f5858e7ce82e1fc4a14f4c8effcafa389acaad55b#1 \
    --tx-in-script-file vesting.plutus \
    --tx-in-datum-file vesting.json \
    --tx-in-redeemer-value redeemer.json \
    --tx-in bedfb6a1729598dc5af08d29ebf0e7b3c73a86db4a2dc5316fe6fd7873f64946#0 \
    --required-signer ~/keys/.skey \
    --tx-in-collateral bedfb6a1729598dc5af08d29ebf0e7b3c73a86db4a2dc5316fe6fd7873f64946#0 \
    --tx-out "addr1v8dg6hwygkphs4x0f3uwqx0jyywcarvhaquf0f2pzamf2ac7nzw0f + 1000000 lovelace" \
    --change-address addr1vyzwagqvqhd4q5swq67e60fm7dcrtcal4t96z0gea39zrgqjjtcvh \
    --protocol-params-file protocol-parameters.json \
    --out-file unlocking-tx-body.txt
```

A few things to note. We are passing in the same datum we hashed for the locking transaction, e.g. `--tx-in-datum-file vesting.json`. Another thing to note, we have an extra input UTxO to cover the transaction costs. Additionally, we are passing in a file for the redeemer. This file should contain

```
{"constructor":0,"fields":[]}
```

Similar example transactions can be found in the `scripts` folder, which is used for testing, as described below.

## Testing

Here's how to use these scripts:


## General

When in a shell, before running anything below, source the env vars for file for either mainnet or testnet, depending on which you're testing on.


For testnet

```
$ source scripts/envars/testnet-env.envvars
```

For mainnet

```
$ source scripts/envars/mainnet-env.envvars
```

The environment variable files set `CARDANO_NODE_SOCKET_PATH` to the path of the appropriate Daedalus socket file (either Testnet Daedalus or the regular mainnet Daedalus). It you run a `cardano-node` on your own you should set this environment variable to your socket file location after sourcing the environment variable file.

## Init (only done once)

First create the wallets, get the protocol parameters, compile the plutus, and create the script address

```
$ ./scripts/wallets/make-all-wallets.sh
$ ./scripts/query-protocol-parameters.sh
$ ./scripts/compile.sh
$ ./scripts/hash-script.sh
```

## Make sure the `sender` has funds

If you just created the wallets, find the sender address (it will be different then the example value below).

```
$ cat ~/$BLOCKCHAIN_PREFIX/sender.addr
addr_test1vz2wnmjhkvg6t59uh8q39svqq4cms6vdyha802apqwvstuq80a88a
```

If you're testing on the mainnet, you'll need to send some Ada to that address from your wallet (or have someone else send it).

If you're testing on the testnet, you can go to the faucet <https://testnets.cardano.org/en/testnets/cardano/tools/faucet/> and send Ada to that address.

Wait a bit and check that the funds are available

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96     0        1000000000 lovelace + TxOutDatumHashNone
```

If you don't see any transactions, wait a bit longer and try again.

Once we have wallets and the sender has funds, we're ready for testing.

## Test happy path

### Locking transaction

First, we need to create the locking transaction. Before we can do that, we need to find a UTxO, for the sender, that we can use.
We do that by running the `./scripts/query/sender.sh` script.

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96     0        1000000000 lovelace + TxOutDatumHashNone
```

From this, we can see there is a single transaction with a single output. The UTxO address is thus `6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96#0`.

Now we're ready to run the locking transaction.

```
$ ./scripts/happy-path/lock-tx.sh 6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96#0
```

Assuming you didn't see any errors, you can now look for the UTxO for the script (which is needed in the next step)

```
$ ./scripts/query/sc.sh
+++ cat scripts/testnet/vesting.addr
++ cardano-cli query utxo --address addr_test1wq952y8g67s3v5kwk3n5kqc2q5uzumkf7djrh5gfmz95lns7wuzrk --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     1        1724100 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5e9d8bac576e8604e7c3526025bc146f5fa178173e3a5592d122687bd785b520"
```

### Unlocking transaction

For creating the transaction to share the ada sent, we need two UTxOs: the first is the script UTxO, as found at the end of the previous section (`cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#1`).

The second is a UTxO to cover fees and collateral. We use the same UTxO for both. Find a UTxO to use from the sender

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     0        998110179 lovelace + TxOutDatumHashNone
```

Given the above, we'll use the UTxO `cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#0` in this example.

Now we can create the revenue sharing transaction which runs the validator

```
$ ./scripts/happy-path/share-tx.sh cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#1 cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#0
```

Assuming everything went well, and you give it a little time, you should be able to query the script address (`./scripts/query/sc.sh`) and see the UTxO you used is gone. You should also be able to check the other addresses have received the funds

```
$ ./scripts/query/fifty.sh && ./scripts/query/thirtythreethree.sh && ./scripts/query/sixteenseven.sh
++ scripts/query/find-utxo.sh fifty
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9d3d154d005054894695b08e97f08ef9e065a87d6cb3742e9d5cba355d4afecc     1        3448200 lovelace + TxOutDatumNone
++ scripts/query/find-utxo.sh thirtythreethree
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9d3d154d005054894695b08e97f08ef9e065a87d6cb3742e9d5cba355d4afecc     2        2296501 lovelace + TxOutDatumNone
++ scripts/query/find-utxo.sh sixteenseven
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9d3d154d005054894695b08e97f08ef9e065a87d6cb3742e9d5cba355d4afecc     3        1151698 lovelace + TxOutDatumNone
```

## Negative Test

To demonstrate that the validator fails when the split is invalid follow the instructions for locking funds above.

After the funds are at the script address query the senders address for a UTxO to cover the fees:

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     0        998110179 lovelace + TxOutDatumHashNone
```

Next, find the UTxO for the funds at the script address:

```
$ ./scripts/query/sc.sh
+++ cat scripts/testnet/vesting.addr
++ cardano-cli query utxo --address addr_test1wq952y8g67s3v5kwk3n5kqc2q5uzumkf7djrh5gfmz95lns7wuzrk --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     1        1724100 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5e9d8bac576e8604e7c3526025bc146f5fa178173e3a5592d122687bd785b520"
```

Finally execute the negative, which should fail:

```
./scripts/failure-cases/invalid-split.sh cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#1 cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#0
...
...
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 1 (in the order of the TxIds) failed with:
The Plutus script evaluation failed: An error has occurred:  User error:
The provided Plutus code called 'error'.
Script debugging logs: Not all addresses were paid the correct amount
PT5
```
