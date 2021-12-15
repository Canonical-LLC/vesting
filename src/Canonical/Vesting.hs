{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Canonical.Vesting
  ( vesting
  , Datum(..)
  , Portion(..)
  , Schedule
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger hiding (Datum, singleton)
import qualified Ledger.Contexts as Validation
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import Plutus.V1.Ledger.Credential
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Portion = Portion
  { deadline :: POSIXTime
  , amount :: Value
  }

PlutusTx.unstableMakeIsData ''Portion

type Schedule = [Portion]

data Datum = Datum
  { beneficiary :: PubKeyHash
  , schedule :: Schedule
  }

PlutusTx.unstableMakeIsData ''Datum

-------------------------------------------------------------------------------
{-

Batch Transaction Exploit Protection

If multiple script inputs were allowed in a single transaction, it would be
possible for a beneficiary to take an unvested amount from one vesting schedule
by combining it with a vested schedule and having the script output satisfy the
terms of both vesting schedules.

As an example, take the following vesting schedules, which both go to the same beneficiary:
  * in vesting schedule A, 10 Ada vests at 2 months and another 10 Ada vests at 4 months
  * in vesting schedule B, 10 Ada vests at 1 month and another 10 Ada vests at 2 months

If the beneficiary does not unlock any value from B until after 3 months, they can construct
a transaction that allows them to unlock 20 Ada from schedule A and zero from
schedule B. There would still be 20 Ada left locked in the script, which would satisfy
the validator for schedules A and B. The beneficiary could then create a second
transaction to unlock the value from schedule B.
-}
-------------------------------------------------------------------------------
{-# INLINABLE isScriptAddress #-}
isScriptAddress :: Address -> Bool
isScriptAddress Address { addressCredential } = case addressCredential of
  ScriptCredential _ -> True
  _ -> False

-- Verify that there is only one script input and get it's value.
{-# INLINABLE onlyOneScriptInput #-}
onlyOneScriptInput :: TxInfo -> Bool
onlyOneScriptInput info =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput = isScriptAddress . txOutAddress . txInInfoResolved

  in case filter isScriptInput . txInfoInputs $ info of
    [_] -> True
    _ ->  False


-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------
{-
This is a validator for a vesting contract. It is configured with a beneficiary
and a vesting schedule, a list of deadlines and the amount available to the
beneficiary after the deadline has passed.

At any point, the beneficiary can unlock any amount that has vested.
They are free to send the value to any address they choose.
If they only unlock a part of what's vested, they can create another
transaction to unlock the remaining value. That also means when
we're 100% vested, there can still be some value locked in the script,
but the beneficiary can create another transcation to unlock that value.

Caveat: If the benefactor puts in too little value to begin with,
when the first vesting deadline (and possibly later deadlines)
is reached, the full value (or even a portion of it) may not be accessible
to the beneficiary. The beneficiary will have to wait until a later vesting
deadline. 100% of the value will always be accessible after all deadlines have passed.
-}
mkValidator :: Datum -> () -> ScriptContext -> Bool
mkValidator datum _ ctx =
  traceIfFalse "expected exactly one script input" (onlyOneScriptInput info)
    && traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
    && traceIfFalse "Not enough value remains locked to fulfill vesting schedule" outputValid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info . beneficiary $ datum

    locked :: Value
    locked = Validation.valueLockedBy info (Validation.ownHash ctx)

    -- vested portions are the ones that the deadline is before
    -- the time the transaction is valid in
    isVested :: Portion -> Bool
    isVested = (`before` txInfoValidRange info) . deadline

    unvested :: Value
    unvested = mconcat . fmap amount . filter (not . isVested) . schedule $ datum

    -- Make sure there is enough still locked in the script
    -- to satisfy the remainder of unvested portions to be fulfilled.
    --
    outputValid :: Bool
    outputValid = locked `Value.geq` unvested


-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = Datum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Datum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

-------------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------------
vesting :: PlutusScript PlutusScriptV1
vesting
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  $ validator
