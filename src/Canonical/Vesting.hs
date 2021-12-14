{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Canonical.Vesting
  ( vesting
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger hiding (Datum, singleton)
import qualified Ledger.Contexts as Validation
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Ledger.Ada hiding (divide)

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
to the beneficiary. The benefactor can lock more value in the script or
the beneficiary will have to wait until a later vesting deadline. 100%
of the value will be accessible after all deadlines have passed.
-}
mkValidator :: Datum -> () -> ScriptContext -> Bool
mkValidator datum _ ctx =
  traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
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
