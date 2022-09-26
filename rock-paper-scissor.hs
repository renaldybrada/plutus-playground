{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week04.Vesting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Data.Char            as Char
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

data LockDatum = LockDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    , lockRps     :: Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''LockDatum

{-# INLINABLE mkValidator #-}
mkValidator :: LockDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

data SimpleRps
instance Scripts.ValidatorTypes SimpleRps where
    type instance DatumType SimpleRps = LockDatum
    type instance RedeemerType SimpleRps = ()

typedValidator :: Scripts.TypedValidator SimpleRps
typedValidator = Scripts.mkTypedValidator @SimpleRps
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @LockDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data LockParams = LockParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    , gpRps         :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data ChallengeParams = ChallengeParams
    { cAmount      :: !Integer
    , cRps         :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SimpleRpsSchema =
            Endpoint "1.lock" LockParams
        .\/ Endpoint "2.challenge" ChallengeParams
        .\/ Endpoint "3.grab" ()

lock :: AsContractError e => LockParams -> Contract w s e ()
lock gp = do
    let dat = LockDatum
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                , lockRps     = gpRps gp
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "lock a challenge with %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

challenge :: forall w s e. AsContractError e => ChallengeParams -> Contract w s e ()
challenge cParams = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
    if Map.null utxos
        then logInfo @String $ "no challenge available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                        Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                        Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "you win!"
  where
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> beneficiary d == pkh && deadline d <= now && isChallengerWin (lockRps d) (cRps cParams)

-- Rock Paper Scissor Logic
-- 1 = Rock
-- 2 = Paper
-- 3 = Scissor
-- this function shows whether the challenger wins
-- draw is considered a loss, so it will return false when draw
isChallengerWin :: Integer -> Integer -> Bool
isChallengerWin lockInt cInt = 
    if lockInt == 1 then -- ex : if lock == rock and challenger == paper  
        if cInt == 2 then True else False -- then challenger win, otherwise the challenger loss even when it's draw
    else if lockInt == 2 then
        if cInt == 3 then True else False
    else if lockInt == 3 then
        if cInt == 1 then True else False
    else False

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable pkh) <$> utxosAt scrAddress
    if Map.null utxos
        then logInfo @String $ "no utxo available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                        Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "grab available fund"
  where
    isSuitable :: PaymentPubKeyHash -> ChainIndexTxOut -> Bool
    isSuitable pkh o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> beneficiary d == pkh

endpoints :: Contract () SimpleRpsSchema Text ()
endpoints = awaitPromise (lock' `select` challenge') >> endpoints
  where
    lock' = endpoint @"1.lock" lock
    challenge' = endpoint @"2.challenge" challenge

mkSchemaDefinitions ''SimpleRpsSchema

mkKnownCurrencies []