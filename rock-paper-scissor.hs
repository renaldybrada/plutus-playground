-- A simple Rock Paper Scissor game. This script adapted and extended from plutus guess game.
-- This simple game only accept string input rock/paper/scissor. If player 1 or player 2 input others, the fund stay locked on utxo.
-- Just like a typical rock paper scissor game, player or wallet 1 lock RPS word and some amount.
-- player or wallet 2 try to challenge from player 1. 
-- If player 2 win (for example : player 1 lock 'paper' and player 2 challenge with 'scissor') then the fund goes to challenger (player 2)
-- If it isn't (either challenger lose or draw), the fund stay locked on utxo.

-- please visit https://github.com/renaldybrada/plutus-playground 
-- for more information about simulation 

import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Ledger (Address, Datum (Datum), ScriptContext, Validator, Value)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Typed.Scripts qualified as Scripts
import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (pure, (<$>))
import Prelude qualified as Haskell

------------------------------------------------------------

newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

type GameSchema =
        Endpoint "1. lock" LockParams
        .\/ Endpoint "2. challenge" ChallengeParams

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateChallenge ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @HashedString @ClearString

-- create a data script for the rock paper scissor game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for the rock paper scissor game by lifting the
-- string to its on-chain representation
clearString :: Haskell.String -> ClearString
clearString = ClearString . toBuiltin . C.pack

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
validateChallenge :: HashedString -> ClearString -> ScriptContext -> Bool
validateChallenge hs cs _ = isChallengerWin hs cs

-- Rock Paper Scissor Logic
-- this function shows whether the challenger wins.
-- Draw is considered a loss, so it will return false when it is draw.
-- As you can see, `actual` and `challenge'` only accept hashed word from rock / paper / scissor.
-- When the player input other than that, it will return false means that the fund stay locked 
isChallengerWin :: HashedString -> ClearString -> Bool
isChallengerWin (HashedString actual) (ClearString challenge') = 
    if actual == sha2_256 "rock" then
        if challenge' == "paper" then True else False
    else if actual == sha2_256 "paper" then
        if challenge' == "scissor" then True else False
    else if actual == sha2_256 "scissor" then
        if challenge' == "rock" then True else False
    else False

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

-- | The address of the game (the hash of its validator script)
gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { lockRpsWord :: Haskell.String
    , amount     :: Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "challenge" endpoint
newtype ChallengeParams = ChallengeParams
    { challengeWord :: Haskell.String
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | The "lock" contract endpoint
lock :: AsContractError e => Promise () GameSchema e ()
lock = endpoint @"1. lock" @LockParams $ \(LockParams lockRps amt) -> do
    logInfo @Haskell.String $ "Pay " <> Haskell.show amt <> " to the script"
    let tx         = Constraints.mustPayToTheScript (hashString lockRps) amt
    void (submitTxConstraints gameInstance tx)

-- | The "challenge" contract endpoint
challenge :: AsContractError e => Promise () GameSchema e ()
challenge = endpoint @"2. challenge" @ChallengeParams $ \(ChallengeParams theChallenge) -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)

    let redeemer = clearString theChallenge
        tx       = collectFromScript utxos redeemer

    let hashedLockRps = findLockRpsValue utxos
        isChallengerWon = fmap (`isChallengerWin` redeemer) hashedLockRps == Just True
    if isChallengerWon
        then do 
            logWarn @Haskell.String "Ha! You win!"
            logInfo @Haskell.String "Submitting transaction to challenger"
            void (submitTxConstraintsSpending gameInstance utxos tx)
        else logWarn @Haskell.String "You lose !"
    

-- | Find the rock paper scissor word in the Datum of the UTxOs
findLockRpsValue :: Map TxOutRef ChainIndexTxOut -> Maybe HashedString
findLockRpsValue =
  listToMaybe . catMaybes . Map.elems . Map.map lockRpsValue

-- | Extract the lock rock paper scissor word in the Datum of a given transaction output is possible
lockRpsValue :: ChainIndexTxOut -> Maybe HashedString
lockRpsValue o = do
  Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
  PlutusTx.fromBuiltinData d

game :: AsContractError e => Contract () GameSchema e ()
game = do
    logInfo @Haskell.String "Waiting for challenge or lock endpoint..."
    selectList [lock, challenge]

endpoints :: AsContractError e => Contract () GameSchema e ()
endpoints = game

mkSchemaDefinitions ''GameSchema

$(mkKnownCurrencies [])
