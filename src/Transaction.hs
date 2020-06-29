{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}

module Transaction(
    Transaction
    , createTransaction
    , sender
    , receiver
    , amount
) where

import Data.Aeson
import GHC.Generics

-- | Data structure to store transaction data
data Transaction = Trans {
        sender :: String      -- ^ Sender's ID which should be the sender's public key
    , receiver :: String      -- ^ Receiver's ID which should be the receivers's public key
    , amount :: Int           -- ^ Amount to be transferred
    } deriving (Show, Eq, Generic)

-- | Creates a new Transaction from sender, receiver and amount to be transferred. 
createTransaction :: String -> String -> Int -> Transaction
createTransaction sender receiver amount
    | (sender==receiver) = error "sender and receiver cannot be the same"
    | (amount<0) = error "amount cannot be negative"
    | otherwise = (Trans {sender=sender, receiver=receiver, amount=amount})

-- The parser is autoderived
instance FromJSON Transaction

-- The encoder needs to be implemented to preserve the order 
instance ToJSON Transaction where
  toEncoding trans =
    pairs $ "sender"   .= sender trans
         <> "receiver" .= receiver trans
         <> "amount"   .= amount trans

