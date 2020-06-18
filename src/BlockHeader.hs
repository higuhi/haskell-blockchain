{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}

module BlockHeader (
    BlockHeader
    , createBlockHeader
    , index
    , timestamp
    , prevHash
    , proof
) where

import Data.Aeson
import GHC.Generics

-- | Data structure for a Block header
data BlockHeader = BlkHdr {
        index :: Int                  -- ^ index of the block in the chain
    , timestamp :: Int                -- ^ timestamp of the block creation
    , prevHash :: String              -- ^ hash of the preivous block 
    , proof :: String                 -- ^ the proof of work for this blcok
    } deriving (Show, Eq, Generic)

-- | Creates a new Block from index, timestamp, hash of the previous block and proof of work
createBlockHeader :: Int -> Int -> String -> String -> BlockHeader
createBlockHeader index timestamp prevHash proof
    | (index<0) = error "index cannot be negative"
    | (timestamp<0) = error "timestamp cannot be negative"
    | otherwise = (BlkHdr {index=index, timestamp=timestamp, prevHash=prevHash, proof=proof})

-- The parser is autoderived
instance FromJSON BlockHeader

-- The encoder needs to be implemented to preserve the order 
instance ToJSON BlockHeader where
  toEncoding blkHdr =
    pairs $ "index"     .= index blkHdr
         <> "timestamp" .= timestamp blkHdr
         <> "prevHash"  .= prevHash blkHdr
         <> "proof"     .= proof blkHdr         