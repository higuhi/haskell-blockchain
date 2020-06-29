{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}

module BlockHeader (
    BlockHeader
    , createBlockHeader
    , getBlockHash
    , isValidBlockHeader
    , workForProof
    , index
    , timestamp
    , prevHash
    , proof
) where

import Data.Aeson
import GHC.Generics
import Crypto.Hash (hash, SHA256, Digest)
import Data.ByteString.Lazy (toStrict) 

-- | Data structure for a Block header
data BlockHeader = BlkHdr {
        index :: Int        -- ^ index of the block in the chain
    , timestamp :: Int      -- ^ timestamp of the block creation
    , prevHash :: String    -- ^ hash of the preivous block 
    , proof :: Int          -- ^ the proof of work for this blcok
    } deriving (Show, Eq, Generic)

-- | Creates a new Block from index, timestamp, hash of the previous block and proof of work
createBlockHeader :: Int -> Int -> String -> Int -> BlockHeader
createBlockHeader index timestamp prevHash proof
    | (index<0) = error "index cannot be negative"
    | (timestamp<0) = error "timestamp cannot be negative"
    | otherwise = (BlkHdr {index=index, timestamp=timestamp, prevHash=prevHash, proof=proof})

-- | Returns the block header hash (aka Block ID) using SHA256
getBlockHash :: BlockHeader -> String
getBlockHash blkHdr = show (hash (toStrict (encode blkHdr)) :: Digest SHA256)

-- | Verify the block header hash using the given hash cash prefix
-- | This function returns true only if the block header hash starts with 
-- | the hash cash prefix.
isValidBlockHeader :: BlockHeader -> String -> Bool
isValidBlockHeader blkHdr hashCashPrefix = (take (length hashCashPrefix) (getBlockHash blkHdr)) == hashCashPrefix

-- | Finds a proof that makes valid block header hash and returns the block header
workForProof :: BlockHeader -> String -> BlockHeader
workForProof blkHdr hashCashPrefix
    | isValidBlockHeader blkHdr hashCashPrefix = blkHdr
    | otherwise = workForProof next_blkHdr hashCashPrefix
    where 
        next_blkHdr = blkHdr {proof=(proof blkHdr)+1}

-- The parser is autoderived
instance FromJSON BlockHeader

-- The encoder needs to be implemented to preserve the order 
instance ToJSON BlockHeader where
  toEncoding blkHdr =
    pairs $ "index"     .= index blkHdr
         <> "timestamp" .= timestamp blkHdr
         <> "prevHash"  .= prevHash blkHdr
         <> "proof"     .= proof blkHdr         