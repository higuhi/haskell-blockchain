{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}

module Blockchain (
    Blockchain
    , Block
    , createGenesis
    , addNewBlock
    , mineBlock
    , hashCashPrefix
    , requestNewTransaction
    , isValidChain
    , getLength
    , getLastBlock
) where

import Data.Aeson
import GHC.Generics
import Debug.Trace
import Transaction
import BlockHeader

-- | Block consists of BlockHeader and a list of Transactions
type Block = (BlockHeader, [Transaction])

-- | A blockchain data has a pool of transactions to be processed 
-- | and chained blocks of finalized transactions. 
data Blockchain = BC {
    pool :: [Transaction]   -- ^ a pool of transactions to be processed
    , blocks :: [Block]     -- ^ a chain of blocks which consist of BlockHeader and a list of Transactions
    } deriving (Show, Eq, Generic)


-- | Hashcash target used in this blockchain. The proof in the block header must 
-- | be correct that block header hash is going to start with this hashCashPrefix
hashCashPrefix :: String
hashCashPrefix = "0000"

-- | Creates a genesis block
-- | Timestamp will be 0 indicating this is a genesis block, and 
-- | hash of the privious block and proof of work are arbitrarily chosen.  
createGenesis :: Blockchain
createGenesis = addNewBlock (BC {pool=[], blocks=[]}) 0 "100" 1

-- | Adds a transaction to the pool of transactions to be processed.
-- | This function will return a Blockchain with the transaction in the pool 
-- | and the block index number that the transaction will be finalized. 
-- | Note that transactions are just pooled and are not finalized yet. 
requestNewTransaction :: Blockchain -> Transaction -> (Blockchain, Int)
requestNewTransaction (BC {blocks=[]}) _ = error "Cannot add transaction to an empty blockchain"
requestNewTransaction bc trans = (newBc, nextIndex)
    where
        newBc = BC {pool=newPool, blocks=blks} 
        newPool = (pool bc) ++ [trans]
        blks = blocks bc
        nextIndex = (index (fst (last blks))) + 1 

-- | Creates a new block from timestamp, hash of the previous block, 
-- | proof of work and transactions in the pool. 
-- | The block will be added to the end of chain and transaction pool 
-- | will be cleared. (This function is for testing purpose and should not be used)
addNewBlock :: Blockchain -> Int -> String -> Int -> Blockchain
addNewBlock bc timestamp prevHash proof = (BC {pool=[], blocks=(blks ++ [newBlk])})
    where
        blks = blocks bc
        newBlkHdr = createBlockHeader ((length blks)+1) timestamp prevHash proof
        newBlk = (newBlkHdr, (pool bc))

-- | Creates a new block from timestamp and mines it to make a valid block. 
-- | The returned blockchain is always one block longer than the input blockchain. 
mineBlock :: Blockchain -> Int -> Blockchain
mineBlock bc timestamp = (BC {pool=[], blocks=(blks ++ [newBlk])})
    where
        blks = blocks bc
        lastBlkHdr = fst (getLastBlock bc)
        newBlkHdr = workForProof (createBlockHeader ((length blks)+1) timestamp (getBlockHash lastBlkHdr) 0) hashCashPrefix
        newBlk = (newBlkHdr, (pool bc))

-- | Checks if the given blockchain is valid 
isValidChain :: Blockchain -> Bool 
isValidChain bc = isValidBlocks (blocks bc)

-- | Checks if the given list of blocks are vaid by checking that 
-- | the block has correct block header hash from the previous block and
-- | the block header hash of the block has valid hash cash prefix. 
isValidBlocks :: [Block] -> Bool
isValidBlocks [] = False        -- blocks should not be empty 
isValidBlocks (blk:[]) = True   -- genesis block is always valid
isValidBlocks (prevBlock:currentBlock:rest) 
    = hashCheck && proofCheck && (isValidBlocks (currentBlock:rest))
    where
        hashCheck = getBlockHash (fst prevBlock) == prevHash (fst currentBlock)
        proofCheck = isValidBlockHeader (fst currentBlock) hashCashPrefix

-- | Returns the length of this blockchain 
getLength :: Blockchain -> Int
getLength bc = length (blocks bc)

-- | Retrieves the last block in the chain
getLastBlock :: Blockchain -> Block
getLastBlock (BC {blocks=[]}) = error "Cannot retrieve the last block from an empty blockchain"
getLastBlock bc = last (blocks bc)

-- The parser is autoderived
instance FromJSON Blockchain

-- The encoder needs to be implemented to preserve the order 
instance ToJSON Blockchain where
  toEncoding bc =
    pairs $ "blocks" .= blocks bc
         <> "pool"   .= pool bc