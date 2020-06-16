module Blockchain (
    Blockchain
    , Block
    , createGenesis
    , addNewBlock
    , requestNewTransaction
    , getLength
    , getLastBlock
) where

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
    } deriving (Show, Eq)

-- | Creates a genesis block
-- | Timestamp will be 0 indicating this is a genesis block, and 
-- | hash of the privious block and proof of work are arbitrarily chosen.  
createGenesis :: Blockchain
createGenesis = addNewBlock (BC {pool=[], blocks=[]}) 0 "100" "1"


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
-- | will be cleared. 
addNewBlock :: Blockchain -> Int -> String -> String -> Blockchain
addNewBlock bc timestamp prevHash proof = (BC {pool=[], blocks=(blks ++ [newBlk])})
    where
        blks = blocks bc
        newBlkHdr = createBlockHeader ((length blks)+1) timestamp prevHash proof
        newBlk = (newBlkHdr, (pool bc))

-- | Returns the length of this blockchain 
getLength :: Blockchain -> Int
getLength bc = length (blocks bc)

-- | Retrieves the last block in the chain
getLastBlock :: Blockchain -> Block
getLastBlock (BC {blocks=[]}) = error "Cannot retrieve the last block from an empty blockchain"
getLastBlock bc = last (blocks bc)

