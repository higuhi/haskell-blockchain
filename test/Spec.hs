import Test.Hspec
import Control.Exception.Base
import Data.Aeson
import Crypto.Hash (hash, SHA256, Digest)
import Data.ByteString.Lazy (toStrict) 

import Blockchain
import Transaction
import BlockHeader

user1 = "0001" 
user2 = "123abc"
user3 = "1s2h"

transaction1 = createTransaction user1 user2 1
transaction2 = createTransaction user2 user3 100
transaction_NG_1 = createTransaction user2 user3 (-5)
transaction_NG_2 = createTransaction user2 user2 10

index1 = 2
timestamp1 = 100000
prevHash1 = "hkE2k42fkVnd4kjK6"
proof1 = 1298457
blockHdr1 = createBlockHeader index1 timestamp1 prevHash1 proof1 
blockHdr_NG_1 = createBlockHeader (-1) timestamp1 prevHash1 proof1 
blockHdr_NG_2 = createBlockHeader index1 (-1) prevHash1 proof1 

bc1 = createGenesis
bc1_blockHdr1 = createBlockHeader 1 0 "100" 1  -- genesis block 
bc1_blockHdr1_Hash = show(hash (toStrict (encode bc1_blockHdr1)) :: Digest SHA256)

(bc1_transaction1, bc1_nextIndex) = requestNewTransaction bc1 transaction1
bc2 = addNewBlock bc1_transaction1 timestamp1 prevHash1 proof1  
bc2_blockHdr = blockHdr1
bc2_blockHdr_Hash = show(hash (toStrict (encode bc2_blockHdr)) :: Digest SHA256)

index2 = 3
timestamp2 = 200000
prevHash2 = "9ka7HkelAnb4is"
proof2 = 9999333
(bc2_transaction1, bc2_nextIndex1) = requestNewTransaction bc2 transaction1
(bc2_transaction2, bc2_nextIndex2) = requestNewTransaction bc2_transaction1 transaction2
bc3 = addNewBlock bc2_transaction2 timestamp2 prevHash2 proof2 
bc3_blockHdr = createBlockHeader index2 timestamp2 prevHash2 proof2 
bc3_blockHdr_Hash = show(hash (toStrict (encode bc3_blockHdr)) :: Digest SHA256)

b1 = createGenesis
(b1_t1, b1_next) = requestNewTransaction b1 transaction1
b2 = mineBlock b1_t1 timestamp1
(b2_t1, b2_next1) = requestNewTransaction b2 transaction1
(b2_t2, b2_next2) = requestNewTransaction b2_t1 transaction2
b3 = mineBlock b2_t2 timestamp2 
b3_fake1 = addNewBlock b2_t2 timestamp2 (getBlockHash (fst (getLastBlock b2))) proof2 
b3_fake2 = addNewBlock b2_t2 timestamp2  prevHash2 (proof (fst (getLastBlock b2)))
b3_fake3 = addNewBlock b2_t2 timestamp1 (getBlockHash (fst (getLastBlock b2))) (proof (fst (getLastBlock b2))) 


main :: IO ()
main = hspec $ do
    describe "Test Transaction" $ do
        it "returns the sender's name" $
            sender transaction1 `shouldBe` user1

        it "returns the receiver's name" $
            receiver transaction1 `shouldBe` user2

        it "returns the amount" $
            amount transaction1 `shouldBe` 1

        it "returns the sender's name" $
            sender transaction2 `shouldBe` user2

        it "returns the receiver's name" $
            receiver transaction2 `shouldBe` user3

        it "returns the amount" $
            amount transaction2 `shouldBe` 100

        it "checks error for negative amount" $
            evaluate (transaction_NG_1) `shouldThrow` anyErrorCall 

        it "checks error for having sender and reciver the same" $
            evaluate (transaction_NG_2) `shouldThrow` anyErrorCall

    describe "Test BlockHeader" $ do
        it "returns the sender's name" $
            index blockHdr1 `shouldBe` index1

        it "returns the receiver's name" $
            timestamp blockHdr1 `shouldBe` timestamp1

        it "returns the amount" $
            proof blockHdr1 `shouldBe` proof1

        it "returns the sender's name" $
            prevHash blockHdr1 `shouldBe` prevHash1

        it "checks error for negative amount" $
            evaluate (blockHdr_NG_1) `shouldThrow` anyErrorCall 

        it "checks error for having sender and reciver the same" $
            evaluate (blockHdr_NG_2) `shouldThrow` anyErrorCall

    describe "Test BlockChain" $ do
        it "checks the gensis block" $
            getLastBlock bc1 `shouldBe` (bc1_blockHdr1, [])

        it "returns the length of the gensis block" $
            getLength bc1 `shouldBe` 1

        it "checks the last block" $
            getLastBlock bc2 `shouldBe` (bc2_blockHdr, [transaction1])

        it "returns the length of the block to be 2" $
            getLength bc2 `shouldBe` 2

        it "the third block will have two transactions" $
            getLastBlock bc3 `shouldBe` (bc3_blockHdr, [transaction1, transaction2])

        it "returns the length of the block to be 3" $
            getLength bc3 `shouldBe` 3

    describe "Test Block Header Hash " $ do

        it "Test bc1 BlockHash" $
            getBlockHash bc1_blockHdr1 `shouldBe` bc1_blockHdr1_Hash

        it "Test bc2 BlockHash" $
            getBlockHash bc2_blockHdr `shouldBe` bc2_blockHdr_Hash

        it "Test bc3 BlockHash" $
            getBlockHash bc3_blockHdr `shouldBe` bc3_blockHdr_Hash

    describe "Test chain of blocks and proof of work" $ do

        it "Test b2's previous hash" $
            (prevHash (fst (getLastBlock b2))) `shouldBe` (getBlockHash (fst (getLastBlock b1)))

        it "Test b3's previous hash" $
            (prevHash (fst (getLastBlock b3))) `shouldBe` (getBlockHash (fst (getLastBlock b2)))

        it "Test b2's block header" $
            (isValidBlockHeader (fst (getLastBlock b2)) hashCashPrefix) `shouldBe` True

        it "Test b3's block header" $
            (isValidBlockHeader (fst (getLastBlock b3)) hashCashPrefix) `shouldBe` True

        it "Test fake b3 (proof is forged)" $
            (isValidBlockHeader (fst (getLastBlock b3_fake1)) hashCashPrefix) `shouldBe` False

        it "Test fake b3 (prevHash is forged)" $
            (isValidBlockHeader (fst (getLastBlock b3_fake2)) hashCashPrefix) `shouldBe` False

        it "Test fake b3 (timestamp is forged)" $
            (isValidBlockHeader (fst (getLastBlock b3_fake3)) hashCashPrefix) `shouldBe` False

        it "Test blockchain vaidity of b1" $
            (isValidChain b1) `shouldBe` True

        it "Test blockchain vaidity of b2" $
            (isValidChain b2) `shouldBe` True

        it "Test blockchain vaidity of b3" $
            (isValidChain b3) `shouldBe` True

        it "Test blockchain vaidity of fake b3 (proof is forged) " $
            (isValidChain b3_fake1) `shouldBe` False

        it "Test blockchain vaidity of fake b3 (prevHash is forged)" $
            (isValidChain b3_fake2) `shouldBe` False

        it "Test blockchain vaidity of fake b3 (timestamp is forged)" $
            (isValidChain b3_fake3) `shouldBe` False