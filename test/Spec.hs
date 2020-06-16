import Test.Hspec
import Control.Exception.Base

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
proof1 = "1298457"
blockHdr1 = createBlockHeader index1 timestamp1 prevHash1 proof1
blockHdr_NG_1 = createBlockHeader (-1) timestamp1 prevHash1 proof1
blockHdr_NG_2 = createBlockHeader index1 (-1) prevHash1 proof1

bc1 = createGenesis
bc1_blockHdr1 = createBlockHeader 1 0 "100" "1"  -- genesis block 
(bc1_transaction1, bc1_nextIndex) = requestNewTransaction bc1 transaction1
bc2 = addNewBlock bc1_transaction1 timestamp1 prevHash1 proof1
bc2_blockHdr = blockHdr1

index2 = 3
timestamp2 = 200000
prevHash2 = "9ka7HkelAnb4is"
proof2 = "9999333"
(bc2_transaction1, bc2_nextIndex1) = requestNewTransaction bc2 transaction1
(bc2_transaction2, bc2_nextIndex2) = requestNewTransaction bc2_transaction1 transaction2
bc3 = addNewBlock bc2_transaction2 timestamp2 prevHash2 proof2
bc3_blockHdr = createBlockHeader index2 timestamp2 prevHash2 proof2

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

