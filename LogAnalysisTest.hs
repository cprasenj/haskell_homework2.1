module LogAnalysisTest where
  import Test.Hspec
  import Test.QuickCheck
  import Control.Exception (evaluate)
  import LogAnalysis
  import Log

  main :: IO ()
  main = hspec $ do
    let fileString =
                  "I 6 Completed armadillo processing\n" ++
                  "I 1 Nothing to report\n" ++
                  "E 99 10 Flange failed!\n" ++
                  "I 4 Everything normal\n" ++
                  "I 11 Initiating self-destruct sequence\n" ++
                  "E 70 3 Way too many pickles\n" ++
                  "E 65 8 Bad pickle-flange interaction detected\n" ++
                  "W 5 Flange is due for a check-up\n" ++
                  "I 7 Out for lunch, back in two time steps\n" ++
                  "E 20 2 Too many pickles\n" ++
                  "I 9 Back from lunch"

    describe "parseMessage" $ do
      it "should return   LogMessage (Error 2) 562 help help for E 2 562 help help" $ do
        parseMessage "E 2 562 help help" `shouldBe`  LogMessage (Error 2) 562 "help help"

      it "should return LogMessage Info 29 la la la for I 29 la la la" $ do
        parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

      it "should return Unknown This is not in the right format for This is not in the right format" $ do
        parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

      it "should return Warning 5 Flange is due for a check-up for W 5 Flange is due for a check-up" $ do
        parseMessage "W 5 Flange is due for a check-up" `shouldBe` LogMessage Warning 5 "Flange is due for a check-up"

    describe "parse" $ do

      it "should create Logmessage for the given string" $ do
        parse "E 2 562 help help\nI 29 la la la" `shouldBe` [LogMessage (Error 2) 562 "help help", LogMessage Info 29 "la la la"]

    describe "insertInto" $ do
      it "should create a new tree when given a empty tree and a Logmessage" $ do
        insertInto Leaf (LogMessage (Error 2) 562 "help help") `shouldBe` (Node Leaf (LogMessage (Error 2) 562 "help help") Leaf)

      it "should return the tree if it is given an unknown" $ do
        insertInto Leaf (Unknown "This is not in the right format") `shouldBe` Leaf

      it "should insert logmessage in the left subtree if the node timestamp is greater than that of the new logmessage" $ do
        insertInto (Node Leaf (LogMessage (Error 2) 562 "help help") Leaf) (LogMessage Info 540 "All is well")
          `shouldBe` (Node (Node Leaf (LogMessage Info 540 "All is well") Leaf) (LogMessage (Error 2) 562 "help help") Leaf)

      it "should insert logmessage in the right subtree if the node timestamp is less than that of the new logmessage" $ do
        insertInto (Node Leaf (LogMessage (Error 2) 562 "help help") Leaf) (LogMessage Info 999 "All is well")
          `shouldBe` (Node Leaf (LogMessage (Error 2) 562 "help help") (Node Leaf (LogMessage Info 999 "All is well") Leaf))

      describe "build" $ do
        it "should give Leaf for empty list" $ do
          build [] `shouldBe` Leaf

        it "should build the tree with the given list of log messages" $ do
          build [LogMessage (Error 2) 562 "help help", LogMessage Info 999 "All is well"]
          `shouldBe` (Node Leaf (LogMessage (Error 2) 562 "help help") (Node Leaf (LogMessage Info 999 "All is well") Leaf))

      describe "inOrder" $ do
        it "should traverse the tree and give a list of log messages" $ do
          inOrder (Node Leaf (LogMessage (Error 2) 562 "help help") (Node Leaf (LogMessage Info 999 "All is well") Leaf))
          `shouldBe` [LogMessage (Error 2) 562 "help help", LogMessage Info 999 "All is well"]

      describe "whatWentWrong" $ do
        it "should give the relevant log messages from a list of message" $ do
          whatWentWrong (parse fileString) `shouldBe` ["Flange failed!", "Way too many pickles", "Bad pickle-flange interaction detected"]
