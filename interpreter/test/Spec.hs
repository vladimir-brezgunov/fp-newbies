import Control.Monad                (foldM)
import SimpleInteractiveInterpreter
import Test.Hspec
import Test.HUnit

main :: IO ()
main = hspec $ do
  let int0 = newInterpreter
      assert0 = assertDouble_ int0
      assertError0_ = assertError_ int0

  it "Constants" $ do
    assert0 "1.0" 1
    assert0 "1" 1
    assertError0_ "1abc"

  it "Basic arithmetic" $ do
    assertDouble_ int0 "1+1" 2
    assertDouble_ int0 "2-1" 1
    assertDouble_ int0 "2*3" 6
    assertDouble_ int0 "8 / 4" 2
    assertDouble_ int0 "16 - 8/4 +   10" 24
    assertDouble_ int0 "(16 - 8)/4 +   10" 12
    --assertDouble_ int0 "7 % 4" 3

  it "canParseVars" $ do
    parseExpr "x = 0" `shouldBe` Right (Assign "x" (Const 0))
    parseExpr "x = y = 0" `shouldBe` Right (Assign "x" (Assign "y" (Const 0)))
    parseExpr "x = 1 + (y = 2)" `shouldBe` Right (Assign "x" (BinaryOp Add (Const 1) (Assign "y" (Const 2))))


  it "Variables" $ do
    int1 <- assertDouble int0 "x = 1" 1
    assertDouble_ int1 "x" 1
    assertDouble_ int1 "x + 3" 4
    assertError_ int1 "y"
{-
  it "Functions" $ do
    int1 <- assertNone int0 "fn avg x y => (x + y) / 2"
    assertDouble_ int1 "avg 4 2" 3
    assertError_ int1 "avg 7"
    assertError_ int1 "avg 7 2 4"

  it "Conflicts" $ do
    withInterpreter ["x = 1", "fn avg x y => (x + y) / 2"] $
      \int0 -> do
        assertError_ int0 "fn x => 0"
        assertError_ int0 "avg = 5"
-}


withInterpreter exprs f =
  case foldM apply newInterpreter exprs of
       Right interp -> f interp >> return ()
       Left err -> pendingWith ("Unexpected error: " ++ err)
  where
    apply int0 s = input s int0 >>= return . snd

assertNone int s = assertEvaluate int s Nothing
assertNone_ int s = assertNone int s >> return ()
assertDouble int s ex = assertEvaluate int s (Just ex)
assertDouble_ int s ex = assertDouble int s ex >> return ()

assertEvaluate int0 s ex =
  case input s int0 of
       Right (x, int1) -> assertEqual inputStr ex x >> return int1
       Left msg -> assertFailure errorMsg >> return undefined
         where errorMsg = unlines [ inputStr
                                  , "expected: " ++ show ex
                                  , "but got error: " ++ msg
                                  ]
  where inputStr = "   input: " ++ show s

assertError_ int s =
  case input s int of
       Left _ -> return ()
       Right (val, _) -> assertFailure errorMsg
         where errorMsg = unlines [ "  input: " ++ show s
                                  , "expected error"
                                  , "but got: " ++ show val
                                  ]
                                  
isLeft (Left _) = True
isLeft _ = False