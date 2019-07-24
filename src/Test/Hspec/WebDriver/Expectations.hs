module Test.Hspec.WebDriver.Expectations where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Control.Exception.Lifted       ( Exception
                                                , try
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Test.HUnit                     ( assertBool
                                                , assertEqual
                                                , assertFailure
                                                )
import qualified Test.Hspec                    as Hspec
import           Test.WebDriver                 ( WD )
import           Test.WebDriver.Commands        ( Element
                                                , tagName
                                                , getText
                                                , attr
                                                )


-- | 'H.shouldBe' lifted into the 'WD' monad.
shouldBe :: (Show a, Eq a) => a -> a -> WD ()
x `shouldBe` y = liftIO $ x `Hspec.shouldBe` y


-- | Asserts that the given element matches the given tag.
shouldBeTag :: Element -> Text -> WD ()
e `shouldBeTag` name = do
    t <- tagName e
    liftIO $ assertEqual ("tag of " ++ show e) name t


-- | Asserts that the given element has the given text.
shouldHaveText :: Element -> Text -> WD ()
e `shouldHaveText` txt = do
    t <- getText e
    liftIO $ assertEqual ("text of " ++ show e) txt t


-- | Asserts that the given element contains the given text.
shouldContainText :: Element -> Text -> WD ()
e `shouldContainText` txt = do
    t <- getText e
    liftIO 
        $ assertBool (show t ++ " does not contain " ++ show txt) 
        $ T.isInfixOf txt t 


-- | Asserts that the given elemnt has the attribute given by @(attr name, value)@.
shouldHaveAttr :: Element -> (Text, Text) -> WD ()
e `shouldHaveAttr` (a, txt) = do
    t <- attr e a
    liftIO $ assertEqual ("attribute " ++ T.unpack a ++ " of " ++ show e)
                         (Just txt)
                         t


-- | Asserts that the action returns the expected result.
shouldReturn :: (Show a, Eq a) => WD a -> a -> WD ()
action `shouldReturn` expected =
    action >>= (\a -> liftIO $ a `Hspec.shouldBe` expected)


-- | Asserts that the action throws an exception.
shouldThrow :: (Show e, Eq e, Exception e) => WD a -> e -> WD ()
shouldThrow w expected = do
    r <- try w
    case r of
        Left err -> err `shouldBe` expected
        Right _ ->
            liftIO
                $  assertFailure
                $  "did not get expected exception "
                ++ show expected
