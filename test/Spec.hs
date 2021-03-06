{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec.WebDriver

browserSettings :: [Capabilities]
browserSettings = [chromeCaps]

main :: IO ()
main = hspec $ describe "Cotoami" $ do

    session "Sign in with email" $ using browserSettings $ do
        it "Open the top page" $ withBrowser $ do
            openPage "http://cotoami:4000"
            body <- findElem $ ByCSS "body"
            body `shouldContainText` "Welcome to Cotoami!"
