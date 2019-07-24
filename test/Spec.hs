{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec.WebDriver

browserSettings :: [Capabilities]
browserSettings = [chromeCaps]

main :: IO ()
main = hspec $ describe "日本Seleniumユーザーコミュニティ提供のテストサイト" $ do

    session "予約の成功例" $ using browserSettings $ do
        it "予約フォームを開く" $ withBrowser $ do
            openPage "http://example.selenium.jp/reserveApp/"

        it "予約フォームの入力と送信" $ withBrowser $ do
            monthInput <- findElem ( ByCSS "input#reserve_month" )
            clearInput monthInput
            sendKeys "8" monthInput

            guestnameInput <- findElem ( ByCSS "input#guestname" )
            sendKeys "山田太郎" guestnameInput

            findElem ( ByCSS "form#reserve_info" ) >>= submit

        it "予約内容の確認" $ withBrowser $ do
            body <- findElem $ ByCSS "body"
            body `shouldContainText` "予約内容"
            body `shouldContainText` "ご人数: 1名様"
            body `shouldContainText` "朝食: あり"
            body `shouldContainText` "お名前: 山田太郎 様"

            findElem ( ByCSS "button#commit" ) >>= click

        it "完了ページ" $ withBrowser $ do
            body <- findElem $ ByCSS "body"
            body `shouldContainText` "予約を完了しました。"

    session "名前未入力" $ using browserSettings $ do
        it "予約フォームを開く" $ withBrowser $ do
            openPage "http://example.selenium.jp/reserveApp/"

        it "そのまま送信" $ withBrowser $ do
            findElem ( ByCSS "form#reserve_info" ) >>= submit

        it "入力エラーが表示される" $ withBrowser $ do
            body <- findElem $ ByCSS "body"
            body `shouldContainText` "予約エラー"
            body `shouldContainText` "お名前が指定されていません"

    session "宿泊日未入力" $ using browserSettings $ do
        it "予約フォームを開く" $ withBrowser $ do
            openPage "http://example.selenium.jp/reserveApp/"

        it "名前だけ入力して送信" $ withBrowser $ do
            guestnameInput <- findElem ( ByCSS "input#guestname" )
            sendKeys "山田太郎" guestnameInput

            findElem ( ByCSS "form#reserve_info" ) >>= submit

        it "入力エラーが表示される" $ withBrowser $ do
            body <- findElem $ ByCSS "body"
            body `shouldContainText` "予約エラー"
            body `shouldContainText` "宿泊日には、翌日以降の日付を指定してください。"

            
