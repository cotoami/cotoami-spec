{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}
module Test.Hspec.WebDriver.Example where

import           Data.Default                   ( Default(..) )
import           Data.Typeable                  ( Typeable
                                                , cast
                                                )
import qualified Data.IORef                    as IORef
import           Control.Exception              ( SomeException(..) )
import           Control.Exception.Lifted       ( try
                                                , Exception
                                                , onException
                                                , throwIO
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Test.Hspec                     ( Expectation )
import           Test.Hspec.Core.Spec           ( Result(..)
                                                , ResultStatus(..)
                                                , Example(..)
                                                )
import           Test.WebDriver                 ( WD )
import qualified Test.WebDriver                as WebDriver
import qualified Test.WebDriver.Session        as WDS
import           Test.Hspec.WebDriver.Session   ( SessionState(..)
                                                , WdTestSession(..)
                                                )


-- | A webdriver example.
--
-- The webdriver action of type @'WD' ()@ should interact with the webpage using commands from
-- "Test.WebDriver.Commands" (which is re-exported from this module) and then use the
-- <#g:4 expectations> in this module.  It is possible to split up the spec of a single page into multiple
-- examples where later examples start with the web browser state from the end of the previous
-- example.  This is helpful to keep each individual example small and allows the entire spec to be
-- described at the beginning with pending examples.
--
-- The way this works is that you combine examples into a session using 'session' or 'sessionWith'.
-- A webdriver session is then threaded through all examples in a session so that a later example in
-- the session can rely on the webbrowser state as set up by the previous example.  The type system
-- enforces that every webdriver example must be located within a call to 'session' or
-- 'sessionWith'.  Indeed, a 'WdExample' produces a @'SpecWith' ('WdTestSession' multi)@ which can
-- only be converted to 'Spec' using 'session' or 'sessionWith'.  The reason for the 'WdPending'
-- constructor is so that a pending example can be specified with type @'SpecWith' ('WdTestSession'
-- multi)@ so it can compose with the other webdriver examples.
--
-- The type @multi@ is used when testing multiple sessions at once (e.g. to test multiple
-- interacting users), otherwise it is @()@. Values of this type are used to determine which browser
-- session the example should be executed against.  A new session is created every time a new value
-- of type @multi@ is seen.  Note that the type system enforces that every example within the
-- session has the same type @multi@.
data WdExample multi = WdExample multi WdOptions (WD ()) | WdPending (Maybe String)


data WdOptions = WdOptions {
  -- | As soon as an example fails, skip all remaining tests in the session.  Defaults to True.
  skipRemainingTestsAfterFailure :: Bool
}


instance Default WdOptions where
    def = WdOptions { skipRemainingTestsAfterFailure = True }


-- | A shorthand for constructing a 'WdExample' from a webdriver action when you are only testing a
-- single browser session at once.  See the XKCD example at the top of the page.
withBrowser :: WD () -> WdExample ()
withBrowser = WdExample () def


-- | Create a webdriver example, specifying which of the multiple sessions the example should be
-- executed against.  I suggest you create an enumeration for multi, for example:
--
-- >data TestUser = Gandolf | Bilbo | Legolas
-- >    deriving (Show, Eq, Enum, Bounded)
-- >
-- >runUser :: TestUser -> WD () -> WDExample TestUser
-- >runUser = runWDWith
-- >
-- >spec :: Spec
-- >spec = session "tests some page" $ using [firefoxCaps] $ do
-- >    it "does something with Gandolf" $ runUser Gandolf $ do
-- >        openPage ...
-- >    it "does something with Bilbo" $ runUser Bilbo $ do
-- >        openPage ...
-- >    it "goes back to the Gandolf session" $ runUser Gandolf $ do
-- >        e <- findElem ....
-- >        ...
--
-- In the above code, two sessions are created and the examples will go back and forth between the
-- two sessions.  Note that a session for Legolas will only be created the first time he shows up in
-- a call to @runUser@, which might be never.  To share information between the sessions (e.g. some
-- data that Gandolf creates that Bilbo should expect), the best way I have found is to use IORefs
-- created with 'runIO' (wrapped in a utility module).
withMultipleBrowserSessions :: multi -> WD () -> WdExample multi
withMultipleBrowserSessions multi = WdExample multi def


-- | A pending example.
pending :: WdExample multi
pending = WdPending Nothing


-- | A pending example with a message.
pendingWith :: String -> WdExample multi
pendingWith = WdPending . Just


-- | A version of 'H.example' which lifts an @IO ()@ to a webdriver example (so it can be composed
-- with other webdriver examples).  In the case of multiple sessions, it doesn't really matter which
-- session the expectation is executed against, so a default value is used.  In the case of single
-- sessions, the type is @WdExample ()@.
example :: Default multi => Expectation -> WdExample multi
example = WdExample def def . liftIO


data AbortSession = AbortSession
    deriving (Show, Typeable)
instance Exception AbortSession


-- | Abort the session without closing the session.
--
-- Normally, 'session' will automatically close the session either when the tests complete without
-- error or when any of the tests within the session throws an error.  When developing the test
-- suite, this can be annoying since closing the session causes the browser window to close.
-- Therefore, while developing the test suite, you can insert a call to 'inspectSession'.  This will
-- immedietly halt the session (all later tests will fail) but will not close the session so that
-- the browser window stays open.
inspectSession :: WD ()
inspectSession = throwIO AbortSession


instance Eq multi => Example (WdExample multi) where
    type Arg (WdExample multi) = WdTestSession multi
    evaluateExample (WdPending msg) _ _ _ =
        return $ Result "" (Pending Nothing msg)
    evaluateExample (WdExample multi (WdOptions { skipRemainingTestsAfterFailure }) wd) _ act _
        = do
            prevHadError <- IORef.newIORef False
            aborted      <- IORef.newIORef False

            act $ \testsession -> do

                tstate <- wdTestOpen testsession

                msess  <-
                    case
                        ( lookup multi $ stSessionMap tstate
                        , (stPrevHadError tstate || stPrevAborted tstate)
                            && skipRemainingTestsAfterFailure
                        )
                    of
                        (_     , True ) -> return Nothing
                        (Just s, False) -> return $ Just s
                        (Nothing, False) ->
                            Just
                                <$>           stCreateSession tstate
                                `onException` wdTestClose
                                                  testsession
                                                  tstate { stPrevHadError = True
                                                         }

                case msess of
                    Just wdsession -> WebDriver.runWD wdsession $ do
                        -- run the example
                        macterr <- try wd
                        case macterr of
                            Right () -> do
                                -- pass current session on to the next test
                                wdsession' <- WDS.getSession
                                let
                                    smap = (multi, wdsession') : filter
                                        ((/= multi) . fst)
                                        (stSessionMap tstate)
                                liftIO $ wdTestClose
                                    testsession
                                    tstate { stSessionMap = smap }

                            Left acterr@(SomeException actex) ->
                                case cast actex of
                                    Just AbortSession -> do
                                        -- pass empty list on to the next test so the session is not closed
                                        liftIO $ wdTestClose
                                            testsession
                                            tstate { stSessionMap  = []
                                                   , stPrevAborted = True
                                                   }
                                        liftIO $ IORef.writeIORef aborted True
                                    Nothing -> do
                                        liftIO $ wdTestClose
                                            testsession
                                            tstate { stPrevHadError = True }
                                        throwIO acterr

                    _ -> do
                        -- on error, just pass along the session and error
                        IORef.writeIORef prevHadError $ stPrevHadError tstate
                        IORef.writeIORef aborted $ stPrevAborted tstate
                        wdTestClose testsession tstate

            merr   <- IORef.readIORef prevHadError
            mabort <- IORef.readIORef aborted
            return $ case (merr, mabort) of
                (True, _) -> Result
                    ""
                    (Pending Nothing (Just "Previous example had an error"))
                (_, True) -> Result
                    ""
                    (Pending Nothing (Just "Session has been aborted"))
                _ -> Result "" Success
