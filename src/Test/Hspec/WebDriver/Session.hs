module Test.Hspec.WebDriver.Session where

import qualified Data.Text                     as Text
import qualified Data.Aeson                    as Aeson
import qualified Control.Monad                 as Monad
import qualified Control.Monad.Trans.State     as MTS
import qualified Control.Concurrent.MVar       as MVar
import           Test.Hspec                    as Hspec
import           Test.Hspec.Core.Spec           ( SpecTree
                                                , Item(..)
                                                , Tree(..)
                                                )
import qualified Test.Hspec.Core.Spec          as Spec
import           Test.WebDriver                 ( Capabilities )
import qualified Test.WebDriver                as WD
import qualified Test.WebDriver.Session        as WDS
import qualified Test.WebDriver.Capabilities   as WDCap
import qualified Test.WebDriver.Config         as WDCfg
import qualified Test.WebDriver.Commands       as WDCmd
import qualified Test.Hspec.WebDriver.SpecUtils
                                               as SpecUtils


-- | The state passed between examples inside the mvars.
data SessionState multi = SessionState {
    -- | The already created sessions
    stSessionMap :: [(multi, WDS.WDSession)]
    -- | True if the previous example had an error
  , stPrevHadError :: Bool
    -- | True if the previous example was aborted with 'inspectSession'
  , stPrevAborted :: Bool
    -- | Create a new session
  , stCreateSession :: IO WDS.WDSession
}


-- | Internal state for webdriver test sessions.
data WdTestSession multi = WdTestSession {
    wdTestOpen :: IO (SessionState multi)
  , wdTestClose :: SessionState multi -> IO ()
}


-- | Capabilities 毎の説明文を自動生成して sessionWith を呼び出す。
session :: String -> ([Capabilities], SpecWith (WdTestSession multi)) -> Spec
session msg (caps, spec) = sessionWith
    WD.defaultConfig { WDCfg.wdHost = "localhost" }
    msg
    (caps', spec)
  where
    caps' = map f caps
    f c = case Aeson.toJSON (WDCap.browser c) of
        Aeson.String b -> (c, Text.unpack b)
        _              -> (c, show c) -- this should not be the case, every browser toJSON is a string


-- | A synonym for constructing pairs that allows the word @using@ to be used with 'session' so that the session
-- description reads like a sentance.
using
    :: [caps]
    -> SpecWith (WdTestSession multi)
    -> ([caps], SpecWith (WdTestSession multi))
using = (,)


-- | Capabilities ごとに webdriver セッションを作り、そのセッションを SpecTree 内の各 Item からアクセスできるようにして、
-- `SpecWith (WdTestSession multi)` を hspec が実行可能な型 Spec (`SpecWith ()`) に変換する。
-- 
-- * Later examples can rely on the browser state created by earlier examples. 
--
-- * `SpecWith (WdTestSession multi)` は、`[SpecTree (WdTestSession multi)]` を蓄積する Writer モナド。
--
--     type Spec = SpecWith ()
--     type SpecWith a = SpecM a ()
--     newtype SpecM a r = SpecM (WriterT [SpecTree a] IO r)
--     type SpecTree a = Tree (ActionWith a) (Item a)
--
-- * In the simplest case when @multi@ is @()@, before the first example is executed a new webdriver
-- session with the given capabilities is created.  The examples are then executed in depth-first
-- order, and the session is then closed when either an exception occurs or the examples complete.
-- (The session can be left open with 'inspectSession').
--
-- * More generally, as the examples are executed, each time a new value of type @multi@ is seen, a
-- new webdriver session with the capabilities is automatically created.  Later examples will
-- continue with the session matching their value of @multi@.
--
-- In addition, each capability is paired with a descriptive string which is passed to hspec to
-- describe the example.  By default, 'session' uses the browser name as the description.  'sessionWith'
-- supports a more detailed description so that in the hspec output you can distinguish between
-- capabilities that share the same browser but differ in the details, for example capabilities with and
-- without javascript.

sessionWith
    :: WDCfg.WDConfig
    -> String
    -> ([(Capabilities, String)], SpecWith (WdTestSession multi))
    -> Spec
sessionWith cfg msg (caps, spec) = spec'
  where
    procT c = procTestSession cfg (WDCap.getCaps c) spec
    spec' = case caps of
        []           -> it msg $ Hspec.pendingWith "No capabilities specified"
        [(c, cDscr)] -> describe (msg ++ " using " ++ cDscr) $ procT c
        _            -> describe msg $ mapM_
            (\(c, cDscr) -> describe ("using " ++ cDscr) $ procT c)
            caps


-- | Capabilities に対して一つのセッションを作り、SpecTreeに共有させる。
procTestSession
    :: WDCfg.WDConfig -> Capabilities -> SpecWith (WdTestSession multi) -> Spec
procTestSession cfg cap spec = do
    (mvars, trees) <- runIO $ do
        trees <- Spec.runSpecM spec
        let itemCnt = SpecUtils.countItems trees
        mvars <- Monad.replicateM itemCnt MVar.newEmptyMVar
        return (mvars, trees)

    Spec.fromSpecList $ SpecUtils.mapWithCounter
        (procSpecItem cfg { WDCfg.wdCapabilities = cap } mvars)
        trees


-- | Convert a single test item to a generic item by providing it with the WdTestSession.
--
-- `Item (WdTestSession multi)` を `Item ()` に変換。
-- itemExample 関数の第二引数を以下のように変換:
--     `(ActionWith (WdTestSession multi) -> IO ())`
--       ↓
--     `(ActionWith () -> IO ())`
procSpecItem
    :: WDCfg.WDConfig
    -> [MVar.MVar (SessionState multi)]
    -> Int
    -> Item (WdTestSession multi)
    -> Item ()
procSpecItem cfg mvars n item = item
    { itemExample = \p act progress -> itemExample item p (act . act') progress
    }
    where act' f () = f (createTestSession cfg mvars n)


-- itemExample :: Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
-- type ActionWith a = a -> IO ()


-- | Create a WdTestSession.
createTestSession
    :: WDCfg.WDConfig
    -> [MVar.MVar (SessionState multi)]
    -> Int
    -> WdTestSession multi
createTestSession cfg mvars n = WdTestSession open close
  where
    open | n == 0    = return $ SessionState [] False False create
         | otherwise = MVar.takeMVar (mvars !! n)

    create = do
        s <- WDCfg.mkSession cfg
        WD.runWD s $ WDCmd.createSession $ WDCfg.wdCapabilities cfg

    close st
        | length mvars - 1 == n = mapM_ ((`WD.runWD` WDCmd.closeSession) . snd)
        $ stSessionMap st
        | otherwise = MVar.putMVar (mvars !! (n + 1)) st
