module Test.Hspec.WebDriver.SpecUtils where

import qualified Control.Monad.Trans.State     as State
import           Test.Hspec.Core.Spec           ( SpecTree
                                                , Item(..)
                                                , Tree(..)
                                                )


countItems :: [SpecTree a] -> Int
countItems s = flip State.execState 0 $ traverseSpec go s
    where go item = State.state $ \cnt -> (item, cnt + 1)


-- 以下は、Data.Traversable 利用したツリー巡回処理の好例。


-- | SpecTree の各 item に procItemWithCnt 関数を適用する。
-- * traverseSpec 関数で SpecTree を巡回しつつ、各 item への処理を State モナドに変換した後、evalState で実行する。
-- * procItemWithCnt 関数を　State モナドに変換する stProcItemWithCnt を traverseSpec に渡す。
mapWithCounter :: (Int -> Item a -> Item b) -> [SpecTree a] -> [SpecTree b]
mapWithCounter procItemWithCnt specTree = flip State.evalState 0
    $ traverseSpec stProcItemWithCnt specTree
  where
    stProcItemWithCnt item =
        State.state $ \cnt -> (procItemWithCnt cnt item, cnt + 1)


-- | Data.Traversable を使った SpecTree の巡回
-- cf. http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Traversable.html
-- 各 item を State モナドに変換する関数を利用して、SpecTree 全体を対象にした State モナドを作る
traverseSpec
    :: Applicative f => (Item a -> f (Item b)) -> [SpecTree a] -> f [SpecTree b]
traverseSpec f = traverse (traverseTree f)


-- | SpecTree の各要素毎の処理
-- SpecTree を各要素を Applicative に変換する（実態は State モナド）
traverseTree
    :: Applicative f => (Item a -> f (Item b)) -> SpecTree a -> f (SpecTree b)
traverseTree f (Leaf i     ) = Leaf <$> f i -- 
traverseTree f (Node msg ss) = Node msg <$> traverse (traverseTree f) ss
traverseTree f (NodeWithCleanup c ss) =
    NodeWithCleanup c' <$> traverse (traverseTree f) ss
    where c' _b = c undefined -- this undefined is OK since we do not export the definition of WdTestSession
                            -- so the user cannot do anything with the passed in value to 'afterAll'


