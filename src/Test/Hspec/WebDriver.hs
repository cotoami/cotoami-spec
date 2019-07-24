-- | Write hspec tests that are webdriver tests, automatically managing the webdriver sessions.
--
-- This module re-exports functions from "Test.Hspec" and "Test.WebDriver.Commands" and it is
-- intended that you just import @Test.Hspec.WebDriver@.  If you need to import @Test.Hspec@ or
-- @Test.WebDriver@, you should do so using a qualified import.
module Test.Hspec.WebDriver
  (
  -- * Webdriver Example
    WdExample(..)
  , WdOptions(..)
  , withBrowser
  , withMultipleBrowserSessions
  , pending
  , pendingWith
  , example

  -- * Webdriver Sessions
  , session
  , sessionWith
  , inspectSession
  , using
  , WdTestSession

  -- * Capabilities
  , module Test.Hspec.WebDriver.Capabilities

  -- * Expectations
  , module Test.Hspec.WebDriver.Expectations

  -- * Re-exports from "Test.Hspec"
  , hspec
  , Spec
  , SpecWith
  , describe
  , context
  , it
  , specify
  , parallel
  , runIO

  -- * Re-exports from "Test.WebDriver"
  , WD
  , Capabilities
  , module Test.WebDriver.Commands
  )
where

import           Test.Hspec              hiding ( shouldReturn
                                                , shouldBe
                                                , shouldSatisfy
                                                , shouldThrow
                                                , pending
                                                , pendingWith
                                                , example
                                                )
import           Test.Hspec.Core.Spec           ( Result(..)
                                                , ResultStatus(..)
                                                , Item(..)
                                                , Example(..)
                                                , SpecTree
                                                , Tree(..)
                                                , fromSpecList
                                                , runSpecM
                                                )

import           Test.WebDriver                 ( WD
                                                , Capabilities
                                                )
import           Test.WebDriver.Commands

import           Test.Hspec.WebDriver.Session   ( SessionState(..)
                                                , WdTestSession(..)
                                                , session
                                                , using
                                                , sessionWith
                                                )
import           Test.Hspec.WebDriver.Example   ( WdExample(..)
                                                , WdOptions(..)
                                                , withBrowser
                                                , withMultipleBrowserSessions
                                                , pending
                                                , pendingWith
                                                , example
                                                , inspectSession
                                                )
import           Test.Hspec.WebDriver.Expectations
import           Test.Hspec.WebDriver.Capabilities
