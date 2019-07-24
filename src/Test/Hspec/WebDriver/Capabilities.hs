module Test.Hspec.WebDriver.Capabilities where

import           Test.WebDriver                 ( Capabilities )
import qualified Test.WebDriver                as W

firefoxCaps, chromeCaps, ieCaps, operaCaps, iphoneCaps, ipadCaps, androidCaps
    :: Capabilities
firefoxCaps = W.defaultCaps { W.browser = W.firefox }
chromeCaps = W.defaultCaps { W.browser = W.chrome }
ieCaps = W.defaultCaps { W.browser = W.ie }
operaCaps = W.defaultCaps { W.browser = W.opera }
iphoneCaps = W.defaultCaps { W.browser = W.iPhone }
ipadCaps = W.defaultCaps { W.browser = W.iPad }
androidCaps = W.defaultCaps { W.browser = W.android }
