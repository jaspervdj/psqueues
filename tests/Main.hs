import           Data.Tagged (Tagged (..), untag)

import           Test.Framework (Test, defaultMain, testGroup)

import qualified Data.IntPSQ          as IntPSQ
import qualified Data.IntPSQ.Tests
import qualified Data.PSQ.Class.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Data.IntPSQ.Tests" Data.IntPSQ.Tests.tests
    , testGroup "Data.PSQ.Class.Tests IntPSQ" $
        untag (Data.PSQ.Class.Tests.tests :: Tagged IntPSQ.IntPSQ [Test])
    ]
