import           Data.Tagged          (Tagged (..), untag)

import           Test.Framework       (Test, defaultMain, testGroup)

import qualified Data.HashPSQ         as HashPSQ
import qualified Data.HashPSQ.Tests
import qualified Data.IntPSQ          as IntPSQ
import qualified Data.IntPSQ.Tests
import qualified Data.PSQ             as PSQ
import qualified Data.PSQ.Class.Tests
import qualified Data.PSQ.Tests
import           Data.PSQ.Tests.Util

main :: IO ()
main = defaultMain
    [ testGroup "Data.IntPSQ.Tests"
        Data.IntPSQ.Tests.tests
    , testGroup "Data.HashPSQ.Tests"
        Data.HashPSQ.Tests.tests
    , testGroup "Data.PSQ.Tests"
        Data.PSQ.Tests.tests
    , testGroup "Data.PSQ.Class.Tests IntPSQ"  $ untag
        (Data.PSQ.Class.Tests.tests
            :: Tagged IntPSQ.IntPSQ [Test])
    , testGroup "Data.PSQ.Class.Tests PSQ"     $ untag
        (Data.PSQ.Class.Tests.tests
            :: Tagged (PSQ.PSQ Int) [Test])
    , testGroup "Data.PSQ.Class.Tests HashPSQ" $ untag
        (Data.PSQ.Class.Tests.tests
            :: Tagged (HashPSQ.HashPSQ LousyHashedInt) [Test])
    ]
