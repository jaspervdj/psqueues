import           Data.Tagged          (Tagged (..), untag)

import           Test.Framework       (Test, defaultMain, testGroup)

import qualified Data.HashPSQ         as HashPSQ
import qualified Data.HashPSQ.Tests
import qualified Data.IntPSQ          as IntPSQ
import qualified Data.IntPSQ.Tests
import qualified Data.OrdPSQ          as OrdPSQ
import qualified Data.OrdPSQ.Tests
import qualified Data.PSQ.Class.Tests
import           Data.PSQ.Class.Util

main :: IO ()
main = defaultMain
    [ testGroup "Data.IntPSQ.Tests"
        Data.IntPSQ.Tests.tests
    , testGroup "Data.HashPSQ.Tests"
        Data.HashPSQ.Tests.tests
    , testGroup "Data.OrdPSQ.Tests"
        Data.OrdPSQ.Tests.tests
    , testGroup "Data.PSQ.Class.Tests IntPSQ"  $ untag
        (Data.PSQ.Class.Tests.tests
            :: Tagged IntPSQ.IntPSQ [Test])
    , testGroup "Data.PSQ.Class.Tests PSQ"     $ untag
        (Data.PSQ.Class.Tests.tests
            :: Tagged (OrdPSQ.OrdPSQ Int) [Test])
    , testGroup "Data.PSQ.Class.Tests HashPSQ" $ untag
        (Data.PSQ.Class.Tests.tests
            :: Tagged (HashPSQ.HashPSQ LousyHashedInt) [Test])
    ]
