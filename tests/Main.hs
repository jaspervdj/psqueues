import           Test.Framework (defaultMain, testGroup)

import qualified Data.IntPSQ.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Data.IntPSQ.Tests" Data.IntPSQ.Tests.tests
    ]
