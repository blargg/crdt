import Test.Tasty

import DeltaCRDTSpec

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "Set is DCRDT" setIsDCRDT
  ]
