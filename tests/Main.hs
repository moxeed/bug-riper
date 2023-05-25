import Test.HUnit
import BasicSumTest(tests)
import DUEffectTest(tests)
import qualified System.Exit as Exit

main :: IO ()
main = do
    result <- runTestTT $ TestList (BasicSumTest.tests ++ DUEffectTest.tests)
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess