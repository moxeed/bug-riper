module DUEffectTest where
import DUEffect
import Test.HUnit
 
test1 :: Test
test1 = TestCase (assertEqual "should return 2" 2 (func 1 1))

test2 :: Test
test2 = TestCase (assertEqual "should return -2" (-2) (func (-1) (-1)))
 
tests :: [Test]
tests = [
    TestLabel "test1" test1,
    TestLabel "test2" test2
    ]