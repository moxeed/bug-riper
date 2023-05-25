module BasicSumTest where
import BasicSum
import Test.HUnit
 
test1 :: Test
test1 = TestCase (assertEqual "should return 3" 3 (basicSum 1 2))
 
tests :: [Test]
tests = [TestLabel "test1" test1]