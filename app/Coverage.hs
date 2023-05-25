module Coverage where
import Control.Monad.State
import Data.Set as Set

type CoverageItem = String
type CoverageInfo = [CoverageItem]

emptyCoverage :: CoverageInfo
emptyCoverage = []

coverageScore :: CoverageInfo -> Int
coverageScore = Set.size . Set.fromList

type Coverage = State CoverageInfo

covers :: a -> [CoverageItem] -> Coverage a
covers value items = do 
  curCoverage <- get
  put (items ++ curCoverage)
  return value  