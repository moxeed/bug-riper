# Revision history for bug-riper

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.


forum -> XRec unboxing issue
definitions -> Need to Fix them

g2 -> symbolic execution -> reuse the process

manual annotation with monad (sorting network) 
1 -> Measure Def Use Coverage
2 -> Automation HTS


# How To Install G2

Only Runs On GHC 8.2.2
If You Want to Use Multiple GHC Version Specify The Version Using -W Option In Cabal This Option Should Be Provided With The Path of Your GHC Version

cabal install g2 -w D:\ghc\ghc-8.2.2\bin\ghc.exe --constraint 'liquid-fixpoint < 0.8.10'
Add G2 To Path Variable

# Code Optimization

GHC Optimization Can Handle Some Constant Values, Therefore Causing CFG Output To Be Incorrect
We Sould Disable This Options by Setting -O0 Flag In GHC Build

[Optimization Documents Reference](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html)

