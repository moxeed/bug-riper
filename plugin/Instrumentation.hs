{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Instrumentation ( plugin ) where

-- base
import Data.Traversable ( for )
import Control.Monad.IO.Class ( liftIO )
import Data.Foldable ( toList )

-- ghc
import GHC.Data.Bag
import GHC.Types.SrcLoc
import GHC.Tc.Utils.Env
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Core.Type
import GHC.Rename.Expr
import GHC.Tc.Gen.Expr
import GHC.Iface.Env
import GHC.Parser.Annotation
import System.IO.Unsafe

import qualified GHC
import qualified GHC.ThToHs as GHC
import qualified GHC.HsToCore as GHC
import qualified GHC.Rename.Expr as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC
import qualified GHC.Tc.Types.Evidence as GHC
import qualified GHC.Tc.Utils.Env as GHC
import qualified GHC.Tc.Utils.Monad as GHC
import qualified GHC.Tc.Utils.TcMType as GHC
import qualified GHC.Tc.Utils.Zonk as GHC
import qualified GHC.Tc.Solver.Monad as SM
import qualified GHC.Tc.Solver as GHC
import qualified GHC.Tc.Gen.Expr as GHC
import qualified GHC.Parser.Annotation as GHC

import qualified GHC.Core.Utils as CoreUtils

--syb
import Data.Generics ( everywhereM, mkM )

-- template-haskell
import Language.Haskell.TH as TH

trace :: String -> a -> a
trace input body = unsafePerformIO $ do
 appendFile "run.out" (input ++ "\n")
 return body

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { 
      GHC.typeCheckResultAction = \_cliOptions -> pluginImpl
    }


pluginImpl :: GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
pluginImpl ms tcGblEnv = do
  let tcg_binds = GHC.tcg_binds tcGblEnv
  expr_tcg_binds <- mkM addExprTrace `everywhereM` tcg_binds
  new_tcg_binds <- mkM addMatchTrace `everywhereM` expr_tcg_binds
  return tcGblEnv { GHC.tcg_binds = new_tcg_binds}

addExprTrace :: GHC.LHsExpr GHC.GhcTc -> GHC.TcM (GHC.LHsExpr GHC.GhcTc)
addExprTrace (GHC.L loc (GHC.HsIf p cond first second)) = do
  new_first <- injectTrace first
  new_second <- injectTrace second
  return (GHC.L loc (GHC.HsIf p cond new_first new_second))

addExprTrace other = 
  return other

addMatchTrace :: GHC.LGRHS GHC.GhcTc (GHC.LHsExpr GHC.GhcTc) -> GHC.TcM (GHC.LGRHS GHC.GhcTc (GHC.LHsExpr GHC.GhcTc))
addMatchTrace (GHC.L loc (GHC.GRHS p g body)) = do
  new_body <- injectTrace body
  return (GHC.L loc (GHC.GRHS p g new_body))

addMatchTrace other = 
  return other

injectTrace :: GHC.LHsExpr GHC.GhcTc -> GHC.TcM (GHC.LHsExpr GHC.GhcTc)
injectTrace expr@(GHC.L GHC.SrcSpanAnn{GHC.locA=(GHC.RealSrcSpan loc _)} _) = do
  Just exprT <-
    typeOfExpr expr

  let 
    ppWhere =
      GHC.renderWithContext 
      GHC.defaultSDocContext
      ( GHC.ppr loc )

  Right traceExprPs <-
        fmap ( GHC.convertToHsExpr GHC.Generated GHC.noSrcSpan )
          $ liftIO
          $ TH.runQ
          $ [| trace ppWhere |]

  ( traceExprRn, _ ) <-
    GHC.rnLExpr traceExprPs

  ( traceExprTc, wanteds ) <-
    GHC.captureConstraints
    ( GHC.tcMonoExpr
      traceExprRn
      ( GHC.Check ( GHC.mkVisFunTyMany exprT exprT ) )
    )

  -- Solve wanted constraints and build a wrapper.
  evBinds <-
    GHC.EvBinds . GHC.evBindMapBinds . snd
      <$> SM.runTcS ( GHC.solveWanteds wanteds )

  emptyZonkEnv <- GHC.emptyZonkEnv
  ( _, zonkedEvBinds ) <-
    GHC.zonkTcEvBinds emptyZonkEnv evBinds

  let
    wrapper = 
      GHC.mkWpLet zonkedEvBinds

  -- Apply the wrapper to our type checked syntax and fully saturate the
  -- diagnostic function with the necessary arguments.
  newBody <-
    GHC.zonkTopLExpr
    ( GHC.mkHsPar
      ( GHC.mkHsApp
        ( GHC.mkLHsWrap wrapper traceExprTc )
        ( GHC.mkHsPar expr )
      )
    )

  return newBody

injectTrace other = 
  return other

typeOfExpr :: GHC.LHsExpr GHC.GhcTc -> GHC.TcM ( Maybe GHC.Type )
typeOfExpr e = do
  hs_env  <-
    GHC.getTopEnv

  ( _, mbe ) <-
    liftIO ( GHC.deSugarExpr hs_env e )

  return ( CoreUtils.exprType <$> mbe )