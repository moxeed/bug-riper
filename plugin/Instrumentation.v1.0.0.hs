{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Instrumentation ( plugin ) where

-- base
import Data.Traversable ( for )
import Control.Monad.IO.Class ( liftIO )
import Data.Foldable ( toList )
import Debug.Trace ( trace )

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

import qualified GHC.Core.Utils as CoreUtils

--syb
import Data.Generics ( everywhereM, mkM )

-- template-haskell
import Language.Haskell.TH as TH

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { 
      GHC.typeCheckResultAction = \_cliOptions -> pluginImpl
    }


pluginImpl :: GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
pluginImpl ms tcGblEnv = do
  let tcg_binds = GHC.tcg_binds tcGblEnv
  new_tcg_binds <- mkM addTrace `everywhereM` tcg_binds
  --liftIO $ putStrLn $ GHC.renderWithContext GHC.defaultSDocContext $ GHC.ppr new_tcg_binds
  return tcGblEnv { GHC.tcg_binds = new_tcg_binds}

applyMatch :: GHC.LMatch GHC.GhcTc (GHC.LHsExpr GHC.GhcTc) -> GHC.TcM (GHC.LMatch GHC.GhcTc (GHC.LHsExpr GHC.GhcTc))
applyMatch (GHC.L loc match@GHC.Match{GHC.m_grhss=(GHC.GRHSs a grhssGRHSs grhssLocalBinds)}) = do
      new_grhssGRHSs <- mkM applyGRHS `everywhereM` grhssGRHSs
      return (GHC.L loc match{GHC.m_grhss=(GHC.GRHSs a new_grhssGRHSs grhssLocalBinds)})

applyGRHS :: GHC.LGRHS GHC.GhcTc (GHC.LHsExpr GHC.GhcTc) -> GHC.TcM (GHC.LGRHS GHC.GhcTc (GHC.LHsExpr GHC.GhcTc))
applyGRHS (GHC.L loc (GHC.GRHS a b body)) = do
  new_body <- (trace "BodyChanged" injectTrace body)
  return (trace (GHC.renderWithContext GHC.defaultSDocContext $ GHC.ppr new_body) GHC.L loc (GHC.GRHS a b new_body))

addTrace :: GHC.LHsBind GHC.GhcTc -> GHC.TcM (GHC.LHsBind GHC.GhcTc)
addTrace (GHC.L span fun@GHC.FunBind{GHC.fun_matches=fun_matches}) = do
    new_matches <- mkM applyMatch `everywhereM` matches
    return (GHC.L span fun{GHC.fun_matches=fun_matches{GHC.mg_alts=GHC.L innerSpan new_matches}})
  where 
    GHC.L innerSpan matches = GHC.mg_alts fun_matches

addTrace (GHC.L loc (GHC.XHsBindsLR bind@GHC.AbsBinds{GHC.abs_binds=abs_binds})) = do
  new_abs_binds <- mkM addTrace `everywhereM` abs_binds
  return (trace "XHsBindsLR" GHC.L loc (GHC.XHsBindsLR bind{GHC.abs_binds=new_abs_binds}))

addTrace typeChecked@(GHC.L loc a@GHC.PatBind{}) = 
  return (trace "PatBind" typeChecked)

addTrace typeChecked@(GHC.L loc a@GHC.PatSynBind{}) = 
  return (trace "PatSynBind" typeChecked)
  
addTrace typeChecked = do 
  return (trace "OtherType" typeChecked)


injectTrace :: GHC.LHsExpr GHC.GhcTc -> GHC.TcM (GHC.LHsExpr GHC.GhcTc)
injectTrace expr@(GHC.L loc _) = do
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
      ( GHC.Check ( GHC.mkInvisFunTy exprT exprT exprT ) )
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
      ( GHC.mkHsApp
          ( GHC.mkLHsWrap wrapper traceExprTc )
          ( GHC.mkHsPar expr )
      )

  return newBody

typeOfExpr :: GHC.LHsExpr GHC.GhcTc -> GHC.TcM ( Maybe GHC.Type )
typeOfExpr e = do
  hs_env  <-
    GHC.getTopEnv

  ( _, mbe ) <-
    liftIO ( GHC.deSugarExpr hs_env e )

  return ( CoreUtils.exprType <$> mbe )