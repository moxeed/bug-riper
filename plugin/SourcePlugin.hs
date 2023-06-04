{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-} 
module SourcePlugin where

import Control.Monad.IO.Class
import GHC.Driver.Plugins
import GHC.Plugins
import GHC.Tc.Types
import GHC.Hs.Decls
import GHC.Hs.Expr
import GHC.Hs.Extension
import GHC
import Language.Haskell.Syntax.Decls

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = parsedPlugin
  , renamedResultAction = renamedAction
  , typeCheckResultAction = typecheckPlugin
  , spliceRunAction = metaPlugin
  , interfaceLoadAction = interfaceLoadPlugin
  }

printBind _ = "asds\n"
--   where id = unXRec fun_id
-- printBind PatBind{..} = "PatBind\n"
-- printBind VarBind{..} = "VarBind\n"
-- printBind _ = "Other\n"

f :: HsDecl p -> String
f (TyClD _ _) = "TyClD\n"
f (InstD _ _) = "InstD\n"
f (DerivD _ _) = "DerivD\n"
f (ValD id bind) = printBind bind
f (SigD _ _) = "SigD\n"
f (KindSigD _ _) = "KindSigD\n"
f (DefD _ _) = "DefD\n"
f (ForD _ _) = "ForD\n"
f (WarningD _ _) = "WarningD\n"
f (AnnD _ _) = "AnnD\n"
f (RuleD _ _) = "RuleD\n"
f (SpliceD _ _) = "SpliceD\n"
f (DocD _ _) = "DocD\n"
f (RoleAnnotD  _ _) = "RoleAnnotD\n"
f (XHsDecl _) = "XHsDecl\n"
f _ = "Others\n"

checkSingleDeclaration :: [GenLocated a (HsDecl GhcPs)] -> String
checkSingleDeclaration d = concat $ fmap (f . unLoc) d

showLocated :: HsModule -> String
showLocated m = checkSingleDeclaration $ hsmodDecls m

showAll :: HsParsedModule -> String
showAll (HsParsedModule m _) = showLocated $ unLoc m

parsedPlugin :: [CommandLineOption] -> ModSummary
             -> ParsedResult -> Hsc ParsedResult
parsedPlugin _ _ parsed@(ParsedResult pm _)
     = do liftIO $ putStrLn $ "Test Generator \n"
          liftIO $ putStrLn $ showAll pm
          return parsed


--NON USED PLUGINS

renamedAction :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction _ tc gr = do return (tc, gr)

typecheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckPlugin _ _ tc
  = do return tc

metaPlugin :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
metaPlugin _ meta
  = do return meta

interfaceLoadPlugin :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
interfaceLoadPlugin _ iface
  = do return iface