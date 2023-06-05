module HieDUGraphGenerator
where

import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name.Cache
import GHC.Utils.Outputable
import GHC.Data.FastString
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified HieASTGraphGenerator as AG

import Debug.Trace

data DefUse = DefUse Span [String] [String] [DefUse]
  deriving (Show)

convertToDefUse :: AG.AST -> DefUse -> DefUse
convertToDefUse (AG.AST _ "HsVar"   _ [name]) (DefUse span def use children) = DefUse span def (name:use) children
convertToDefUse (AG.AST _ "VarPat"  _ [name]) (DefUse span def use children) = DefUse span (name:def) use children
convertToDefUse (AG.AST _ "WildPat" _ [name]) (DefUse span def use children) = DefUse span (name:def) use children
--Function Name
convertToDefUse (AG.AST _ ""        _ [name]) (DefUse span def use children) = DefUse span (name:def) (name:use) children

convertToDefUse ast@(AG.AST _ "FunBind" _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)
convertToDefUse ast@(AG.AST _ "Match"   _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)
convertToDefUse ast@(AG.AST _ "HsApp"   _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)
convertToDefUse ast@(AG.AST _ "HsIf"    _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)
convertToDefUse ast@(AG.AST _ "HsCase"  _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)

convertToDefUse (AG.AST _ "GRHS"    children _) defUse = foldr convertToDefUse defUse children
convertToDefUse (AG.AST _ "ConPat"  children _) defUse = foldr convertToDefUse defUse children
convertToDefUse (AG.AST _ "ParPat"  children _) defUse = foldr convertToDefUse defUse children
convertToDefUse (AG.AST _ "HsPar"   children _) defUse = foldr convertToDefUse defUse children

convertToDefUse _ defUse = defUse

convertToDefUses :: AG.AST -> DefUse
convertToDefUses (AG.AST s _ children _) = foldr convertToDefUse (DefUse s [] [] []) children

analyze :: String -> IO (String)
analyze file = do 
  asts <- AG.loadAST file
  return $ show $ fmap convertToDefUses asts
