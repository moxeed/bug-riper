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

import Debug.Trace

data AST = AST Span String [AST] [String]
  deriving (Show)

data DefUse = DefUse Span [String] [String] [DefUse]
  deriving (Show)

showNodes = ["HsIf", "FunBind", "HsVar", "HsOverLit", "HsCase", "VarPat", "NPat", "WildPat", "HsApp", "ConPat"]
trimNodes = ["TypeSig"]

isMember :: [NodeAnnotation] -> [String] -> Bool
isMember [] _ = False
isMember _ [] = False
isMember nodes list = len > 0
  where len = length intersectData
        intersectData = L.intersect list items
        items = fmap (unpackFS . nodeAnnotConstr) nodes

analyzeIdentifer :: Identifier -> IdentifierDetails a -> String
analyzeIdentifer name details = if '$' `elem` out then "" else out  -- ++ (show $ length $ identInfo $ details)
  where out = showSDocUnsafe $ ppr nameData
        Right nameData = name

getType :: [NodeAnnotation] -> String
getType [] = ""
getType ax = unpackFS . nodeAnnotConstr $ head ax

analyzeAst :: HieAST a -> AST
analyzeAst ast = if shouldTrim then AST codeSpan nodeType [] contextStrings
    else AST codeSpan nodeType childrenNode contextStrings
  where codeSpan = nodeSpan ast

        sNodeInfo = getSourcedNodeInfo $ sourcedNodeInfo ast
        Just nodeInfo = M.lookup SourceInfo sNodeInfo
        identifiers = nodeIdentifiers nodeInfo
        anotations = S.toList $ nodeAnnotations nodeInfo

        nodeType = getType anotations

        contextStrings = filter (/= "") $ (M.elems . (M.mapWithKey analyzeIdentifer)) identifiers

        children = nodeChildren ast
        childrenNode = fmap analyzeAst children

        shouldIgnore = (length anotations > 0) && (not $ isMember anotations showNodes)
        shouldTrim = isMember anotations trimNodes

printAST :: String -> AST -> String
printAST t (AST span nodeType [] [name]) = t ++ (show span) ++ ":" ++ nodeType ++ ":" ++ (name) ++ "\n" 
printAST t (AST span nodeType children context) = t ++ (show span) ++ ":" ++ nodeType ++ ":" ++ (show context) ++
  "\n" ++ (concat $ fmap (printAST ('\t':t)) children)

convertToDefUse :: AST -> DefUse -> DefUse
convertToDefUse (AST _ "HsVar"   _ [name]) (DefUse span def use children) = DefUse span def (name:use) children
convertToDefUse (AST _ "VarPat"  _ [name]) (DefUse span def use children) = DefUse span (name:def) use children
convertToDefUse (AST _ "WildPat" _ [name]) (DefUse span def use children) = DefUse span (name:def) use children
convertToDefUse (AST _ ""        _ [name]) (DefUse span def use children) = DefUse span (name:def) use children

convertToDefUse ast@(AST _ "FunBind" _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)
convertToDefUse ast@(AST _ "Match"   _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)
convertToDefUse ast@(AST _ "HsApp"   _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)
convertToDefUse ast@(AST _ "HsIf"    _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)
convertToDefUse ast@(AST _ "HsCase"  _ _)  (DefUse span def use children) = DefUse span def use ((convertToDefUses ast):children)

convertToDefUse (AST _ "GRHS" children _) defUse = foldr convertToDefUse defUse children
convertToDefUse (AST _ "ConPat" children _) defUse = foldr convertToDefUse defUse children
convertToDefUse (AST _ "ParPat" children _) defUse = foldr convertToDefUse defUse children

convertToDefUse _ defUse = defUse

convertToDefUses :: AST -> DefUse
convertToDefUses (AST s _ children _) = foldr convertToDefUse (DefUse s [] [] []) children

analyzeAsts :: HieFileResult -> String
analyzeAsts hieFileResult = show $ fmap (convertToDefUses . analyzeAst) asts
--analyzeAsts hieFileResult = concat $ fmap ((printAST "") . analyzeAst) asts
  where asts = getAsts $ hie_asts hieFile
        hieFile = hie_file_result hieFileResult

analyze :: String -> IO String
analyze file = do 
  nameCache <- initNameCache 'a' []
  hieFile <- readHieFile nameCache file
  return $ analyzeAsts hieFile