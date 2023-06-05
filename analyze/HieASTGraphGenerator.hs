module HieASTGraphGenerator
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

importantNodes = ["HsIf", "FunBind", "HsVar", "HsOverLit", "HsCase", "VarPat"]
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
analyzeIdentifer name details = if '$' `elem` out then "" else out
  where out = case name of 
          (Right nameData) -> showSDocUnsafe $ ppr nameData
          _ -> ""

getType :: [NodeAnnotation] -> String
getType [] = ""
getType ax = if length importantTypes > 0 
    then head importantTypes 
    else head types
  where 
    types = fmap (unpackFS . nodeAnnotConstr) ax
    importantTypes = L.intersect types importantNodes

createAst :: HieAST a -> AST
createAst ast = if shouldTrim then AST codeSpan nodeType [] contextStrings
    else AST codeSpan nodeType childrenNode contextStrings
  where codeSpan = nodeSpan ast

        sNodeInfo = getSourcedNodeInfo $ sourcedNodeInfo ast
        Just nodeInfo = M.lookup SourceInfo sNodeInfo
        identifiers = nodeIdentifiers nodeInfo
        anotations = S.toList $ nodeAnnotations nodeInfo

        nodeType = getType anotations

        contextStrings = filter (/= "") $ (M.elems . (M.mapWithKey analyzeIdentifer)) identifiers

        children = nodeChildren ast
        childrenNode = fmap createAst children
        
        shouldTrim = isMember anotations trimNodes

printAST :: String -> AST -> String
printAST t (AST span nodeType [] [name]) = t ++ (show span) ++ ":" ++ nodeType ++ ":" ++ (name) ++ "\n" 
printAST t (AST span nodeType children context) = t ++ (show span) ++ ":" ++ nodeType ++ ":" ++ (show context) ++
  "\n" ++ (concat $ fmap (printAST ('\t':t)) children)
  
loadAST :: String -> IO([AST])
loadAST file = do 
  nameCache <- initNameCache 'a' []
  hieFile <- readHieFile nameCache file
  let
    hieFileResult = hie_file_result hieFile
    asts = getAsts $ hie_asts hieFileResult
  return $ M.elems $ fmap createAst asts

printAsts :: String -> IO(String)
printAsts file = do 
  asts <- loadAST file
  return $ concat $ fmap (printAST "") asts