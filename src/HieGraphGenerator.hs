module HieGraphGenerator
where

import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name.Cache
import GHC.Utils.Outputable
import GHC.Data.FastString
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

showNodes = ["HsIf", "FunBind", "HsVar", "HsOverLit", "HsCase", "VarPat", "NPat", "WildPat", "HsApp", "ConPat"]
trimNodes = ["TypeSig"]

isMember :: [NodeAnnotation] -> [String] -> Bool
isMember [] _ = False
isMember _ [] = False
isMember nodes list = len > 0
  where len = length intersectData
        intersectData = L.intersect list items
        items = fmap (unpackFS . nodeAnnotConstr) nodes

getType :: [NodeAnnotation] -> String
getType [] = "Unkonwn"
getType nodes = t
  where (t:_) = L.intersect showNodes items
        items = fmap (unpackFS . nodeAnnotConstr) nodes

analyzeIdentifer :: Identifier -> IdentifierDetails a -> String
analyzeIdentifer name details = if '$' `elem` out then "" else out  -- ++ (show $ length $ identInfo $ details)
  where out = showSDocUnsafe $ ppr nameData
        Right nameData = name

showNodeAnotations :: [NodeAnnotation] -> String
showNodeAnotations (x:ax) = (show aConstr) ++ "," ++ showNodeAnotations ax
  where aConstr = nodeAnnotConstr x
        aType = nodeAnnotType x
showNodeAnotations [] = ""

-- createGraph String -> [HieAST a] -> [String]
-- createGraph "HsIf" children

analyzeAst :: HieAST a -> [String]
analyzeAst ast = if shouldTrim then []
    else if shouldIgnore 
    then (concat $ fmap analyzeAst children) 
    else [(show codeSpan) ++ "#" ++ (showNodeAnotations anotations) ++ "#" ++ contextStrings ++ "\n" ] ++ 
    (concat $ fmap analyzeAst children)
  where Just nodeInfo = M.lookup SourceInfo sNodeInfo -- Can Be GeneratedInfo Too
        sNodeInfo = getSourcedNodeInfo $ sourcedNodeInfo ast
        codeSpan = nodeSpan ast
        identifiers = nodeIdentifiers nodeInfo
        contextStrings = concat $ filter (/= "") $ (M.elems . (M.mapWithKey analyzeIdentifer)) identifiers
        children = nodeChildren ast
        anotations = S.toList $ nodeAnnotations nodeInfo
        shouldIgnore = False -- (length anotations > 0) && (not $ isMember anotations showNodes)
        shouldTrim = isMember anotations trimNodes
        nodeType = getType anotations
        
analyzeAsts :: HieFileResult -> String
analyzeAsts hieFileResult = concat $ concat $ fmap analyzeAst asts
  where asts = getAsts $ hie_asts hieFile
        hieFile = hie_file_result hieFileResult

analyze :: String -> IO String
analyze file = do 
  nameCache <- initNameCache 'a' []
  hieFile <- readHieFile nameCache file
  return $ analyzeAsts hieFile