module HieReader where

import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name.Cache
import GHC.Utils.Outputable
import GHC.Data.FastString
import qualified Data.Map as M
import qualified Data.Set as S

showNodes = ["HsIf", "FunBind", "HsVar", "HsOverLit", "HsCase", "VarPat", "Match", "GRHS"]

ignore :: [NodeAnnotation] -> Bool
--ignore _ = False
ignore [x] = aConstr `notElem` showNodes
  where aConstr = unpackFS $ nodeAnnotConstr x
ignore (x:ax) = aConstr `notElem` showNodes && ignore ax
  where aConstr = unpackFS $ nodeAnnotConstr x
ignore [] = False

analyzeIdentifer :: Identifier -> IdentifierDetails a -> String
analyzeIdentifer name details = if '$' `elem` out then "" else out  -- ++ (show $ length $ identInfo $ details)
  where out = showSDocUnsafe $ ppr nameData
        Right nameData = name

showNodeAnotations :: [NodeAnnotation] -> String
showNodeAnotations (x:ax) = if shouldShow then (show aConstr) ++ "," ++ showNodeAnotations ax
    else showNodeAnotations ax
  where aConstr = nodeAnnotConstr x
        aType = nodeAnnotType x
        shouldShow = (unpackFS aConstr) `elem` showNodes
showNodeAnotations [] = "\n"

analyzeAst :: HieAST a -> [String]
analyzeAst ast = if shouldIgnore 
    then (concat $ fmap analyzeAst children) 
    else [(show codeSpan),  ":{\n"] ++ 
    [(showNodeAnotations anotations)] ++
    [contextStrings ++ "\n"] ++
    ( fmap ("\t" ++) $ concat $ fmap analyzeAst children) ++
    ["}\n"]
  where Just nodeInfo = M.lookup SourceInfo sNodeInfo -- Can Be GeneratedInfo Too
        sNodeInfo = getSourcedNodeInfo $ sourcedNodeInfo ast
        codeSpan = nodeSpan ast
        identifiers = nodeIdentifiers nodeInfo
        contextStrings = concat $ filter (/= "") $ (M.elems . (M.mapWithKey analyzeIdentifer)) identifiers
        children = nodeChildren ast
        anotations = S.toList $ nodeAnnotations nodeInfo
        shouldIgnore = ignore anotations 
        
analyzeAsts :: HieFileResult -> String
analyzeAsts hieFileResult = concat $ concat $ fmap analyzeAst asts
  where asts = getAsts $ hie_asts hieFile
        hieFile = hie_file_result hieFileResult

analyze :: String -> IO String
analyze file = do 
  nameCache <- initNameCache 'a' []
  hieFile <- readHieFile nameCache file
  return $ analyzeAsts hieFile

