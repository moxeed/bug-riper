module HieDUJungleGenerator
where

import GHC.Iface.Ext.Types
import Text.Regex
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified HieASTGraphGenerator as AG
import qualified GHC.Utils.Outputable as GHC


data GraphNode = DefNode String [GraphNode] | UseNode Span [String] [GraphNode]
  deriving (Show)

convertToGraphNode :: AG.AST -> GraphNode -> GraphNode

convertToGraphNode (AG.AST _ "VarPat"  _ [name]  ) (DefNode defName children) = DefNode defName ((DefNode name []):children)

convertToGraphNode (AG.AST _ ""        _ [name]  ) (DefNode _ children) = DefNode name children
convertToGraphNode (AG.AST _ "FunBind" children _) (DefNode name nodeChildren) = DefNode name (funcNode:nodeChildren)
  where funcNode = foldr convertToGraphNode (DefNode "" []) children

convertToGraphNode (AG.AST _ "HsVar" _      [name]) (UseNode s uses children) = UseNode s (name:uses) children 
convertToGraphNode (AG.AST s "HsIf"  [cond, first@(AG.AST fs _ _ _), second@(AG.AST ss _ _ _)] _ ) graphNode = 
  addChild (addChild condNode firstNode) secondNode
  where 
    firstNode  = convertToGraphNode first  (UseNode fs [] [])
    secondNode = convertToGraphNode second (UseNode ss [] [])
    condNode = convertToGraphNode cond graphNode
    
convertToGraphNode (AG.AST _ "GRHS"  [child@(AG.AST s _ _ _)]  _   ) graphNode = addChild graphNode (convertToGraphNode child (UseNode s [] []))
convertToGraphNode (AG.AST _ _     children _   )   graphNode = foldr convertToGraphNode graphNode children

addChild :: GraphNode -> GraphNode -> GraphNode
addChild (DefNode name children) newChild  = DefNode name (newChild:children)
addChild (UseNode s uses children) newChild = UseNode s uses (newChild:children)

createDefMap :: GraphNode -> M.Map String GraphNode -> M.Map String GraphNode
createDefMap def@(DefNode name children) defMap  = M.insert name def childMap
  where childMap = foldr createDefMap defMap children
createDefMap (UseNode _ _ children) defMap = foldr createDefMap defMap children
createDefMap _ defMap = defMap

createUseArray :: GraphNode -> [GraphNode] -> [GraphNode]
createUseArray (UseNode _ [] children) useArray  = foldr createUseArray useArray children
createUseArray (DefNode _ children) useArray = foldr createUseArray useArray children
createUseArray (UseNode s uses children) useArray  = (UseNode s uses []):childArray
  where childArray = foldr createUseArray useArray children
createUseArray _ useArray = useArray

    
createDefPath :: GraphNode -> [String]
createDefPath (UseNode span uses children) = case children of
    [] -> (ppWhere):(subPaths)
    _  -> subPaths
  where
      subPaths = concat $ fmap ( fmap ((ppWhere ++ "->") ++) . createDefPath) children
      ppWhere = GHC.renderWithContext GHC.defaultSDocContext ( GHC.ppr span )
createDefPath _ = []

createUsePath :: M.Map String GraphNode -> GraphNode -> [String]
createUsePath defMap (UseNode span uses _) = concat $ fmap createPath uses
  where
    createPath use = case M.lookup use defMap of
      Just (DefNode _ children) -> createDefPath (UseNode span uses children)
      _ -> []
createUsePath _ _ = []

createDefUsePath :: GraphNode -> [String]
createDefUsePath node = concat $ fmap (createUsePath defMap) useArray
  where 
    defMap = createDefMap node M.empty
    useArray = createUseArray node []

convertToGraphNodeInit :: AG.AST -> GraphNode
convertToGraphNodeInit ast = convertToGraphNode ast (DefNode "" [])

uniq:: [String] -> [String]
uniq (a:ax) = if elem a ax then uniq ax else a:(uniq ax)
uniq [] = []

analyzePath :: String -> String -> String
analyzePath trace path = 
  case matchRegex (mkRegex regexString) trace of
       Just _ -> "\ESC[32mCovered: " ++ path
       _ -> "\ESC[31mNotCovered: " ++ path
  where 
    regexString = L.replace "->" ".*"  rParReplace
    rParReplace = L.replace ")" "[)]" lParReplace
    lParReplace = L.replace "(" "[(]" slashReplace
    slashReplace = L.replace "\\" "[\\]" path

analyze :: String -> String -> IO (String)
analyze file runFile = do 
  asts <- AG.loadAST file
  runData <- readFile runFile
  let 
    duPath = uniq $ concat $ fmap (createDefUsePath . convertToGraphNodeInit) asts
    trace = L.intercalate "->" (lines runData)
  return $ (L.intercalate "\n" $ fmap (analyzePath trace) duPath) ++ "\n"
    