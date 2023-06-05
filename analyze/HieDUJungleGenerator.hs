module HieDUJungleGenerator
where

import GHC.Iface.Ext.Types
import qualified Data.Map as M
import qualified Data.List as L
import qualified HieASTGraphGenerator as AG

data GraphNode = DefNode String [GraphNode] | UseNode Span [String] [GraphNode] | FunName String | Null
  deriving (Show)

convertToGraphNode :: AG.AST -> GraphNode

convertToGraphNode (AG.AST _ "VarPat"  _ [name]  ) = DefNode name []

convertToGraphNode (AG.AST _ ""        _ [name]  ) = FunName name
convertToGraphNode (AG.AST _ "FunBind" children _) = foldr createFunDeclare (DefNode "" []) children
convertToGraphNode (AG.AST _ "FunBind" children _) = foldr createFunDeclare (DefNode "" []) children

convertToGraphNode (AG.AST s "HsVar" _        name) = UseNode s name []
convertToGraphNode (AG.AST s "HsIf"  children _   ) = UseNode s [] (fmap convertToGraphNode children)
convertToGraphNode (AG.AST s "GRHS"  [child]  _   ) = convertToGraphNode child
convertToGraphNode (AG.AST s _       children _   ) = foldr createUseNode (UseNode s [] []) children

createUseNode :: AG.AST -> GraphNode -> GraphNode
createUseNode child use@(UseNode s uses children) =
  case childNode of
    UseNode _ newUses newChildren -> UseNode s (uses++newUses) (children++newChildren)
    Null -> use
    other -> UseNode s uses (other:children)
  where childNode = convertToGraphNode child  
createUseNode _ use = use

createFunDeclare :: AG.AST -> GraphNode -> GraphNode
createFunDeclare child def@(DefNode name children) =
  case childNode of
    FunName funName -> DefNode funName children
    Null -> def
    other -> DefNode name (other:children)
  where childNode = convertToGraphNode child
createFunDeclare _ def = def

createDefMap :: GraphNode -> M.Map String GraphNode -> M.Map String GraphNode
createDefMap def@(DefNode name children) defMap  = M.insert name def childMap
  where childMap = foldr createDefMap defMap children
createDefMap (UseNode _ _ children) defMap = foldr createDefMap defMap children
createDefMap _ defMap = defMap

createUseArray :: GraphNode -> [GraphNode] -> [GraphNode]
createUseArray (UseNode s uses children) useArray  = (UseNode s uses []):childArray
  where childArray = foldr createUseArray useArray children
createUseArray (DefNode _ children) useArray = foldr createUseArray useArray children
createUseArray _ useArray = useArray

createUsePath :: M.Map String GraphNode -> GraphNode -> [String]
createUsePath defMap (UseNode span uses _) = (show span):(concat $ fmap createPath uses)
  where
    createPath use = case M.lookup use defMap of
      Just (DefNode _ children) -> concat $ fmap (\x -> fmap (\y -> (show span) ++ "->" ++ y) $ createUsePath defMap x) children
      _ -> []
createUsePath _ _ = []

createDefUsePath :: GraphNode -> [String]
createDefUsePath node = concat $ fmap (createUsePath defMap) useArray
  where 
    defMap = createDefMap node M.empty
    useArray = createUseArray node []

analyze :: String -> IO (String)
analyze file = do 
  asts <- AG.loadAST file
  return $ L.intercalate "\n" $ concat $ fmap (createDefUsePath . convertToGraphNode) asts
  --return $ show $ concat $ fmap ((\x -> createUseArray x []) . convertToGraphNode) asts