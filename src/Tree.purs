module Tree where

import Prelude

import Control.Fold (Fold, foldl, unfoldFold_)
import Control.Monad.Except (ExceptT, except, lift)
import Data.Array (drop, take)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.NonEmpty (fromList, toList)
import Data.List.Types (NonEmptyList)
import Data.Map (SemigroupMap(..), empty)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (sequence)
import Data.Tree (Tree, Forest, mkTree)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.CSV as CSV

type Node a = 
  { code :: String 
  , name :: String
  | a 
  }

newtype TreeNode = TreeNode (Node ())

instance Show TreeNode where
  show (TreeNode node) = node.name <> "(" <> node.code <> ")"

derive newtype instance Eq TreeNode
derive newtype instance Ord TreeNode


type Validated = { isValid :: Boolean }

newtype ValidatedNode = ValidatedNode (Node ( isValid :: Boolean ))
instance Show ValidatedNode where
  show (ValidatedNode node) = node.code <> " (" <> node.name <> ")"

derive newtype instance Eq ValidatedNode
derive newtype instance Ord ValidatedNode

isValid :: ValidatedNode -> Boolean
isValid (ValidatedNode n) = n.isValid

derive instance Newtype ValidatedNode _

root :: TreeNode
root = TreeNode { code: "All", name: "All" }

type HierarchyList = NonEmptyList TreeNode

newtype TreeBuilder = TreeBuilder (SemigroupMap TreeNode TreeBuilder)
derive instance Newtype TreeBuilder _

emptyTreeBuilder :: TreeBuilder
emptyTreeBuilder = TreeBuilder (SemigroupMap empty)

parseTree :: ExceptT String Effect (Tree TreeNode)
parseTree = do
  file <- lift $ readTextFile UTF8 "./geo3.csv"
  csv <- except $ lmap _.error $ CSV.parse file
  hier <- except <<< sequence $ note "Row does not have enough columns to build tree" <<< fromList <<< toHierarchy <$> csv

  let result = foldl builderFold $ toList <$> hier
  let tree = mkTree root $ toTree result

  pure tree

  where
  toHierarchy :: Array String -> List TreeNode
  toHierarchy [] = Nil
  toHierarchy xs = case take 2 xs of 
    [code, name] -> TreeNode { code, name } : toHierarchy (drop 2 xs)
    _ -> Nil

  toTree :: TreeBuilder -> Forest TreeNode
  toTree (TreeBuilder (SemigroupMap m)) = (\(Tuple k v) -> mkTree k $ toTree v) <$> M.toUnfoldable m


builderFold :: Fold (List TreeNode) TreeBuilder
builderFold = unfoldFold_ emptyTreeBuilder $ flip addNodes

addNodes :: List TreeNode -> TreeBuilder -> TreeBuilder
addNodes Nil ms = ms
addNodes (x:y:Nil) (TreeBuilder m) = TreeBuilder <<< SemigroupMap $ M.alter (pure <<< insertBuilder y <<< fromMaybe emptyTreeBuilder) x (unwrap m)
addNodes xs@(x:_) (TreeBuilder m) = TreeBuilder <<< SemigroupMap $ M.alter (pure <<< addNodes (L.drop 1 xs) <<< fromMaybe emptyTreeBuilder) x (unwrap m)

insertBuilder :: TreeNode -> TreeBuilder -> TreeBuilder
insertBuilder node (TreeBuilder (SemigroupMap m)) = TreeBuilder <<< SemigroupMap $ M.insert node emptyTreeBuilder m