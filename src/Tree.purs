module Tree where

import Data.Tree (Forest, drawTree, mkTree)
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
import Data.Map (SemigroupMap(..))
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.CSV as CSV

type Node = 
  { code :: String 
  , name :: String
  }

root :: Node
root = { code: "All", name: "All" }

type HierarchyList = NonEmptyList Node

data TreeBuilder = Tree (SemigroupMap Node TreeBuilder) | Leaf (Set Node)

instance Show TreeBuilder where
  show (Tree m) = show m
  show (Leaf l) = show l

instance Eq TreeBuilder where
  eq _ _ = false

instance Ord TreeBuilder where
  compare (Tree m) (Tree m') = compare m m'
  compare _ _ = LT

instance Monoid TreeBuilder where
  mempty = Tree mempty

instance Semigroup TreeBuilder where
  append (Tree m) (Tree m') = Tree $ append m m'
  append (Leaf l) (Leaf l') = Leaf $ append l l'
  append (Tree m) (Leaf l) = Tree <<< SemigroupMap $ M.union (unwrap m) $ M.fromFoldable $ S.map (\k -> Tuple k (Leaf mempty))  l
  append (Leaf l) (Tree map) = append (Tree map) (Leaf l)

test :: ExceptT String Effect String
test = do
  file <- lift $ readTextFile UTF8 "./geo2.csv"
  csv <- except $ lmap _.error $ CSV.parse file
  hier <- except <<< sequence $ note "Row does not have enough columns to build tree" <<< fromList <<< toHierarchy <$> take 80 csv

  let result = foldl builderFold $ toList <$> hier
  let tree = mkTree root $ toTree result

  pure $ drawTree $ _.code <$> tree

  where
  toHierarchy :: Array String -> List Node
  toHierarchy [] = Nil
  toHierarchy xs = case take 2 xs of 
    [code, name] -> { code, name } : toHierarchy (drop 2 xs)
    _ -> Nil

  toTree :: TreeBuilder -> Forest Node
  toTree (Tree (SemigroupMap m)) = (\(Tuple k v) -> mkTree k $ toTree v) <$> M.toUnfoldable m
  toTree (Leaf l) = flip mkTree Nil <$> S.toUnfoldable l


builderFold :: Fold (List Node) TreeBuilder
builderFold = unfoldFold_ mempty $ flip addNodes

addNodes :: List Node -> TreeBuilder -> TreeBuilder
addNodes Nil ms = ms
addNodes (x:y:Nil) (Tree m) = Tree <<< SemigroupMap $ M.alter (pure <<< insertBuilder y <<< fromMaybe mempty) x (unwrap m)
addNodes xs@(x:_:_) (Tree m) = Tree <<< SemigroupMap $ M.alter (pure <<< addNodes (L.drop 1 xs) <<< fromMaybe mempty) x (unwrap m)
addNodes _ (Leaf l) = Leaf $ l
addNodes _ _ = mempty

insertBuilder :: Node -> TreeBuilder -> TreeBuilder
insertBuilder node (Tree (SemigroupMap m)) = Tree <<< SemigroupMap $ M.insert node mempty m
insertBuilder node (Leaf l) = Leaf $ S.insert node l