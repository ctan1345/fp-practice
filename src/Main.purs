module Main where

import Prelude

import Control.Comonad.Cofree (head, tail, (:<))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.List (List, length, nub, (:))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive)
import Data.Newtype (unwrap)
import Data.Tree (Tree, drawTree)
import Effect (Effect)
import Effect.Console (log)
import Record as R
import Tree (ValidatedNode(..), TreeNode(..), isValid)
import Tree as Tree
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  r <- runExceptT Tree.parseTree
  case r of
    Left err -> log err
    Right tree -> do
      let validatedTree = checkValid tree 
      log $ drawTree $ (\n -> if isValid n then show n else "\x1b[31m" <> show n <> "\x1b[0m") <$> validatedTree

      let validatedTreeWithLevel = mapWithLevel (R.insert (Proxy :: _ "level")) (unwrap <$> validatedTree)
      let invalidSubTrees = nub $ match (not <<< _.isValid) validatedTreeWithLevel
      log $ "There are " <> show (length invalidSubTrees) <> " invalid paths"
      log $ intercalate "\n" $ (\n -> n.code <> " (level " <> show n.level <> ")") <$> head <$> invalidSubTrees

type ParentByChildMap = Map { level :: Int, code :: String } (Additive Int)

checkValid :: Tree TreeNode -> Tree ValidatedNode
checkValid tree = mapWithLevel (validate (parentByChild tree)) tree
  where
  parentByChild :: Tree TreeNode -> ParentByChildMap
  parentByChild = foldWithLevel accumParentByChild M.empty

  accumParentByChild :: Int -> ParentByChildMap -> TreeNode -> ParentByChildMap
  accumParentByChild i m (TreeNode node) = M.alter (pure <<< append (pure 1) <<< fromMaybe mempty) { level: i, code: node.code } m

  validate :: ParentByChildMap -> Int -> TreeNode -> ValidatedNode
  validate m i (TreeNode node) = ValidatedNode $ R.insert (Proxy :: _ "isValid") (fromMaybe false $ (>) 2 <<< unwrap <$> M.lookup { level: i, code: node.code } m) node

foldWithLevel :: ∀ a b. (Int -> b -> a -> b) -> b -> Tree a -> b
foldWithLevel = go 0
  where
  go :: Int -> (Int -> b -> a -> b) -> b -> Tree a -> b
  go n f b t = let b' = (f n b $ head t) in foldl (go (n + 1) f) b' (tail t)

mapWithLevel :: ∀ a b. (Int -> a -> b) -> Tree a -> Tree b
mapWithLevel = go 0
  where
  go :: Int -> (Int -> a -> b) -> Tree a -> Tree b
  go n f t = (f n $ head t) :< ((go (n + 1) f) <$> tail t)

match :: ∀ a. (a -> Boolean) -> Tree a -> List (Tree a)
match f t = 
  let h = head t
  in if f h then t : ((match f) =<< tail t) else match f =<< tail t