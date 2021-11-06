module Main where

import Prelude

import Control.Comonad.Cofree (head)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.List (List(..), length, nub)
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Tree (drawTree, mkTree)
import Effect (Effect)
import Effect.Console (log)
import Record as R
import Tree (TreeNode(..), appendAtLevel, firstBranch, joinTree, mapWithLevel, match, parseTree)
import Type.Proxy (Proxy(..))
import Validate (checkValid, isValid)

main :: Effect Unit
main = do
  r <- runExceptT parseTree
  case r of
    Left err -> log err
    Right tree -> do
      let newTree = appendAtLevel 2 (\_ t -> joinTree $ (\_ -> mkTree (TreeNode { code: (unwrap (head t)).code <> "-unassigned", name: "unassigned" }) Nil) <$> (fromMaybe Nil $ L.tail $ firstBranch t)) tree
      let validatedTree = checkValid newTree 
      log $ drawTree $ (\n -> if isValid n then show n else "\x1b[31m" <> show n <> "\x1b[0m") <$> validatedTree

      let validatedTreeWithLevel = mapWithLevel (R.insert (Proxy :: _ "level")) (unwrap <$> validatedTree)
      let invalidSubTrees = nub $ match (not <<< _.isValid) validatedTreeWithLevel
      log $ "There are " <> show (length invalidSubTrees) <> " invalid paths"
      log $ intercalate "\n" $ (\n -> n.code <> " (level " <> show n.level <> ")") <$> head <$> invalidSubTrees