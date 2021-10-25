module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Tree as Tree

main :: Effect Unit
main = do
  r <- runExceptT Tree.test
  case r of
    Left err -> log err
    Right x -> log x

  