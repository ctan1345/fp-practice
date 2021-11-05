module Zipper where

import Prelude

import Data.List (List, singleton)
import Data.Tree.Zipper (Loc, children, fromTree)

nodesAt :: ∀ a. Int -> Loc a -> List (Loc a)
nodesAt n loc | n > 0 = nodesAt (n - 1) <<< fromTree =<< children loc
nodesAt _ loc = singleton loc

