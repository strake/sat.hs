module Lens where

import Data.Coerce
import Data.Functor.Identity

over :: ((a -> Identity b) -> α -> Identity β) -> (a -> b) -> α -> β
over = coerce
