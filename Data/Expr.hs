module Data.Expr where

import Control.Applicative hiding (Const (..))
import Control.Monad (guard)
import Data.CNF
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Util.HashMap.Strict as HM

data Expr v
    = Var v
    | Expr v :∧: Expr v
    | Expr v :∨: Expr v
    | Not (Expr v)
    | Const Bool
  deriving (Eq, Show)

toCNF :: (Eq v, Hashable v, Alternative f) => Expr v -> CNF HashMap f v
toCNF = CNF . go
  where
    go = \ case
        Var v -> pure $ HM.singleton v True
        Not (Not x) -> go x
        Not (Var v) -> pure $ HM.singleton v False
        Not (Const a) -> go (Const (not a))
        Not (x :∧: y) -> go (Not x :∨: Not y)
        Not (x :∨: y) -> go (Not x :∧: Not y)
        x :∧: y -> go x <|> go y
        x :∨: y -> (liftA2 . HM.unionWithMaybe $ \ a b -> a <$ guard (a == b)) (go x) (go y)
        Const False -> pure HM.empty
        Const True -> empty
