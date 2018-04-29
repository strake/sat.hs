module Data.CNF where

import Data.Function (on)
import Data.Functor.Classes
import Data.HashMap.Strict (HashMap)
import qualified Util.HashMap.Strict as HM

-- no tautologies!
newtype CNF m f v = CNF { unCNF :: f (m v Bool) }
instance (Eq1 f, Eq1 (m v)) => Eq (CNF m f v) where (==) = (liftEq . liftEq) (==) `on` unCNF
instance (Show1 f, Show1 (m v)) => Show (CNF m f v) where showsPrec n = liftShowsPrec showsPrec1 (liftShowList showsPrec showList) n . unCNF

cnfLens :: Functor p => (f (m u Bool) -> p (g (n v Bool))) -> CNF m f u -> p (CNF n g v)
cnfLens f = fmap CNF . f . unCNF

eval :: Foldable f => (v -> Bool) -> CNF HashMap f v -> Bool
eval f = (all . HM.anyWithKey) ((==) . f) . unCNF
