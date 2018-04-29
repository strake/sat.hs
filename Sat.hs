module Sat where

import Prelude hiding (filter)
import Control.Applicative hiding (Const (..))
import Control.Monad ((>=>), (<=<), MonadPlus, guard)
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.Strict
import Data.Bool
import Data.CNF
import Data.Filtrable
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.Hashable
import Data.Monoid (Sum (..))
import Data.Only
import Data.Word (Word)
import Lens
import Util
import qualified Util.HashMap.Strict as HM

solve :: (Eq v, Hashable v, Filtrable f, Traversable f, MonadPlus p) => CNF HashMap f v -> p (HashMap v Bool)
solve = (fmap . fmap) only . execWriterT . solve'
  where
    solve' = solve'' <=< propagateUnits <=< elimLiterals

    solve'' cnf@(CNF x)
      | null x = pure ()
      | all null x = empty
      | Just v <- mostUsedVar cnf = foldr (\ a -> (<|>) $ tell (HM.singleton v (Only a)) *> solve' (plug1 v a cnf)) (pure ()) [False, True]
      | otherwise = error "This can't be happening"

plug1 :: (Eq v, Hashable v, Filtrable f) => v -> Bool -> CNF HashMap f v -> CNF HashMap f v
plug1 = plug ∘∘ HM.singleton

plug :: (Eq v, Hashable v, Filtrable f) => HashMap v Bool -> CNF HashMap f v -> CNF HashMap f v
plug = over cnfLens . mapMaybe . plugClause

plugClause :: (Eq v, Hashable v, Eq a) => HashMap v a -> HashMap v a -> Maybe (HashMap v a)
plugClause = flip . HM.differenceWithM $ \ a b -> Nothing <$ guard (a /= b)

literals :: (Eq v, Hashable v, Foldable f) => CNF HashMap f v -> HashMap v Bool
literals = foldr (HM.unionWithMaybe $ \ a b -> a <$ guard (a == b)) HM.empty . unCNF

elimLiterals :: (Eq v, Hashable v, Filtrable f, Foldable f, Monad m)
             => CNF HashMap f v -> WriterT (HashMap v (Only Bool)) m (CNF HashMap f v)
elimLiterals (CNF x) = CNF (filter (null . HM.intersection ls) x) <$ tell (Only <$> ls)
  where ls = literals (CNF x)

propagateUnits :: (Eq v, Hashable v, Filtrable f, Traversable f, Monad p) => CNF HashMap f v -> WriterT (HashMap v (Only Bool)) p (CNF HashMap f v)
propagateUnits = WriterT . flip runAccumT mempty . untilNoAccum propagateUnits1
  where untilNoAccum :: (Eq b, Monoid b, Monad m) => (a -> AccumT b m a) -> a -> AccumT b m a
        untilNoAccum f x = do
            before <- look
            y <- f x
            after <- look
            bool (untilNoAccum f) pure (before == after) y

propagateUnits1 :: (Eq v, Hashable v, Filtrable f, Traversable f, Monad p) => CNF HashMap f v -> AccumT (HashMap v (Only Bool)) p (CNF HashMap f v)
propagateUnits1 = cnfLens . mapMaybeA . (.) runMaybeT $
                  MaybeT . looks . (. fmap only) . flip plugClause >=> \ x -> case HM.size x of
    1 -> MaybeT $ Nothing <$ add (Only <$> x)
    _ -> pure x

vars :: (Eq v, Hashable v, Foldable f) => CNF HashMap f v -> HashSet v
vars = unCNF & foldMap HM.keysSet

mostUsedVar :: (Eq v, Hashable v, Foldable f) => CNF HashMap f v -> Maybe v
mostUsedVar = unCNF & foldMap ((,) <$> (Sum (1 :: Word) <$) <*> HM.keysSet) & \ (counts, vars) ->
              maximumBy (compare `on` flip HM.lookup counts) vars
