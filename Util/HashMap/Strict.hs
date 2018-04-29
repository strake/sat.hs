module Util.HashMap.Strict where

import Prelude hiding (lookup)
import Data.Filtrable
import Data.Function (on)
import Data.HashMap.Strict as HM
import Data.Hashable
import Util

mapMaybeWithKeyA :: (Applicative p) => (k -> a -> p (Maybe b)) -> HashMap k a -> p (HashMap k b)
mapMaybeWithKeyA f = fmap catMaybes . traverseWithKey f

unionWithMaybe :: (Eq k, Hashable k) => (a -> a -> Maybe a) -> HashMap k a -> HashMap k a -> HashMap k a
unionWithMaybe f = catMaybes ∘∘ unionWith (bind2 f) `on` fmap pure

unionWithM :: (Eq k, Hashable k, Monad m) => (a -> a -> m a) -> HashMap k a -> HashMap k a -> m (HashMap k a)
unionWithM f = sequence ∘∘ unionWith (bind2 f) `on` fmap pure

differenceWithM :: (Eq k, Hashable k, Monad m) => (a -> b -> m (Maybe a)) -> HashMap k a -> HashMap k b -> m (HashMap k a)
differenceWithM f x y = foldrWithKeyM go empty x
  where go k a = case lookup k y of
            Nothing -> pure . insert k a
            Just b -> (<$> f a b) . (maybe <*> flip (insert k))

foldrWithKeyM :: Monad m => (k -> a -> b -> m b) -> b -> HashMap k a -> m b
foldrWithKeyM f z₀ xs = foldlWithKey' f' pure xs z₀ where f' c k x z = f k x z >>= c

foldMapWithKey :: Monoid b => (k -> a -> b) -> HashMap k a -> b
foldMapWithKey f = foldrWithKey ((<>) ∘∘ f) mempty

allWithKey, anyWithKey :: (k -> a -> Bool) -> HashMap k a -> Bool
allWithKey f = foldrWithKey ((&&) ∘∘ f) True
anyWithKey f = foldrWithKey ((||) ∘∘ f) False

instance Filtrable (HashMap k) where mapMaybe = HM.mapMaybe
