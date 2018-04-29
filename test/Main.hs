{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Trans.Writer.Strict
import Data.CNF
import Data.Filtrable
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.Hashable
import Data.Only
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck
import Util

import Sat

equiv :: ∀ v f . (Eq v, Hashable v, Foldable f) => CNF HashMap f v -> CNF HashMap f v -> Bool
equiv x y = (\ ψ -> ((==) `on` eval (\ v -> HM.lookupDefault False v ψ)) x y) `all`
            sequence ([False, True] <$ toMap vs)
  where vs = vars x <> vars y

isTransformationValid :: ∀ v f . (Eq v, Hashable v, Foldable f, Filtrable f)
                      => (CNF HashMap f v -> Writer (HashMap v (Only Bool)) (CNF HashMap f v)) -> CNF HashMap f v -> Bool
isTransformationValid f x = case runWriter $ f x of (y, ψ) -> equiv y $ plug (only <$> ψ) x

toMap :: (Eq k, Hashable k) => HashSet k -> HashMap k ()
toMap = foldMap (flip HM.singleton ())

main :: IO ()
main = defaultMain $ testGroup ""
    [testProperty "plug1" $ \ v a (x :: CNF HashMap [] Char) -> v ∉ vars (plug1 v a x),
     testProperty "literals" $ \ (x :: CNF HashMap [] Char) ->
     flip allWithKey (literals x) $ \ v a -> all ((==) a) $ HM.lookup v `mapMaybe` unCNF x,
     testGroup "transformations preserve equivalence" $
     (uncurry $ \ name -> testProperty name . isTransformationValid @Char @[]) <$>
     [("elimLiterals", elimLiterals),
      ("propagateUnits", propagateUnits)],
     testProperty "solve" $ \ (x :: CNF HashMap [] Char) -> all @[] (\ ψ -> null . unCNF $ plug ψ x) (solve x)]

allWithKey :: (k -> a -> Bool) -> HashMap k a -> Bool
allWithKey f = HM.foldrWithKey ((&&) ∘∘ f) True

instance (Monad p, Serial p (f (m v Bool))) => Serial p (CNF m f v) where series = CNF <$> series
instance (Monad p, Eq k, Hashable k, Serial p k, Serial p a) => Serial p (HashMap k a) where series = HM.fromList <$> series
