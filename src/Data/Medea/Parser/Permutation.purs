module Data.Medea.Parser.Permutation where

import MedeaPrelude
import Control.Alternative (class Alternative, empty, (<|>))

data Permutation m a = P (Maybe a) (m (Permutation m a))

instance functorPermutation :: Functor m => Functor (Permutation m) where
  map f (P v p) = P (f <$> v) (map f <$> p)

instance applyPermutation :: Alternative m => Apply (Permutation m) where
  apply lhs@(P f v) rhs@(P g w) = P (f <*> g) (lhsAlt <|> rhsAlt)
    where
      lhsAlt = (_ <*> rhs) <$> v
      rhsAlt = (lhs <*> _) <$> w

instance applicativePermutation :: Alternative m => Applicative (Permutation m) where
  pure value = P (Just value) (empty)

runPermutation :: forall a m. Alternative m => Monad m => Permutation m a -> m a
runPermutation (P value parser) = optional parser >>= f
  where 
    f Nothing = maybe empty pure value
    f (Just p) = runPermutation p

toPermutationWithDefault :: forall m a. Alternative m => a -> m a -> Permutation m a
toPermutationWithDefault v p = P (Just v) $ pure <$> p
