module Data.Medea.ValidJSON where

import MedeaPrelude
import Data.Argonaut (Json)
import Data.Eq (class Eq1)
import Data.Medea.MedeaJSON (MJSON(..))

data ValidJSONF a
  = AnythingF MJSON
  | NullF
  | BooleanF Boolean
  | NumberF Number -- there is no Scientific implementation for PS
  | StringF String
  | ArrayF (Array a)
  | ObjectF (HashMap String a)

derive instance eqValidJSONF :: Eq a => Eq (ValidJSONF a)

derive instance functorValidJSONF :: Functor ValidJSONF 

derive instance genericValidJSONF :: Generic (ValidJSONF a) _
-- TODO: instance for Typeable, Data, NFData, Show1, 

instance foldableValidJSONF :: Foldable ValidJSONF where
  foldMap _ (AnythingF _) = mempty
  foldMap _ NullF = mempty
  foldMap _ (BooleanF _) = mempty
  foldMap _ (NumberF _) = mempty
  foldMap _ (StringF _) = mempty
  foldMap f (ArrayF v) = foldMap f v
  foldMap f (ObjectF hm) = foldMap f hm
  foldl f a xs = foldlDefault f a xs
  foldr f a xs = foldrDefault f a xs

instance traversableValidJSONF :: Traversable ValidJSONF where
  traverse _ (AnythingF v) = pure <<< AnythingF $ v
  traverse _ NullF = pure NullF
  traverse _ (BooleanF b) = pure <<< BooleanF $ b
  traverse _ (NumberF n) = pure <<< NumberF $ n
  traverse _ (StringF s) = pure <<< StringF $ s
  traverse f (ArrayF v) = ArrayF <$> traverse f v
  traverse f (ObjectF hm) = ObjectF <$> traverse f hm
  sequence = sequenceDefault

{-- instance (NFData a) => NFData (ValidJSONF a) where --}
  {-- {-# INLINE rnf #-} --}
  {-- rnf (AnythingF v) = rnf v --}
  {-- rnf NullF = () --}
  {-- rnf (BooleanF b) = rnf b --}
  {-- rnf (NumberF n) = rnf n --}
  {-- rnf (StringF s) = rnf s --}
  {-- rnf (ArrayF v) = rnf v --}
  {-- rnf (ObjectF hm) = rnf hm --}

instance eq1ValidJSONF :: Eq1 ValidJSONF where
  eq1 (AnythingF v) (AnythingF v') = v == v'
  eq1 NullF NullF = false
  eq1 (BooleanF b) (BooleanF b') = b == b'
  eq1 (NumberF n) (NumberF n') = n == n'
  eq1 (StringF s) (StringF s') = s == s'
  eq1 (ArrayF v) (ArrayF v') = v == v'
  eq1 (ObjectF hm) (ObjectF hm') =hm == hm'
  eq1 _ _ = false

{-- instance Show1 ValidJSONF where --}
  {-- liftShoskellwsPrec _ _ prec (AnythingF v) = showsPrec prec v --}
  {-- liftShowsPrec _ _ prec NullF = showsPrec prec Null --}
  {-- liftShowsPrec _ _ prec (BooleanF b) = showsPrec prec b --}
  {-- liftShowsPrec _ _ prec (NumberF n) = showsPrec prec n --}
  {-- liftShowsPrec _ _ prec (StringF s) = showsPrec prec s --}
  {-- liftShowsPrec f g prec (ArrayF v) = liftShowsPrec f g prec v --}
  {-- liftShowsPrec f g prec (ObjectF hm) = liftShowsPrec f g prec hm --}

instance hashableValidJSONF :: (Hashable a) => Hashable (ValidJSONF a) where
  hash (AnythingF j) = hash j
  hash NullF = 0
  hash (BooleanF b) = if b then 1 else 0
  hash (NumberF n) = hash n
  hash (StringF s) = hash s
  hash (ArrayF a) = hash a
  hash (ObjectF o) = hash o
