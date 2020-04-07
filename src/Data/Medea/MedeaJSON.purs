module Data.Medea.MedeaJSON where

import MedeaPrelude
import Data.Argonaut (Json, stringify)

newtype MJSON = MJSON Json 

derive instance newtypeMJSON :: Newtype MJSON _

derive newtype instance eqMJSON :: Eq MJSON

derive newtype instance ordMJSON :: Ord MJSON

instance showMJSON :: Show MJSON where
  show j = stringify <<< unwrap $ j

instance hashableMJSON :: Hashable MJSON where
  hash j = hash $ show $ j
