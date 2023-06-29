module Data.Medea.Parser.Types
  ( MedeaParser
  , MedeaParseErr(..)
  ) where

import MedeaPrelude
import Parsing (Parser)

data MedeaParseErr
  = IdentifierTooLong String
  | ExpectedReservedIdentifier String
  | LeadingZero String
  | ConflictingSpecRequirements
  | EmptyLengthArraySpec
  | EmptyArrayElements
  | EmptyStringValueSpec

derive instance eqMedeaParseErr :: Eq MedeaParseErr

derive instance ordMedeaParseErr :: Ord MedeaParseErr

derive instance genericMedeaParseErr :: Generic MedeaParseErr _

instance showMedeaParseErr :: Show MedeaParseErr where
  show x = genericShow x

type MedeaParser
  = Parser String
