module Data.Medea.Parser.Spec.String where

import MedeaPrelude
import Unsafe.Coerce (unsafeCoerce)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (try)
import Data.Medea.Parser.Types (MedeaParser, MedeaParseErr(..))
import Data.Medea.Parser.Primitive (MedeaString, parseLine, parseReservedChunk, parseString)

newtype Specification = Specification (Array MedeaString)

derive newtype instance eqSpecification :: Eq Specification

derive instance newtypeSpecification :: Newtype Specification _

derive instance genericSpecification :: Generic Specification _

instance showSpecification :: Show Specification where
  show = genericShow

toReducedSpec :: Specification -> Array String
toReducedSpec spec = map unsafeCoerce $ (unsafeCoerce spec :: Array MedeaString)

defaultSpec :: Specification
defaultSpec = Specification []

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 $ parseReservedChunk "string_values"
  items <- many $ try $ parseLine 8 parseString
  if null items 
    then fail $ show EmptyStringValueSpec
    else pure $ Specification items
