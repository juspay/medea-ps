module Data.Medea.Parser.Spec.Schemata
  ( Specification(..)
  , parseSpecification
  ) where

import MedeaPrelude
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (eof)
import Data.Medea.Parser.Parsing (eol)
import Data.Medea.Parser.Types (MedeaParser)
import Data.Medea.Parser.Spec.Schema as Schema

newtype Specification
  = Specification (Array Schema.Specification)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  specs <- Schema.parseSpecification `sepBy1` eol
  eof
  pure <<< Specification $ fromFoldable $ specs
