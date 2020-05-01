module Data.Medea.Parser.Primitive
  ( Identifier(..)
  , MedeaString(..)
  , PrimTypeIdentifier(..)
  , ReservedIdentifier(..)
  , identFromReserved
  , isReserved
  , isStartIdent
  , parseIdentifier
  , parseKeyVal
  , parseLine
  , parseNatural
  , parseReserved
  , parseString
  , tryPrimType
  , typeOf
  ) where

import MedeaPrelude
import Control.Alternative ((<|>))
import Control.Safely (replicateM_)
import Data.Array as Array
import Data.Char.Unicode (isSeparator, isControl)
import Data.Int as Int
import Data.List (List)
import Data.Natural (Natural)
import Data.Natural as Nat
import Data.String (codePointFromChar, length, singleton) as String
import Data.String.CodeUnits (fromCharArray) as String
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (char, string, anyChar)
import Text.Parsing.Parser.Token (digit)
import Data.Medea.Parser.Parsing (takeWhile1P, eol)
import Data.Medea.JSONType (JSONType(..))
import Data.Medea.Parser.Types (MedeaParser, MedeaParseErr(..))

newtype Identifier
  = Identifier String

derive instance newtypeIdentifier :: Newtype Identifier _

derive instance eqIdentifier :: Eq Identifier

derive instance ordIdentifier :: Ord Identifier

derive instance genericIdentifier :: Generic Identifier _

instance showIdentifier :: Show Identifier where
  show x = genericShow x

parseIdentifier :: MedeaParser Identifier
parseIdentifier = do
  ident <- String.fromCharArray <$> takeWhile1P "Non-Seperator" (not <<< isSeperatorOrControl)
  checkedConstruct Identifier ident

data ReservedIdentifier
  = RSchema
  | RStart
  | RType
  | RStringValues
  | RProperties
  | RPropertyName
  | RPropertySchema
  | RAdditionalPropertiesAllowed
  | ROptionalProperty
  | RMinLength
  | RMaxLength
  | RElementType
  | RTuple
  | RArray
  | RBoolean
  | RNull
  | RNumber
  | RObject
  | RString

derive instance eqReservedIdentifier :: Eq ReservedIdentifier

derive instance genericReservedIdentifier :: Generic ReservedIdentifier _

instance showReservedIdentifier :: Show ReservedIdentifier where
  show x = genericShow x

fromReserved :: ReservedIdentifier -> String
fromReserved RSchema = "$schema"

fromReserved RStart = "$start"

fromReserved RType = "$type"

fromReserved RStringValues = "$string_values"

fromReserved RProperties = "$properties"

fromReserved RPropertyName = "$property-name"

fromReserved RPropertySchema = "$property-schema"

fromReserved RAdditionalPropertiesAllowed = "$additional-properties-allowed"

fromReserved ROptionalProperty = "$optional-property"

fromReserved RMinLength = "$min_length"

fromReserved RMaxLength = "$max_length"

fromReserved RElementType = "$element_type"

fromReserved RTuple = "$tuple"

fromReserved RArray = "$array"

fromReserved RBoolean = "$boolean"

fromReserved RNull = "$null"

fromReserved RNumber = "$number"

fromReserved RObject = "$object"

fromReserved RString = "$string"

identFromReserved :: ReservedIdentifier -> Identifier
identFromReserved = Identifier <<< fromReserved

parseReserved :: ReservedIdentifier -> MedeaParser Identifier
parseReserved reserved = do
  ident <- String.fromCharArray <$> takeWhile1P "Non-seperator" (not <<< isSeperatorOrControl)
  let
    reservedText = fromReserved reserved
  when (ident /= reservedText) $ fail $ show $ ExpectedReservedIdentifier $ reservedText
  checkedConstruct Identifier ident

tryReserved :: String -> Maybe ReservedIdentifier
tryReserved "$schema" = Just RSchema

tryReserved "$start" = Just RStart

tryReserved "$type" = Just RType

tryReserved "$string_values" = Just RStringValues

tryReserved "$properties" = Just RProperties

tryReserved "$property-name" = Just RPropertyName

tryReserved "$property-schema" = Just RPropertySchema

tryReserved "$additional-properties-allowed" = Just RAdditionalPropertiesAllowed

tryReserved "$optional-property" = Just ROptionalProperty

tryReserved "$min_length" = Just RMinLength

tryReserved "$max_length" = Just RMaxLength

tryReserved "$element_type" = Just RElementType

tryReserved "$tuple" = Just RTuple

tryReserved "$array" = Just RArray

tryReserved "$boolean" = Just RBoolean

tryReserved "$null" = Just RNull

tryReserved "$number" = Just RNumber

tryReserved "$object" = Just RObject

tryReserved "$string" = Just RString

tryReserved _ = Nothing

newtype PrimTypeIdentifier
  = PrimTypeIdentifier JSONType

derive instance eqPrimTypeIdentifier :: Eq PrimTypeIdentifier

typeOf :: PrimTypeIdentifier -> JSONType
typeOf (PrimTypeIdentifier jt) = jt

parsePrimType :: MedeaParser PrimTypeIdentifier
parsePrimType =
  PrimTypeIdentifier
    <$> ( string "$null" $> JSONNull
          <|> string "$boolean"
          $> JSONBoolean
          <|> string "$object"
          $> JSONObject
          <|> string "$array"
          $> JSONArray
          <|> string "$number"
          $> JSONNumber
          <|> string "$string"
          $> JSONString
      )

tryPrimType :: Identifier -> Maybe PrimTypeIdentifier
tryPrimType (Identifier ident) = tryReserved ident >>= reservedToPrim

reservedToPrim :: ReservedIdentifier -> Maybe PrimTypeIdentifier
reservedToPrim RNull = Just <<< PrimTypeIdentifier $ JSONNull

reservedToPrim RBoolean = Just <<< PrimTypeIdentifier $ JSONBoolean

reservedToPrim RObject = Just <<< PrimTypeIdentifier $ JSONObject

reservedToPrim RArray = Just <<< PrimTypeIdentifier $ JSONArray

reservedToPrim RNumber = Just <<< PrimTypeIdentifier $ JSONNumber

reservedToPrim RString = Just <<< PrimTypeIdentifier $ JSONString

reservedToPrim _ = Nothing

forgetPrimType :: PrimTypeIdentifier -> Identifier
forgetPrimType ident =
  Identifier
    $ case typeOf ident of
        JSONNull -> "$null"
        JSONBoolean -> "$boolean"
        JSONObject -> "$object"
        JSONArray -> "$array"
        JSONNumber -> "$number"
        JSONString -> "$string"

isReserved :: Identifier -> Boolean
isReserved = isJust <<< tryReserved <<< unwrap

isStartIdent :: Identifier -> Boolean
isStartIdent = (_ == Just RStart) <<< tryReserved <<< unwrap

parseNatural :: MedeaParser Natural
parseNatural = do
  digits <- many digit
  let
    digitString = String.fromCharArray digits
  case Array.uncons digits of
    Just ht -> case ht.head of
      '0' -> fail $ show $ LeadingZero $ digitString
      _ -> do
        let
          mInt = Int.fromString $ digitString
        case mInt of
          Just n -> pure $ Nat.intToNat n
          Nothing -> fail "failed to parse integer"
    Nothing -> fail "expected digits"

newtype MedeaString
  = MedeaString String

derive instance eqMedeaString :: Eq MedeaString

derive instance ordMedeaString :: Ord MedeaString

derive newtype instance showMedeaString :: Show MedeaString

derive instance newtypeMedeaString :: Newtype MedeaString _

derive newtype instance hashableMedeaString :: Hashable MedeaString

pack :: List Char -> String
pack = foldMap (String.singleton <<< String.codePointFromChar)

parseString :: MedeaParser MedeaString
parseString = do
  chars <- char '"' *> manyTill anyChar (char '"')
  pure <<< MedeaString <<< pack $ chars

-- helpers
checkedConstruct :: forall a. (String -> a) -> String -> MedeaParser a
checkedConstruct f t =
  if (_ > 32) <<< String.length $ t then
    fail $ show $ IdentifierTooLong t
  else
    pure <<< f $ t

isSeperatorOrControl :: Char -> Boolean
isSeperatorOrControl c = isSeparator c || isControl c

parseLine :: forall a. Int -> MedeaParser a -> MedeaParser a
parseLine spaces p = (replicateM_ spaces (char ' ')) *> p <* eol

parseKeyVal :: forall a. ReservedIdentifier -> MedeaParser a -> MedeaParser a
parseKeyVal key = ((parseReserved key *> char ' ') *> _)
