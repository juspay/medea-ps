module Data.Medea.Parser.Primitive where

import MedeaPrelude
import Control.Alternative ((<|>))
import Control.Safely (replicateM_)
import Data.Array as Array
import Data.Char.Unicode (isSeparator, isControl)
import Data.Int as Int
import Data.List (List)
import Data.Natural (Natural)
import Data.Natural as Nat
import Data.String (codePointFromChar, length, singleton, uncons) as String
import Data.String.CodeUnits (fromCharArray) as String
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (char, string, anyChar)
import Text.Parsing.Parser.Token (digit)

import Data.Medea.Parser.Parsing (takeWhile1P, eol)
import Data.Medea.JSONType (JSONType(..))
import Data.Medea.Parser.Types (MedeaParser, MedeaParseErr(..))


newtype Identifier = Identifier String

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


startIdentifier :: Identifier
startIdentifier = Identifier "$start"

newtype ReservedIdentifier = ReservedIdentifier String

derive instance eqReservedIdentifier :: Eq ReservedIdentifier

tryReserved :: Identifier -> Maybe ReservedIdentifier
tryReserved (Identifier ident) = 
  case String.uncons ident of
    Just ht -> 
      if ht.head == String.codePointFromChar '$' 
        then Just $ ReservedIdentifier ident
        else Nothing
    Nothing -> Nothing

forgetReserved :: ReservedIdentifier -> Identifier
forgetReserved (ReservedIdentifier a) = Identifier a

newtype PrimTypeIdentifier = PrimTypeIdentifier JSONType

derive instance eqPrimTypeIdentifier :: Eq PrimTypeIdentifier

typeOf :: PrimTypeIdentifier -> JSONType
typeOf (PrimTypeIdentifier jt) = jt

parsePrimType :: MedeaParser PrimTypeIdentifier
parsePrimType 
  = PrimTypeIdentifier <$>
    (   string "$null" $> JSONNull
    <|> string "$boolean" $> JSONBoolean
    <|> string "$object" $> JSONObject
    <|> string "$array" $> JSONArray
    <|> string "$number" $> JSONNumber
    <|> string "$string" $> JSONString
    )


tryPrimType :: Identifier -> Maybe PrimTypeIdentifier
tryPrimType (Identifier ident) = PrimTypeIdentifier <$>
  case ident of
    "$null" -> Just JSONNull
    "$boolean" -> Just JSONBoolean
    "$object" -> Just JSONObject
    "$array" -> Just JSONArray
    "$number" -> Just JSONNumber
    "$string" -> Just JSONString
    _ -> Nothing

forgetPrimType :: PrimTypeIdentifier -> Identifier
forgetPrimType ident = Identifier $ case typeOf ident of
  JSONNull -> "$null"
  JSONBoolean -> "$boolean"
  JSONObject -> "$object"
  JSONArray -> "$array"
  JSONNumber -> "$number"
  JSONString -> "$string"

isReserved :: Identifier -> Boolean
isReserved = isJust <<< tryReserved

isStartIdent :: Identifier -> Boolean
isStartIdent = (_ == startIdentifier)

parseNatural :: MedeaParser Natural
parseNatural = do
  digits <- many digit
  let digitString = String.fromCharArray digits
  case Array.uncons digits of
    Just ht -> 
      case ht.head of 
        '0' -> fail $ show $ LeadingZero $ digitString
        _ -> do 
          let mInt = Int.fromString $ digitString
          case mInt of
            Just n -> pure $ Nat.intToNat n
            Nothing -> fail "failed to parse integer"
    Nothing -> fail "expected digits"

newtype MedeaString = MedeaString String
  
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
  if (_ > 32) <<< String.length $ t
    then fail $ show  $ IdentifierTooLong t
    else pure <<< f $ t

parseReservedChunk :: String -> MedeaParser ReservedIdentifier
parseReservedChunk identName = do
  ident <- string $ "$" <> identName
  checkedConstruct ReservedIdentifier ident

isSeperatorOrControl :: Char -> Boolean
isSeperatorOrControl c = isSeparator c || isControl c

parseLine :: forall a. Int -> MedeaParser a -> MedeaParser a
parseLine spaces p = (replicateM_ spaces (char ' ')) *> p <* eol

parseKeyVal :: forall a. String -> MedeaParser a -> MedeaParser a
parseKeyVal key = ((parseReservedChunk key *> char ' ' ) *> _)

