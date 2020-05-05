module TestM.Util where

import MedeaPrelude
import Data.String (length) as String
import Data.String.CodeUnits (charAt) as String
import Effect.Aff (Aff)
import Node.FS.Aff (readdir)
import Node.Path (FilePath, extname)

appendPath :: FilePath -> FilePath -> FilePath
appendPath a b = case a of
  "" -> b
  str -> case String.charAt 0 b of
    Nothing ->  a
    Just h -> if h == '/' then a <> b else do
      let i = String.length a - 1 
      case String.charAt i a of
        Just '/' -> a <> b
        _ ->  a <> "/" <> b
        
infixr 4 appendPath as </>

listMedeaFiles :: FilePath -> Aff (Array FilePath)
listMedeaFiles dirpath = do
  contents <- readdir dirpath
  pure $ map (dirpath </> _) $ sort $ filter ((_ == ".medea") <<< extname) $ contents
  
