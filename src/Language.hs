module Language where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text

-- Here we need to declare the general structure for a tmLanguage
-- as well as one that we can use to generate equivalent highlightjs code.
--

newtype Include = Include Text
  deriving (Show)

data Rule = Rule
  deriving (Show)

data TmLanguage = TmLanguage
  { name :: Text,
    scopeName :: Text,
    fileTypes :: [Text],
    patterns :: [Include],
    repository :: Map Text Rule
  }
  deriving (Show)

defaultTmLanguage :: Text -> Text -> Text -> TmLanguage
defaultTmLanguage n s ext =
  TmLanguage
    { name = n,
      scopeName = s,
      fileTypes = [ext],
      patterns = [],
      repository = Map.empty
    }

type TmLanguageState = State TmLanguage ()

addFileType :: Text -> TmLanguageState
addFileType ext = modify $ \lang ->
  lang {fileTypes = ext : fileTypes lang}

-- addPattern :: Parser a -> TmLanguageState
-- addPattern a = modify $ \lang ->
--   lang {patterns = toPattern a : patterns lang}

makeTmLanguage :: Text -> Text -> Text -> TmLanguageState -> TmLanguage
makeTmLanguage n s ext b = execState b $ defaultTmLanguage n s ext
