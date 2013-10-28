module Import.Types
  (Main,
   TemplatePart(..),
   parseTemplate)
  where

import Prelude as Import hiding
  (head, init, last, readFile, tail, writeFile, catch)

import qualified Data.Text as Text
import qualified System.IO as IO

import Control.Monad.Logger


type Main a = LoggingT IO a


{-
data CommandTemplate = CommandTemplate {
    commandTemplateType :: CommandType,
    commandTemplateRecipient :: [TemplatePart],
    commandTemplateParts :: [TemplatePart]
  }
-}


data TemplatePart
  = LiteralTemplatePart Text.Text
  | VariableTemplatePart Text.Text
  deriving (Show)


parseTemplate :: Text.Text -> Maybe [TemplatePart]
parseTemplate text = do
  return []
