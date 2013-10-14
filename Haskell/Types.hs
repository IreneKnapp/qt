module Types
  (TemplatePart(..),
   parseTemplate)
  where

import qualified Data.Text as Text


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
