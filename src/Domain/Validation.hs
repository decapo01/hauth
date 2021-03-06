module Domain.Validation where

import           ClassyPrelude
import           Text.Regex.PCRE.Heavy


type Validation e a = a -> Maybe e


validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val =
  case concatMap (\f -> maybeToList $ f val) validations of
    []   -> Right $ constructor val
    errs -> Left errs


blah2 :: Text -> Text
blah2 b = b ++ "text"



rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween minRange maxRnage msg val =
  if val >= minRange && val <= maxRnage then Nothing else Just msg

lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween minLen maxLen msg val =
  rangeBetween minLen maxLen msg (length val)


regexMatches :: Regex -> e -> Validation e Text
regexMatches regex msg val =
  if val =~ regex then Nothing else Just msg

blah :: Text -> Text
blah a = a ++ "blah"
