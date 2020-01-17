module Isogram
  ( isIsogram
  ) where

import           Data.Char (toLower)

isIsogramR :: String -> Bool
isIsogramR "" = True
isIsogramR (x:xs) =
  (x `elem` "- " || not (toLower (x) `elem` xs)) && isIsogram (xs)

isIsogram :: String -> Bool
isIsogram "" = True
isIsogram xs = isIsogramR (toLower <$> xs)
