module School
  ( School
  , add
  , empty
  , grade
  , sorted
  ) where

import           Data.List (sort, sortBy)

data School =
  School
    { students :: [(Int, String)]
    }

-- I did not write this. I found this on StackOverflow
unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter ((/=) x) xs)

add :: Int -> String -> School -> School
add gradeNum student school = School $ (gradeNum, student) : (students school)

empty :: School
empty = School []

grade :: Int -> School -> [String]
grade gradeNum school =
  sort $ snd <$> filter ((== gradeNum) . fst) (students school)

sorted :: School -> [(Int, [String])]
sorted school =
  sortBy (\(a, _) (b, _) -> compare a b) $
  fmap
    (\grd -> (grd, sort $ fmap snd $ filter ((== grd) . fst) (students school)))
    (unique $ fst <$> students school)
