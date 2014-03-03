{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

module Main where

import           Control.Applicative   (Applicative(..), (<$>), (<*>), (<$))
import           Control.Arrow         (first)
import           Control.Monad
import           Data.Char             (toUpper, toLower)
import           Data.List             (foldl')
import           Data.Semigroup        hiding (option)
import           Data.Time
import           Text.Parsec
import           Text.Parsec.Error
import           Text.Parsec.Pos       (initialPos)
import           Text.Parsec.String

-- For Testing ----------------------------------------------------------------
import        System.IO

today :: Day
today = fromGregorian 2014 2 27
-------------------------------------------------------------------------------

data Date =
  Date { month :: Int
       , day   :: Int } deriving Show

-- We want to collect the errors of multiple passes of parser starting at each
-- position in the input string.
instance Semigroup ParseError where
  e1 <> e2 = foldl' (flip addErrorMessage) e1 (errorMessages e2)

-- Utility functions ----------------------------------------------------------

delim :: Parser ()
delim = (skipMany1 punc <* spaces) <|> eof

punc :: Parser Char
punc = char ',' <|> char '.' <|> space

-- | Return the parse of the first substring that parses successfully and the
--   input 'String' with the match removed.
find :: Parser b -> String -> (Either ParseError b, String)
find p s = find' p s "" (newErrorUnknown $ initialPos "")
  where
    find' p s accum errs =
      case parse ((,) <$> p <*> getInput) "" s of
        Left err ->
          case s of
            [] -> (Left (errs <> err), s)
            (x:xs) -> find' p xs (x : accum) (errs <> err)
        Right (y, str) -> (Right y, (reverse accum) ++ str)

-- Given today's date, a month and a year return the first date in the future
-- with same day and month if possible. Otherwise clip that day to the first
-- valid day in that month.
impliedYear :: Day -> Int -> Int -> Day
impliedYear today m d =
    case dateGuess of
        Just d -> if diffDays d today >= 0
                  then d
                  else addGregorianYearsClip 1 d
        Nothing -> impliedYear today m (d-1)
  where
    (yyyy, mm, dd) = toGregorian today
    dateGuess = fromGregorianValid (yyyy) m d


-- Date words -----------------------------------------------------------------

months :: [String]
months = [ "january", "february", "march", "april", "may"
         , "june", "july", "august", "september", "october"
         , "november", "december"]

nums :: [String]
nums = [ "one", "two", "three", "four", "five", "six", "seven"
       , "eight", "nine", "ten", "eleven", "twelve"]

numth :: [String]
numth = [ "first", "second", "third", "fourth", "fifth"
        , "sixth", "seventh", "eighth", "nineth", "tenth"]

days :: [String]
days = [ "mon", "monday", "tue", "tues", "tuesday", "wed", "wednesday"
       , "thu", "thurs", "thursday", "fri", "friday", "sat", "saturday"
       , "sun", "sunday"]

-- Months ---------------------------------------------------------------------

-- Create a parser for the name of a month. Return the month number.
mkMonthA :: String -> Int -> Parser Int
mkMonthA name r = r <$ oneOf a <* string a23 <* (option "" $ string as)
  where
    ((a1, a23), as) = first (splitAt 1) $ splitAt 3 name
    a = [toUpper (head a1), toLower (head a1)]

-- | Parser for the name of the any month. Return the month number.
monthsA :: Parser Int--Monad m => ParsecT String u m Int
monthsA = choice . map try $ zipWith mkMonthA months [1..12]

-- Days of the Month ----------------------------------------------------------

-- | Parser for numeric day of month. Return the day as an 'Int'.
dayOfMonthN :: Parser Int
dayOfMonthN = do
  s <- many1 digit
  let n = read s :: Int
  suffix elem n [1,21,31] "st"
  suffix elem n [2,22] "nd"
  suffix elem n [3,23] "rd"
  suffix notElem n [1,2,3,21,31,22,23] "th"
  if n > 0 && n <= 31
    then return n
    else parserFail "Day of month out of bounds"
  where
    suffix f m ds suf = if m `f` ds then opt suf else return ()
    opt str = optional (string str)

mkDayOfMonthA :: String -> Int -> Parser Int
mkDayOfMonthA name r = r <$ string name

-- | Parser of a text day of month. We only support words up to the tenth
--   day.
dayOfMonthA :: Parser Int
dayOfMonthA = choice . map try $ zipWith mkDayOfMonthA numth [1..10]

dayOfMonth :: Parser Int
dayOfMonth = dayOfMonthN <|> dayOfMonthA

-- | Parse a date, where the month is a word.
parseDateA :: Parser Date
parseDateA = Date <$> monthsA
                   <* delim
                  <*> dayOfMonth
                   <* delim
                   <* optional (try parseYearA <* delim)

parseYearA :: Parser Int
parseYearA = do
  s <- many1 digit
  let n = read s :: Int
  if n > 2014 && n < 2099
    then return n
    else parserFail "Not a year"

-- | Date in numeric form, e.g. 02/26/14---------------------------------------
parseDateN :: Parser Date
parseDateN = do
  ns <- sepBy1 (many1 digit) (char '/')
  let dm = map read ns :: [Int]
  case dm of
    (x:[])  -> parserZero
    (m:d:_) -> if (m > 0) && (m <= 12) && (d > 0) && (d <= 31)
               then return (Date m d)
               else parserFail "Month or day out of range"

parseDate :: Parser Date
parseDate = parseDateA <|> parseDateN

-------------------------------------------------------------------------------

main = do
  withFile "testdata.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    let testStrings = lines contents
        results = map (find parseDate) testStrings
        rs = zip [1..] results
    mapM (putStrLn . show) rs