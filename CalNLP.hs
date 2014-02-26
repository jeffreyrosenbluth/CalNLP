{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

module Main where

import        Control.Applicative   (Applicative(..), (<$>), (<*>))
import        Control.Arrow         (first)
import        Control.Monad
import        Data.Char             (toUpper, toLower)
import        Text.Parsec

-- For Testing --------
import        System.IO
-----------------------

data Date =
  Date { month :: Int
       , day   :: Int } deriving Show

-- Utility functions ----------------------------------------------------------

delim :: Stream s m Char => ParsecT s u m ()
delim = (skipMany1 punc <* spaces) <|> eof

punc :: Stream s m Char => ParsecT s u m Char
punc = char ',' <|> char '.' <|> space

-- | Return the parse of the first substring that parses successfully and the
--   input 'String' with the match removed.
find :: Parsec String () b -> String -> (Either ParseError b, String)
find p s = find' p s ""
  where
    find' p s accum =
      case parse p' "" s of
        Left err ->
          case s of
            [] -> (Left err, s)
            (x:xs) -> find' p xs (accum ++ [x])
        Right (y, str) -> (Right y, accum ++ str)
      where
        p' = (,) <$> p <*> getInput

-- Months ---------------------------------------------------------------------

-- Create a parser for the name of a month. Return the month number paired
-- with the string that matched.
mkMonthA :: Monad m => String -> Int -> ParsecT String u m Int
mkMonthA name r = const r <$> oneOf a <* string a23 <* ending
  where
    ((a1, a23), as) = first (splitAt 1) $ splitAt 3 name
    a = [toUpper (head a1), toLower (head a1)]
    ending = option "" $ string as

-- | Parser for the name of the any month. Return the month number paired
--   with the string that matched.
monthsA :: Monad m => ParsecT String u m Int
monthsA = choice . map try $ zipWith mkMonthA months [1..12]
  where
    months = [ "January", "February", "March", "April", "May"
             , "June", "July", "August", "September", "October"
             , "November", "December"]

-- Days of the Month ----------------------------------------------------------

-- | Parser for numeric day of month. Return the day as an 'Int' paired
--   with the 'String' that matched.
dayOfMonthN :: Monad m => ParsecT String u m Int
dayOfMonthN = do
  s <- many1 digit
  let n = read s :: Int
  -- Allow for things like 31st, 23rd, 15th, and 2nd.
  suffix elem n [1,21,31] "st"
  suffix elem n [2,22] "nd"
  suffix elem n [3,23] "rd"
  suffix notElem n [1,2,3,21,31,22,23] "th"
  -- Valid days of the month must be between 1 and 31, if the day exceeds
  -- the number of days in a particular month it will be handled separately.
  if n > 0 && n <= 31
    then return n
    else parserFail "Day of month out of bounds"
  where
    suffix f m ds suf = if m `f` ds then opt suf else return ()
    opt str = optional (string str)

mkDayOfMonthA :: Monad m => String -> Int -> ParsecT String u m Int
mkDayOfMonthA name r = const r <$> string name

-- | Parser of a text day of month. We only support words up to the tenth
--   day.
dayOfMonthA :: Monad m => ParsecT String u m Int
dayOfMonthA = choice . map try $ zipWith mkDayOfMonthA ds [1..10]
  where
    ds = [ "first", "second", "third", "fourth", "fifth"
         , "sixth", "seventh", "eighth", "nineth", "tenth"]

dayOfMonth :: Monad m => ParsecT String u m Int
dayOfMonth = dayOfMonthN <|> dayOfMonthA

-- | Parse a date, i.e a month and day of the month. We do not need the year.
parseDateA :: Monad m => ParsecT String u m Date
parseDateA = Date <$> monthsA
                   <* delim
                  <*> dayOfMonth
                   <* delim

-- | Date in numeric form, e.g. 02/26/14---------------------------------------
parseDateN :: Monad m => ParsecT String u m Date
parseDateN = do
  ns <- sepBy1 (many1 digit) (char '/')
  let dm = map read ns :: [Int]
  case dm of
    (x:[])  -> parserFail "Need both month and day"
    (m:d:_) -> if (m > 0) && (m <= 12) && (d > 0) && (d <= 31)
               then return (Date m d)
               else parserFail "Month of day out of range"

parseDate :: Monad m => ParsecT String u m Date
parseDate = parseDateA <|> parseDateN

-------------------------------------------------------------------------------

main = do
    withFile "testdata.txt" ReadMode
      ( \handle -> do
          contents <- hGetContents handle
          let testStrings = lines contents
              results = map (find parseDate) testStrings
              rs = zip [1..141] results
          mapM (putStrLn . show) rs
      )
