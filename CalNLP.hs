{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

module Main where

import      Control.Monad
import      Data.Char                 (toUpper, toLower)
import      Text.Parsec
import      Control.Applicative       hiding ((<|>), optional)

data Date =
  Date { month :: Int
       , day   :: Int } deriving Show

-- Utility functions ----------------------------------------------------------

const2 :: a -> (b -> c -> a)
const2 r _ _ = r

punc :: Stream s m Char => ParsecT s u m Char
punc = char ',' <|> char '.'

delims :: Stream s m Char => [ParsecT s u m ()]
delims = [skipMany1 punc, skipMany1 space, eof]

-- Months ---------------------------------------------------------------------

-- Create a parser for the name of a month.
mkMonthA :: Monad m => String -> Int -> ParsecT String u m Int
mkMonthA name r = const2 r <$> oneOf a1 <*> string a23 <* ending
  where
    a1 = toUpper a : toLower a : []
    a23 = take 2 . tail $ name
    ending = case drop 3 name of
      "" -> choice delims
      e  -> choice $ skipMany1 (string e) : delims
    a = head name

-- | Parser for the name of the any month.
monthsA :: Monad m => ParsecT String u m Int
monthsA = choice . map try $ zipWith mkMonthA months [1..12]
  where
    months = [ "January", "February", "March", "April", "May"
             , "June", "July", "August", "September", "October"
             , "November", "December"]

-- Days of the Month ----------------------------------------------------------

-- | Parser for numeric day of month.
dayOfMonthN :: Monad m => ParsecT String u m Int
dayOfMonthN = do
  s <- many1 digit
  let n = read s :: Int
  when (n == 1 || n == 21 || n == 31) (opt "st")
  when (n == 2 || n ==22) (opt "nd")
  when (n == 3 || n ==23) (opt "rd")
  when (not (n `elem` [1,2,3,21,31,22,23])) (opt "th")
  choice delims
  if n > 0 && n <= 31
    then return n
    else parserFail "Day of Month out of bounds"
  where
    opt str = optional $ string str

mkDayOfMonthA :: Monad m => String -> Int -> ParsecT String u m Int
mkDayOfMonthA name r = const2 r <$> string name <*> choice delims

-- | Parser of a text day of month. We only support words up to the tenth
--   day.
dayOfMonthA :: Monad m => ParsecT String u m Int
dayOfMonthA = choice . map try $ zipWith mkDayOfMonthA ds [1..10]
  where
    ds = [ "first", "second", "third", "fourth", "fifth"
         , "sixth", "seventh", "eighth", "nineth", "tenth"]

-------------------------------------------------------------------------------

-- | Parse a date, i.e a month and day of the month. We don't need the year.
parseDate :: Monad m => ParsecT String u m Date
parseDate = Date <$> monthsA
                    <* spaces
                    <*> (dayOfMonthN <|> dayOfMonthA)


main = forever $ do putStrLn "Enter a string: "
                    input <- getLine
                    parseTest parseDate input