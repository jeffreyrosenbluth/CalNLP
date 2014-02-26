{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

module Main where

import      Control.Monad
import      Data.Char                 (toUpper, toLower)
import      Data.List                 (intercalate)
import      Text.Parsec               hiding (notFollowedBy, eof)
import      Control.Applicative       (Applicative(..), (<$>), (<*>), liftA2)
import      Control.Arrow             (first)

type Result  =  (Int, String)
type Result2 =  (Int, Int, String)

data Date =
  Date { month :: Int
       , day   :: Int } deriving Show

-- Utility functions ----------------------------------------------------------

-- | Verstion of @notFollowedBy@ that returns \"\" instead of ().
notFollowedBy :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m String
notFollowedBy p     = try (do{ c <- try p; unexpected (show c) }
                           <|> return ""
                          )

-- | Verstion of @eof@ that returns \"\" instead of ().
eof :: (Stream s m t, Show t) => ParsecT s u m String
eof                 = notFollowedBy anyToken <?> "end of input"

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

delim :: Stream s m Char => ParsecT s u m String
delim = (punc <:> many space) <|> eof

punc :: Stream s m Char => ParsecT s u m Char
punc = char ',' <|> char '.' <|> space

-- | Return the parse of the first substring that parses successfully and the
--   input 'String' with the match removed.
find :: Parsec String () (b, [a]) -> String -> (Either ParseError b, String)
find p s = find' p s ""
  where
    find' p s accum =
      case parse p "" s of
        Left err ->
          case s of
            [] -> (Left err, s)
            (x:xs) -> find' p xs (accum ++ [x])
        Right (y, str) -> (Right y, accum ++ (drop (length str) s))

-- Months ---------------------------------------------------------------------

-- Create a parser for the name of a month. Return the month number paired
-- with the string that matched.
mkMonthA :: Monad m => String -> Int -> ParsecT String u m Result
mkMonthA name r = (\x -> (r,x)) <$> oneOf a <:> string a23 <++> ending
  where
    ((a1, a23), as) = first (splitAt 1) $ splitAt 3 name
    a = [toUpper (head a1), toLower (head a1)]
    ending = option "" $ string as

-- | Parser for the name of the any month. Return the month number paired
--   with the string that matched.
monthsA :: Monad m => ParsecT String u m Result
monthsA = choice . map try $ zipWith mkMonthA months [1..12]
  where
    months = [ "January", "February", "March", "April", "May"
             , "June", "July", "August", "September", "October"
             , "November", "December"]

-- Days of the Month ----------------------------------------------------------

-- | Parser for numeric day of month. Return the day as an 'Int' paired
--   with the 'String' that matched.
dayOfMonthN :: Monad m => ParsecT String u m Result
dayOfMonthN = do
  s <- many1 digit
  let n = read s :: Int
  -- Allow for things like 31st, 23rd and 2nd.
  e1 <- suffix n [1,21,31] "st"
  e2 <- suffix n [2,22] "nd"
  e3 <- suffix n [3,23] "rd"
  e4 <- if (not (n `elem` [1,2,3,21,31,22,23]))
        then opt "th"
        else return ""
  -- Valid days of the month must be between 1 and 31, if the day exceeds
  -- the number of days in a particular month it will be handled separately.
  if n > 0 && n <= 31
    then return (n, concat [s, e1, e2, e3, e4])
    else parserFail "Day of month out of bounds"
  where
    suffix m ds suf = if m `elem` ds
                      then opt suf
                      else return ""
    opt str = option "" (string str)

mkDayOfMonthA :: Monad m => String -> Int -> ParsecT String u m Result
mkDayOfMonthA name r = (\x -> (r, x)) <$> string name

-- | Parser of a text day of month. We only support words up to the tenth
--   day.
dayOfMonthA :: Monad m => ParsecT String u m Result
dayOfMonthA = choice . map try $ zipWith mkDayOfMonthA ds [1..10]
  where
    ds = [ "first", "second", "third", "fourth", "fifth"
         , "sixth", "seventh", "eighth", "nineth", "tenth"]

dayOfMonth :: Monad m => ParsecT String u m Result
dayOfMonth = dayOfMonthN <|> dayOfMonthA

-- | Parse a date, i.e a month and day of the month. We do not need the year.
parseDateA :: Monad m => ParsecT String u m (Date, String)
parseDateA = toDate <$> monthsA
                    <*> delim
                    <*> dayOfMonth
                    <*> delim
  where
    toDate (m, s1) s (d, s2) s' = (Date m d, s1 ++ s ++ s2 ++ s')

-- | Date in numeric form, e.g. 02/26/14-----------------------------------------
parseDateN :: Monad m => ParsecT String u m (Date, String)
parseDateN = do
  ns <- sepBy1 (many1 digit) (char '/')
  let s  = intercalate "/" ns
      dm = map read ns :: [Int]
  case dm of
    (x:[])  -> parserFail "Need both month and day"
    (m:d:_) -> if (m > 0) && (m <= 12) && (d > 0) && (d <= 31)
               then return (Date m d, s)
               else parserFail "Month of day out of range"

parseDate :: Monad m => ParsecT String u m (Date, String)
parseDate = parseDateA <|> parseDateN

--parseDN :: Monad m => ParsecT String u m (Date, String)
--parseDN = do
--  ns <- sepBy1 (many1 digit) (char '/')
--  s  <- getInput
--  let dm = map read ns :: [Int]
--  case dm of
--    (x:[])  -> parserFail "Need both month and day"
--    (m:d:_) -> if (m > 0) && (m <= 12) && (d > 0) && (d <= 31)
--               then return (Date m d, s)
--               else parserFail "Month of day out of range"
-------------------------------------------------------------------------------

main = forever $ do putStrLn "Enter a string: "
                    input <- getLine
                    putStrLn . show $ find parseDate input
