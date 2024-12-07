{-# LANGUAGE TupleSections #-}

module Day3_RE(day3_RE) where

import Utils hiding (string, many)
import Utils qualified as U
import Data.Char
import Data.Monoid
import Control.Applicative
import Data.Functor
import Text.Regex.Applicative
import Data.Scientific (Scientific)


test = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    
pint :: RE Char Int
pint = read <$> some (psym isNumber)

pmul :: RE Char Int
pmul = combine <$> string "mul(" <*> pint <*> sym ',' <*> pint <*> sym ')'
  where
    combine :: String -> Int -> Char -> Int -> Char -> Int
    combine _ x _ y _ = x*y

p1 :: RE  Char [Int]
p1 = some (many (psym (/= 'm')) *> pmul) <* many (psym (/='\n'))


p2 :: RE Char String
p2 = some (many (psym (/='a')) *> sym 'a') <* many (psym (/='a'))

p3 :: RE  Char [Int]
p3 = many (psym (/='m')) *> some (pmul <* many (psym (/= 'm'))) -- <* many (psym (/='\n'))

p4 :: RE Char String
p4 = many (psym (/='a')) *> some (sym 'a' <* many (psym (/='a')))

day3_RE :: IO ()
day3_RE = do
  ss <- getF id 3

  putStrLn $ "1: " ++ show (test =~ p1)
  putStrLn $ "2: " ++ show ("xxabbbabbb" =~ p2)
  putStrLn $ "3: " ++ show (test =~ p3)
  putStrLn $ "4: " ++ show ("xxabxyzbabccacbb" =~ p4)

  return ()

trim :: String -> String
trim input = reverse flippedTrimmed
  where
    trimStart = dropWhile isSpace input
    flipped = reverse trimStart
    flippedTrimmed = dropWhile isSpace flipped


parseLowercaseA :: RE Char Char
parseLowercaseA = sym 'a'

readFeatureWord :: RE Char String
readFeatureWord = string "Feature"

readUntilEndOfLine :: RE Char String
readUntilEndOfLine = many (psym (/= '\n'))

data TwoChars = TwoChars Char Char deriving (Show)

parseNonNewline :: RE Char Char
parseNonNewline = psym (/= '\n')

parseTwoChars :: RE Char TwoChars
parseTwoChars = TwoChars <$> parseNonNewline <*> parseNonNewline

parseFirst :: RE Char Char
parseFirst = parseNonNewline <* parseNonNewline

parseSecond :: RE Char Char
parseSecond = parseNonNewline *> parseNonNewline

readThroughEndOfLine :: RE Char String
readThroughEndOfLine = readUntilEndOfLine <* sym '\n'

readThroughBar :: RE Char String
readThroughBar = readUntilBar <* sym '|'

readUntilBar :: RE Char String
readUntilBar = many (psym (\c -> c /= '|' && c /= '\n'))

parseFeatureTitle :: RE Char String
parseFeatureTitle = string "Feature: " *> readThroughEndOfLine

parseScenarioTitle :: RE Char String
parseScenarioTitle = string "Scenario: " *> readThroughEndOfLine

parseEither :: RE Char String
parseEither = parseFeatureTitle <|> parseScenarioTitle

data Value = ValueNull | ValueBool Bool | ValueString String | ValueNumber Scientific deriving (Show, Eq)


valueParser :: RE Char Value
valueParser = nullParser <|> boolParser <|> numberParser <|> stringParser


-- Note: $> x = *> pure x
nullParser :: RE Char Value
nullParser = (string "null" <|> string "NULL" <|> string "Null") $> ValueNull

boolParser :: RE Char Value
boolParser = trueParser $> ValueBool True <|> falseParser $> ValueBool False
  where
    trueParser = string "True" <|> string "true" <|> string "TRUE"
    falseParser = string "False" <|> string "false" <|> string "FALSE"

numberParser :: RE Char Value
numberParser = ValueNumber . read <$> (negativeParser <|> decimalParser <|> integerParser)
  where
    integerParser :: RE Char String
    integerParser = some (psym isNumber)
    decimalParser :: RE Char String
    decimalParser = combineDecimal <$> many (psym isNumber)  <*> sym '.' <*> some (psym isNumber)
    negativeParser :: RE Char String
    negativeParser = (:) <$> sym '-' <*> (decimalParser <|> integerParser)
    combineDecimal :: String -> Char -> String -> String
    combineDecimal base point decimal = base ++ (point : decimal)

stringParser :: RE Char Value
stringParser = ValueString . trim <$> readUntilBar


isNonNewlineSpace :: RE Char Char
isNonNewlineSpace = psym (\c -> isSpace c && c /= '\n')


exampleLineParser :: RE Char [Value]
exampleLineParser = sym '|' *> many cellParser <* readThroughEndOfLine
  where
    cellParser = many isNonNewlineSpace *> valueParser <* readThroughBar


exampleColumnTitleLineParser :: RE Char [String]
exampleColumnTitleLineParser = sym '|' *> many cellParser <* readThroughEndOfLine
  where
    cellParser = 
      many isNonNewlineSpace *> many (psym isAlpha) <* readThroughBar


data ExampleTable = ExampleTable
  { exampleTableKeys :: [String]
  , exampleTableExamples :: [[(String, Value)]]
  } deriving (Show, Eq)



-- I tried without a parser with regex etc - but failed
parseMul :: ReadP Int
parseMul = do
  _ <- U.string "mul("
  x <- pInt
  _ <- char ','
  y <- pInt
  _ <- char ')'
  return $ x*y


-- The lazy way - just crawl along the sting a character at a time...
parse1 :: Int -> String -> Int
parse1 acc [] = acc
parse1 acc s
  | "mul(" == take 4 s = if null p then parse1 acc (drop 4 s) else parse1 (acc + n) leftover
  | otherwise = parse1 acc $ tail s
  where
    p :: [(Int, String)]
    p = parseS parseMul s -- null if parse fails
    n = fst $ head p
    leftover = snd $ head p


-- Similarly, crawl along, but jump on a few chars on do, dont, mul
parse2 :: Bool -> Int -> String -> Int
parse2 _ acc [] = acc
parse2 False acc s
  | "do()" == take 4 s = parse2 True acc $ drop 4 s
  | otherwise = parse2 False acc $ tail s
parse2 True acc s
  | "don't()" == take 7 s = parse2 False acc $ drop 7 s
  | "mul(" == take 4 s = if null p then parse2 True acc (drop 4 s) else parse2 True (acc + n) leftover
  | otherwise = parse2 True acc $ tail s
  where
    p :: [(Int, String)]
    p = parseS parseMul s -- null if parse fails
    n = fst $ head p
    leftover = snd $ head p
