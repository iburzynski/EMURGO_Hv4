module CCVal (main) where

-- reference: https://datagenetics.com/blog/july42013/index.html

import Data.List (foldl')
import Text.Read (readMaybe)

data Issuer =
    Amex
  | DinersClub
  | Discover
  | JCB
  | MasterCard
  | Visa
  deriving Show

data CC = CC {
    runIssuer :: Issuer
  , runNumber :: Int
  }

main :: IO ()
main = do
  putStrLn "Enter a credit card number to validate:"
  n <- getLine
  let mn = readMaybe n :: Maybe Int
  case mn >>= validateCC of
    Nothing -> putStrLn "Invalid card number: please try again!" *> main
    Just cc -> putStrLn $ mconcat [show (runNumber cc), " is a valid ", show (runIssuer cc), " card"]

validateCC :: Int -> Maybe CC
validateCC n = validateIssuer rds >>= makeCC rds
  where
    -- prevents runtime error from negative inputs
    n'          = abs n
    -- gets reversed list of cc digits
    rds         = revDigs n'
    -- validates cc digits with Luhn's algorithm
    makeCC ds i = if luhn ds then Just (CC i n') else Nothing

-- Converts credit card number to list of digits in reverse order
revDigs :: Int -> [Int]
revDigs 0 = []
revDigs n = n `mod` 10 : revDigs (n `div` 10)

-- Identifies card issuer and checks for correct number of digits
validateIssuer :: [Int] -> Maybe Issuer
validateIssuer xs   = mapM getIssuer (countAndRev xs) >>= uncurry validateLength
  where countAndRev = foldl' (\(c, ys) x -> (c + 1, x : ys)) (0, [])
     -- Counts number of digits and restores them to original order for issuer validation

-- Checks initial card digits against issuer-specific prefixes, returning the issuer if valid
getIssuer :: [Int] -> Maybe Issuer
getIssuer (3:x:_)     = case x of
  4 -> Just Amex
  7 -> Just Amex
  5 -> Just JCB
  6 -> Just DinersClub
  8 -> Just DinersClub
  _ -> Nothing
getIssuer (4:_)       = Just Visa
getIssuer (5:x:_)     = if x `elem` [1..5] then Just MasterCard else Nothing
getIssuer (6:x:y:z:_) = if x == 5 || [x, y, z] == [0, 1, 1] then Just Discover else Nothing
getIssuer _           = Nothing

-- Checks if the card has a valid number of digits
validateLength :: Int -> Issuer -> Maybe Issuer
validateLength 15 Amex = Just Amex
validateLength 16 i    = Just i
validateLength _  _    = Nothing

-- Luhn's Algorithm: takes a list of cc digits (in reverse order) and validates based on checkdigit
luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sumVals . multOddPos
  where sumVals = foldr (\x y -> if x > 9 then x - 9 + y else x + y) 0
     -- Subtracts 9 from values > 9 and sums

-- Separates first digit (checkdigit), multiplies remaining odd-positioned digits by 2, then rejoins
multOddPos :: [Int] -> [Int]
multOddPos []     = []
multOddPos (x:xs) = x : go xs
  where
    go []         = []
    go [y]        = [y * 2]
    go (y:y':ys)  = y * 2 : y' : go ys