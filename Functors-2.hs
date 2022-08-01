{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.Char (toLower, isAlpha, isDigit)

username1 = "One Lonely Burrito"
username2 = "One Lone Burrito"

formatString :: String -> String
formatString str = filter (`notElem` [' ', '\n', '\r', '\t']) $ map toLower str

isValidLength :: Int -> Int -> String -> Bool
isValidLength lo hi str = length str > lo && length str < hi

allAlphaNum :: String -> Bool
allAlphaNum str = all (\c -> isAlpha c || isDigit c) str

-- validateUsername :: String -> Maybe String
-- validateUsername username
--   | isValidLength 9 16 username &&
--     allAlphaNum username           = Just username
--   | otherwise                      = Nothing

isValidUsername :: String -> Bool
isValidUsername username = isValidLength 9 16 username &&
                           allAlphaNum username

formatMaybeString :: Maybe String -> Maybe String
formatMaybeString Nothing    = Nothing
formatMaybeString (Just str) = Just $ formatString str

-- validateMaybeUsername :: Maybe String -> Maybe Bool
-- validateMaybeUsername Nothing         = Nothing
-- validateMaybeUsername (Just username) = validateUsername username

makeGreeting :: String -> String
makeGreeting username
  | isValidUsername username = "Welcome aboard, " ++ username ++ "!"
  | otherwise = "Invalid username!"

makeMaybeGreeting :: Maybe String -> Maybe String
makeMaybeGreeting Nothing         = Nothing
makeMaybeGreeting (Just username) = Just $ makeGreeting username

maybeUsername1 = Just "One Lonely Burrito"
maybeUsername2 = Nothing

formatMaybeString' :: Maybe String -> Maybe String
formatMaybeString' = fmap formatString

isValidMaybeUsername' :: Maybe String -> Maybe Bool
isValidMaybeUsername' = fmap isValidUsername

makeMaybeGreeting' :: Maybe String -> Maybe String
makeMaybeGreeting' = fmap makeGreeting

processUsername :: String -> String
processUsername username = makeGreeting $ formatString username

processMaybeUsername :: Maybe String -> Maybe String
processMaybeUsername username = fmap (makeGreeting . formatString) username

