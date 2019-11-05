{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (when)
import Data.List (nub)
import Text.Read (readMaybe)
import System.Random (StdGen, getStdGen, randomR)


_NO_RANDOM_NAMES :: Int
_NO_RANDOM_NAMES = 1000


_RANDOM_NAME_LENGTH :: Int
_RANDOM_NAME_LENGTH = 5


data Name = Name
  { firstName :: String
  , lastName :: String
  }


main :: IO ()
main = do
  putStrLn "Hi, I'm Bob."
  name <- readName
  writeName name
  queryAndWriteAgeGender name
  checkPalindromes name
  putStrLn "I'll print a list of some 'fictional names' that could be made from the letters of your name."
  writeRandomNames name
  putStrLn $ concat
    [ "Ready: "
    , show _NO_RANDOM_NAMES
    , " name combinations made from the user name "
    , firstName name
    , " "
    , lastName name
    , " can be found in a file named "
    , nameFile name
    ]
  putStrLn "Thank you for using Haskell for hacking your name into pieces and wreaking havoc with them!"


nameFile :: Name -> FilePath
nameFile n = firstName n ++ "_" ++ lastName n


nameToString :: Name -> String
nameToString name = firstName name ++ " " ++ lastName name


prompt :: (String -> Maybe a) -> String -> IO a
prompt parse query = do
  putStrLn query
  line <- getLine
  case parse line of
    Nothing -> do
      putStrLn "Response not understood."
      prompt parse query
    Just x -> pure x


isPalindrome :: String -> Bool
isPalindrome s = reverse s == s


pickRandom :: StdGen -> [a] -> Int -> (a, StdGen)
pickRandom rng xs len
  = let (n, rng') = randomR (0, len - 1) rng in
    (xs !! n, rng')


pickRandomN :: forall a. Int -> StdGen -> [a] -> Int -> ([a], StdGen)
pickRandomN n rng xs len = go n rng
  where
    go :: Int -> StdGen -> ([a], StdGen)
    go 0 rng = ([], rng)
    go n rng
      = let (x, rng') = pickRandom rng xs len
            (xs', rng'') = go (n - 1) rng' in
        (x : xs', rng'')


readName :: IO Name
readName = do
  firstName <- prompt Just "What's your first name?"
  lastName <- prompt Just "What's your last name?"
  pure Name { firstName = firstName, lastName = lastName }


writeName :: Name -> IO ()
writeName name = writeFile (nameFile name) $ nameToString name ++ "\n"


queryAndWriteAgeGender :: Name -> IO ()
queryAndWriteAgeGender n = do
  age <- prompt (readMaybe :: String -> Maybe Int) "What's your age?"
  appendFile (nameFile n) $ show age ++ "\n"
  gender <- prompt Just "What's your gender?"
  appendFile (nameFile n) $ gender ++ "\n"


checkPalindromes :: Name -> IO ()
checkPalindromes n = do
  when (isPalindrome $ firstName n) $ putStrLn
    "You've got a palindromic first name, cool!"
  when (isPalindrome $ lastName n) $ putStrLn
    "You've got a palindromic last name, cool!"


writeRandomNames :: Name -> IO ()
writeRandomNames name = do
  rng <- getStdGen
  loop rng _NO_RANDOM_NAMES
  where
    loop :: StdGen -> Int -> IO ()
    loop _   0 = pure ()
    loop rng n = do
      let (rname, rng') = randomName rng
      appendFile (nameFile name) $ nameToString rname ++ "\n"
      loop rng' (n - 1)

    nameChars :: [Char]
    nameChars = nub $ firstName name ++ lastName name

    noNameChars :: Int
    noNameChars = length nameChars

    randomNamePart :: StdGen -> (String, StdGen)
    randomNamePart rng
      = pickRandomN _RANDOM_NAME_LENGTH rng nameChars noNameChars

    randomName :: StdGen -> (Name, StdGen)
    randomName rng =
      let (firstName, rng') = randomNamePart rng
          (lastName, rng'') = randomNamePart rng' in
      (Name { firstName = firstName, lastName = lastName }, rng'')
