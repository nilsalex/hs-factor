module Main where

import Factor
    ( factor
    )

import System.IO
    ( stdout
    , hSetBuffering
    , BufferMode (..)
    )
import System.Random
    ( getStdGen
    , RandomGen
    )
import Text.Read
    ( readMaybe
    )

notAnIntMessage :: String -> String
notAnIntMessage str =
  "\"" <> str <> "\"" <> " is not an integer"

factorisationMessage :: Integer -> Integer -> Integer -> String
factorisationMessage n k l =
  show n <> " = " <> show k <> " * " <> show l

loop :: RandomGen g => g -> IO ()
loop g = do
  hSetBuffering stdout NoBuffering
  putStr "enter an integer: "
  hSetBuffering stdout LineBuffering

  input <- getLine

  case readMaybe input of
    Nothing -> putStrLn (notAnIntMessage input)
    Just n  -> let k = factor g n
                   l = n `div` k
               in putStrLn (factorisationMessage n k l)

  loop g

main :: IO ()
main = do
    g <- getStdGen
    loop g
