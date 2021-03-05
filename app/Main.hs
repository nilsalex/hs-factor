module Main where

import Factor
    ( factor
    )

import System.IO
    ( stdout
    , hSetBuffering
    , BufferMode (..)
    )
import Text.Read
    ( readMaybe
    )

notAnIntMessage :: String -> String
notAnIntMessage str =
  "\"" <> str <> "\"" <> " is not an integer"

factorisationMessage :: Int -> Int -> Int -> String
factorisationMessage n k l =
  show n <> " = " <> show k <> " * " <> show l

loop :: IO ()
loop = do
  hSetBuffering stdout NoBuffering 
  putStr "enter an integer: "
  hSetBuffering stdout LineBuffering 

  input <- getLine

  case readMaybe input of
    Nothing -> putStrLn (notAnIntMessage input)
    Just n  -> let k = factor n
                   l = n `div` k
               in putStrLn (factorisationMessage n k l)

  loop

main :: IO ()
main = loop
