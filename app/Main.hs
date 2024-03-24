{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
module Main where

import Kripke
import System.Exit (exitSuccess)

data ReplOption where
  LoadFile :: String -> ReplOption
  ValidateTerm :: String -> ReplOption
  SetWorld :: String -> ReplOption
  AddWorld :: String -> ReplOption
  AddOrd :: String -> ReplOption
  ClearWorld :: ReplOption
  Help :: ReplOption
  Quit :: ReplOption
  Error :: String -> ReplOption

main :: IO ()
main = mainLoop emptyModel ""

process :: KripkeModel -> String -> ReplOption -> IO (KripkeModel, String)
process _ wrld (LoadFile fn) = do
  nkm <- processFile <$> readFile fn
  print nkm
  return (nkm, wrld)
process km wrld (ValidateTerm _) = print km >> return (km, wrld) -- Need to define a parser for props, this should print the output of validation
process km _ (SetWorld nwrld) = print km >> return (km, nwrld)
process km wrld (AddWorld nterm) = let nm = addWorld km (readWorldTerm nterm) in print nm >> return (nm, wrld)
process km wrld (AddOrd nterm) = let nm = addOrd km (readWorldOrd nterm) in print nm >> return (nm, wrld)
process _ wrld ClearWorld = print emptyModel >> return (emptyModel, wrld)
process km wrld Help = print km >> return (km, wrld)
process _ _ Quit = exitSuccess
process km wrld (Error msg) = putStrLn msg >> return (km, wrld)

mainLoop :: KripkeModel -> String -> IO ()
mainLoop km s = do
  input <- replSeq
  (nkm, ns) <- process km s input
  mainLoop nkm ns

  return ()

findOption :: String -> ReplOption
findOption (':' : 'l' : xs) = LoadFile (dropWhile (==' ') xs)
findOption (':' : 'v' : 't' : xs) = ValidateTerm xs
findOption (':' : 'a' : 'w' : xs) = AddWorld xs
findOption (':' : 'a' : 'o' : xs) = AddOrd xs
findOption (':' : 'w' : xs) = SetWorld xs
findOption ":clear" = ClearWorld
findOption ":q" = Quit
findOption ":help" = Help
findOption s = Error ("Unable to process string " ++ s)

replSeq :: IO ReplOption
replSeq = findOption <$> getLine
