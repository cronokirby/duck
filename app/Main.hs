module Main (main) where

import Relude
import System.Process (callProcess)

main :: IO ()
main = do
  putTextLn "Calling GH"
  callProcess "gh" ["--version"]
