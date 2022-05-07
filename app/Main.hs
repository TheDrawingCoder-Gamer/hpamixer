module Main where

import System.Audio
import Data.Foldable
import Control.Monad(join)
main :: IO ()
main = do 
    pulse <- connectPulse "examplepulse" 
    mapM_ printDevice =<< pulseGetSources pulse
    putStrLn "hi"
