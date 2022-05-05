module Main where

import System.Audio

main :: IO ()
main = do 
    pulse <- connectPulse "examplepulse" 
    dev <- pulseDefaultSource pulse 
    deviceVolume dev >>= print
