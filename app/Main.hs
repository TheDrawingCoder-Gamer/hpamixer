module Main where

import System.Audio.Pulse
import Data.Foldable
import Control.Monad(join)
main :: IO ()
main = do 
    pulse <- connectPulse "pamixer"
    -- default OUTPUT :sob:
    dev <- pulseDefaultSink pulse 
    deviceName dev >>= putStrLn
    deviceVolumePercent dev >>= print
    deviceIndex dev >>= print
    deviceMute dev >>= print
    mapM_ printDevice =<< pulseGetSources pulse
    putStrLn "hi"
