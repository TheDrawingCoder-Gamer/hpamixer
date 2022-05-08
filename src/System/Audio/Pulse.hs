module System.Audio.Pulse
(module System.Audio.Pulse.Internal
    ,deviceVolumePercent) 
where

import System.Audio.Pulse.Internal hiding (deviceVolumePercent)
import qualified System.Audio.Pulse.Internal as A

deviceVolumePercent :: Device -> IO Int 
deviceVolumePercent dev = 
    do 
        mute <- deviceMute dev
        if mute then 
            pure 0 
        else 
            A.deviceVolumePercent dev


