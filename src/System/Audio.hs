module System.Audio 
    (module System.Audio.Internal
    ,deviceVolumePercent) 
where

import System.Audio.Internal hiding (deviceVolumePercent)
import qualified System.Audio.Internal as A

deviceVolumePercent :: Device -> IO Int 
deviceVolumePercent dev = 
    do 
        mute <- deviceMute dev
        if mute then 
            pure 0 
        else 
            A.deviceVolumePercent dev


