{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE CPP, CApiFFI #-}
module System.Audio.Pulse.Internal (
    connectPulse,
    pulseSetVolume,
    pulseSetMute,
    pulseGetSinks,
    pulseGetSources,
    pulseGetSourceByIndex,
    pulseGetSourceByName,
    pulseGetSinkByIndex,
    pulseGetSinkByName,
    pulseDefaultSink,
    pulseDefaultSource,
    PulseState(..),
    DeviceType(..),
    DeviceState(..),
    Pulseaudio,
    Device,
    deviceIndex,
    deviceType,
    deviceName,
    deviceDescription,
    deviceState,
    deviceChannelVolumes,
    deviceVolume,
    deviceVolumePercent,
    deviceMute,
    printDevice
) where 

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils (fromBool, toBool)
import Data.Functor ((<&>))
import Control.Exception (mask_)
import Control.Monad ((>=>))
foreign import capi "ffi.h pulseaudio_create" createPulse 
    :: CString -> IO (Ptr CPulseaudio)
foreign import capi "ffi.h &pulseaudio_destroy" destroyPulse
    :: FunPtr (Ptr CPulseaudio -> IO())
foreign import capi "ffi.h pulseaudio_get_sinks" c_pulseGetSinks 
    :: Ptr CPulseaudio -> IO (Ptr (Ptr CDevice))
foreign import capi "ffi.h pulseaudio_get_sources" c_pulseGetSources
    :: Ptr CPulseaudio -> IO (Ptr (Ptr CDevice))
foreign import capi "ffi.h pulseaudio_get_sink_index" c_pulseGetSinkIndex
    :: Ptr CPulseaudio -> Word32 -> IO (Ptr CDevice) 
foreign import capi "ffi.h pulseaudio_get_sink_name"  c_pulseGetSinkName 
    :: Ptr CPulseaudio -> CString -> IO (Ptr CDevice) 
foreign import capi "ffi.h pulseaudio_get_source_index" c_pulseGetSourceIndex 
    :: Ptr CPulseaudio -> Word32 -> IO (Ptr CDevice) 
foreign import capi "ffi.h pulseaudio_get_source_name"  c_pulseGetSourceName 
   :: Ptr CPulseaudio -> CString -> IO (Ptr CDevice)
foreign import capi "ffi.h pulseaudio_get_default_sink" c_pulseGetDefaultSink 
    :: Ptr CPulseaudio -> IO (Ptr CDevice) 
foreign import capi "ffi.h pulseaudio_get_default_source" c_pulseGetDefaultSource 
    :: Ptr CPulseaudio -> IO (Ptr CDevice)
foreign import capi "ffi.h pulseaudio_set_volume" c_pulseSetVolume 
    :: Ptr CPulseaudio -> Ptr CDevice -> Word32 -> IO () 
foreign import capi "ffi.h pulseaudio_set_mute" c_pulseSetMute
    :: Ptr CPulseaudio ->  Ptr CDevice -> CBool -> IO ()
foreign import capi "ffi.h device_index" c_deviceIndex
    :: Ptr CDevice -> IO Word32
foreign import capi "ffi.h device_type" c_deviceType
    -- is an enum
    :: Ptr CDevice -> IO Int 
foreign import capi "ffi.h device_name" c_deviceName 
    :: Ptr CDevice -> IO CString 
foreign import capi "ffi.h device_description" c_deviceDescription
    :: Ptr CDevice -> IO CString 
foreign import capi "ffi.h device_state" c_deviceState 
    -- is an enum
    :: Ptr CDevice -> IO Int
foreign import capi "ffi.h device_volume" c_devChannelVol
    :: Ptr CDevice -> IO (Ptr Word32)
foreign import capi "ffi.h device_channels" c_devChannelCount
    :: Ptr CDevice -> IO Word8
foreign import capi "ffi.h device_volume_avg" c_deviceVolume 
    :: Ptr CDevice -> IO Word32
foreign import capi "ffi.h device_volume_percent" c_deviceVolumePercent
    :: Ptr CDevice -> IO Int 
foreign import capi "ffi.h device_mute" c_deviceMute 
    :: Ptr CDevice -> IO Bool 
-- not dealing with creation  because then I have to write externs for pa_source_info 
foreign import capi "ffi.h &device_destroy" c_deviceDestroy 
    :: FunPtr (Ptr CDevice -> IO ())
data PulseState
    = Connecting 
    | Connected
    | Error 
    deriving (Enum, Bounded)
data DeviceType 
    = Source
    | Sink 
    deriving (Enum, Bounded)
data DeviceState
    = Invalid
    | Running
    | Idle
    | Suspended 
    deriving (Enum, Bounded)

data CPulseaudio
data CDevice
data CHArray contents
newtype Pulseaudio = Pulseaudio { unwrapPulse :: ForeignPtr CPulseaudio }
newtype Device = Device { unwrapDevice :: ForeignPtr CDevice } 
fromCEnum :: Enum a => a -> CInt 
fromCEnum = fromIntegral . fromEnum
toCEnum :: Enum a => CInt -> a 
toCEnum = toEnum . fromIntegral

connectPulse :: String -> IO Pulseaudio
connectPulse name = mask_ $ do 
    cname <- newCString name
    ptr <- createPulse cname 
    Pulseaudio <$> newForeignPtr destroyPulse ptr

pulseSetMute :: Pulseaudio -> Device -> Bool -> IO()
pulseSetMute (Pulseaudio pulse) (Device dev) mute = mask_ $ do
    withForeignPtr pulse (\a ->  
         withForeignPtr dev $ \d -> c_pulseSetMute a d (fromBool mute)
         )
pulseSetVolume :: Pulseaudio -> Device -> Word32 -> IO ()
pulseSetVolume (Pulseaudio pulse') (Device device) volume = mask_ $ 
    withForeignPtr pulse' (\pulse ->  
            withForeignPtr device $ \dev -> c_pulseSetVolume pulse dev volume)

pulseGetSinks :: Pulseaudio -> IO [Device] 
pulseGetSinks (Pulseaudio pulse') = withForeignPtr pulse' $ \pulse -> 
    c_pulseGetSinks pulse >>= peekArray0 nullPtr >>= traverse wrapDevice
pulseGetSources :: Pulseaudio -> IO [Device]  
pulseGetSources (Pulseaudio pulse') = withForeignPtr pulse' $ \pulse ->
    c_pulseGetSources pulse >>= peekArray0 nullPtr >>= traverse wrapDevice

pulseGetSourceByIndex :: Pulseaudio -> Word32 -> IO Device  
pulseGetSourceByIndex (Pulseaudio pulse') idx = withForeignPtr pulse' $ \pulse -> 
    c_pulseGetSourceIndex pulse idx >>= wrapDevice
pulseGetSinkByIndex :: Pulseaudio -> Word32 -> IO Device
pulseGetSinkByIndex (Pulseaudio pulse') idx = withForeignPtr pulse' $ \pulse -> 
    c_pulseGetSinkIndex pulse idx >>= wrapDevice
pulseGetSourceByName :: Pulseaudio -> String -> IO Device
pulseGetSinkByName :: Pulseaudio -> String -> IO Device
pulseGetSourceByName (Pulseaudio pulse') name = withForeignPtr pulse' $ \pulse -> 
    newCString name >>= c_pulseGetSourceName pulse >>= wrapDevice
pulseGetSinkByName (Pulseaudio pulse') name = withForeignPtr pulse' $ \pulse -> 
    newCString name >>= c_pulseGetSinkName pulse >>= wrapDevice
pulseDefaultSink :: Pulseaudio -> IO Device 
pulseDefaultSink (Pulseaudio pulse') = withForeignPtr pulse' $ c_pulseGetDefaultSink >=> wrapDevice
pulseDefaultSource :: Pulseaudio -> IO Device 
pulseDefaultSource (Pulseaudio pulse') = withForeignPtr pulse' $ c_pulseGetDefaultSource >=> wrapDevice
deviceIndex :: Device -> IO Int
deviceIndex (Device dev) = withForeignPtr dev $ fmap fromIntegral . c_deviceIndex
deviceType :: Device -> IO DeviceType 
deviceType (Device dev) = withForeignPtr dev $ fmap toEnum . c_deviceType
deviceName :: Device -> IO String
deviceName (Device dev) = withForeignPtr dev $ c_deviceName >=> peekCString
deviceDescription :: Device -> IO String 
deviceDescription (Device dev) = withForeignPtr dev $ c_deviceDescription >=> peekCString
deviceState :: Device -> IO DeviceState 
deviceState (Device dev) = withForeignPtr dev $ fmap toEnum . c_deviceState
deviceVolume :: Device -> IO Int 
deviceVolume (Device dev) = withForeignPtr dev $ fmap fromIntegral . c_deviceVolume
deviceVolumePercent :: Device -> IO Int
deviceVolumePercent (Device dev) = withForeignPtr dev $ fmap fromIntegral . c_deviceVolumePercent
deviceMute :: Device -> IO Bool 
deviceMute (Device dev) = withForeignPtr dev c_deviceMute
deviceChannelVolumes :: Device -> IO [Word32]
deviceChannelVolumes (Device d) = withForeignPtr d $ \dev -> do 
    ptr <- c_devChannelVol  dev
    count <- c_devChannelCount dev 
    peekArray (fromIntegral count) ptr
wrapDevice :: Ptr CDevice -> IO Device 
wrapDevice dev =  
    Device <$> newForeignPtr c_deviceDestroy dev

printDevice :: Device -> IO () 
printDevice dev = do 
    mute <- deviceMute dev 
    volume <- deviceVolumePercent dev 
    print $ show volume ++ " " ++ show mute
