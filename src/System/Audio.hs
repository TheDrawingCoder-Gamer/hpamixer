{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE CPP, CApiFFI #-}
module System.Audio (
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
    deviceVolume,
    deviceVolumePercent,
    deviceMute
) where 

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils (fromBool, toBool)
import Data.Functor ((<&>))
import Control.Exception (mask_)
foreign import capi "ffi.h pulseaudio_create" createPulse 
    :: CString -> IO (Ptr CPulseaudio)
foreign import capi "ffi.h &pulseaudio_destroy" destroyPulse
    :: FunPtr (Ptr CPulseaudio -> IO())
foreign import capi "ffi.h pulseaudio_get_sinks" c_pulseGetSinks 
    :: Ptr CPulseaudio -> IO (Ptr (CHArray (Ptr CDevice)))
foreign import capi "ffi.h pulseaudio_get_sources" c_pulseGetSources
    :: Ptr CPulseaudio -> IO (Ptr (CHArray (Ptr CDevice)))
foreign import capi "ffi.h pulseaudio_get_sink_index" c_pulseGetSinkIndex
    :: Ptr CPulseaudio -> CUInt -> IO (Ptr CDevice) 
foreign import capi "ffi.h pulseaudio_get_sink_name"  c_pulseGetSinkName 
    :: Ptr CPulseaudio -> CString -> IO (Ptr CDevice) 
foreign import capi "ffi.h pulseaudio_get_source_index" c_pulseGetSourceIndex 
    :: Ptr CPulseaudio -> CUInt -> IO (Ptr CDevice) 
foreign import capi "ffi.h pulseaudio_get_source_name"  c_pulseGetSourceName 
   :: Ptr CPulseaudio -> CString -> IO (Ptr CDevice)
foreign import capi "ffi.h pulseaudio_get_default_sink" c_pulseGetDefaultSink 
    :: Ptr CPulseaudio -> IO (Ptr CDevice) 
foreign import capi "ffi.h pulseaudio_get_default_source" c_pulseGetDefaultSource 
    :: Ptr CPulseaudio -> IO (Ptr CDevice)
foreign import capi "ffi.h pulseaudio_set_volume" c_pulseSetVolume 
    :: Ptr CPulseaudio -> Ptr CDevice -> CUInt -> IO () 
foreign import capi "ffi.h pulseaudio_set_mute" c_pulseSetMute
    :: Ptr CPulseaudio ->  Ptr CDevice -> CBool -> IO ()
foreign import capi "ffi.h device_index" c_deviceIndex
    :: Ptr CDevice -> IO CUInt
foreign import capi "ffi.h device_type" c_deviceType
    -- is an enum
    :: Ptr CDevice -> IO CInt 
foreign import capi "ffi.h device_name" c_deviceName 
    :: Ptr CDevice -> IO CString 
foreign import capi "ffi.h device_description" c_deviceDescription
    :: Ptr CDevice -> IO CString 
foreign import capi "ffi.h device_state" c_deviceState 
    -- is an enum
    :: Ptr CDevice -> IO CInt
-- ignoring device.volume because sanity
foreign import capi "ffi.h device_volume_avg" c_deviceVolume 
    :: Ptr CDevice -> IO CUInt
foreign import capi "ffi.h device_volume_percent" c_deviceVolumePercent
    :: Ptr CDevice -> IO CInt 
foreign import capi "ffi.h device_mute" c_deviceMute 
    :: Ptr CDevice -> IO CBool -- Word8 LOL
-- not dealing with creation  because then I have to write externs for pa_source_info 
foreign import capi "ffi.h &device_destroy" c_deviceDestroy 
    :: FunPtr (Ptr CDevice -> IO ())
foreign import capi "ffi.h harray_size" harraySize 
    :: Ptr (CHArray a) -> IO Int 
foreign import capi "ffi.h harray_array" harrayData 
    :: Ptr (CHArray a) -> IO (Ptr a)
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
-- CDevice SHOULDN'T BE KILLED!
newtype Device = Device { unwrapDevice :: Ptr CDevice } 
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
         c_pulseSetMute a dev (fromBool mute)
         )
pulseSetVolume :: Pulseaudio -> Device -> CUInt -> IO ()
pulseSetVolume (Pulseaudio pulse') (Device device) volume = mask_ $ 
    withForeignPtr pulse' (\pulse ->  
            c_pulseSetVolume pulse device volume)

pulseGetSinks :: Pulseaudio -> IO [Device] 
pulseGetSinks (Pulseaudio pulse') = withForeignPtr pulse' $ \pulse -> 
    c_pulseGetSinks pulse >>= harrayToList <&> map Device
pulseGetSources :: Pulseaudio -> IO [Device]  
pulseGetSources (Pulseaudio pulse') = withForeignPtr pulse' $ \pulse ->
    c_pulseGetSources pulse >>= harrayToList <&> map Device
pulseGetSourceByIndex :: Pulseaudio -> CUInt -> IO Device  
pulseGetSourceByIndex (Pulseaudio pulse') idx = withForeignPtr pulse' $ \pulse -> 
    Device <$> c_pulseGetSourceIndex pulse idx
pulseGetSinkByIndex :: Pulseaudio -> CUInt -> IO Device
pulseGetSinkByIndex (Pulseaudio pulse') idx = withForeignPtr pulse' $ \pulse -> 
    Device <$> c_pulseGetSinkIndex pulse idx
pulseGetSourceByName :: Pulseaudio -> String -> IO Device
pulseGetSinkByName :: Pulseaudio -> String -> IO Device
pulseGetSourceByName (Pulseaudio pulse') name = withForeignPtr pulse' $ \pulse -> 
    newCString name >>= c_pulseGetSourceName pulse <&> Device
pulseGetSinkByName (Pulseaudio pulse') name = withForeignPtr pulse' $ \pulse -> 
    newCString name >>= c_pulseGetSinkName pulse <&> Device
pulseDefaultSink :: Pulseaudio -> IO Device 
pulseDefaultSink (Pulseaudio pulse') = withForeignPtr pulse' $ \pulse -> 
    c_pulseGetDefaultSink pulse <&> Device
pulseDefaultSource :: Pulseaudio -> IO Device 
pulseDefaultSource (Pulseaudio pulse') = withForeignPtr pulse' $ \pulse -> 
    c_pulseGetDefaultSource pulse <&> Device
harrayToList :: Storable a => Ptr (CHArray a) -> IO [a] 
harrayToList harray = do
    size <- harraySize harray 
    contents <- harrayData harray 
    free harray
    peekArray (fromIntegral size) contents
deviceIndex :: Device -> IO Int
deviceIndex (Device dev) = fromIntegral <$> c_deviceIndex dev
deviceType :: Device -> IO DeviceType 
deviceType (Device dev) = toCEnum <$> c_deviceType dev
deviceName :: Device -> IO String 
deviceName (Device dev) = c_deviceName dev >>= peekCString
deviceDescription :: Device -> IO String 
deviceDescription (Device dev) = c_deviceDescription dev >>= peekCString
deviceState :: Device -> IO DeviceState 
deviceState (Device dev) = toCEnum <$> c_deviceState dev
deviceVolume :: Device -> IO Int 
deviceVolume (Device dev) = fromIntegral <$> c_deviceVolume dev
deviceVolumePercent :: Device -> IO Int
deviceVolumePercent (Device dev) = fromIntegral <$> c_deviceVolumePercent dev
deviceMute :: Device -> IO Bool 
deviceMute (Device dev) = toBool <$> c_deviceMute dev
