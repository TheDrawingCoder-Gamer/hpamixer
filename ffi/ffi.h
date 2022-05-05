#include <pulse/pulseaudio.h>
extern "C" {
    typedef struct ffi_server_info;
    ffi_server_info* server_create();
    void server_destroy(ffi_server_info* object);
    const char* server_default_source_name(ffi_server_info* object);
    const char* server_default_sink_name(ffi_server_info* object);
    typedef struct ffi_pulseaudio;
    // is an enum
    int pulseaudio_state(ffi_pulseaudio* object);
    ffi_pulseaudio* pulseaudio_create(const char* name);
    void pulseaudio_destroy(ffi_pulseaudio* object);
    ffi_device** pulseaudio_get_sinks(ffi_pulseaudio* object);
    ffi_device** pulseaudio_get_sources(ffi_pulseaudio* object);
    ffi_device* pulseaudio_get_sink_index(ffi_pulseaudio* obj, uint32_t index);
    ffi_device* pulseaudio_get_sink_name(ffi_pulseaudio* obj const char* name);
    ffi_device* pulseaudio_get_source_index(ffi_pulseaudio* obj, uint32_t index);
    ffi_device* pulseaudio_get_source_name(ffi_pulseaudio* obj, const char* name);
    void pulseaudio_set_volume(ffi_pulseaudio* obj, ffi_device* device, pa_volume_t new_volume);
    void pulseaudio_set_mute(ffi_pulseaudio* obj, ffi_device* device, bool mute);

    typedef struct ffi_device;
    uint32_t device_index(ffi_device* obj);
    int device_type(ffi_device* obj);
    const char* device_name(ffi_device* obj);
    const char* device_description(ffi_device* obj);
    int device_state(ffi_device* obj);
    pa_cvolume device_volume(ffi_device* obj);
    pa_volume_t device_volume_avg(ffi_device* obj);
    int device_volume_percent(ffi_device* obj);
    bool device_mute(ffi_device* obj);
    ffi_device* device_create_source(const pa_source_info* i);
    ffi_device* device_create_sink(const pa_sink_info* i);
    void device_destroy(ffi_device* obj);
    bool device_mute(ffi_device* obj);

    struct haskell_array {
        uint32_t size 
        void* arr
    }

    uint32_t harray_size(haskell_array* arr);
    void* harray_array(haskell_array* arr);
}
