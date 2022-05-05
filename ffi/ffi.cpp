#include "ffi.h"
#include "device.hh"
#include <pulse/pulseaudio.h>
#include "pulseaudio.hh"
#include <string>
#include <list>
#include <array>
#include <vector>
#include <algorithm>
extern "C" {
    ffi_pulseaudio* pulseaudio_create(const char* name) {
        auto object = new Pulseaudio(std::string(name));
	return reinterpret_cast<ffi_pulseaudio*>(object);
    }
    void pulseaudio_destroy(ffi_pulseaudio* obj) {
        auto pulse = reinterpret_cast<Pulseaudio*>(obj);
	delete pulse;
    }

    haskell_array* pulseaudio_get_sinks(ffi_pulseaudio* obj) {
        auto pulse = reinterpret_cast<Pulseaudio*>(obj);
        return to_c_array(reinterpret_cast<std::list<ffi_pulseaudio*>(pulse->get_sinks()));
    }
    haskell_array* pulseaudio_get_sources(ffi_pulseaudio* obj) {
        auto pulse = reinterpret_cast<Pulseaudio*>(obj);
        return to_c_array(reinterpret_cast<std::list<ffi_pulseaudio*>((pulse->get_sources())&));
    }
    ffi_device* pulseaudio_get_sink_index(ffi_pulseaudio* obj, uint32_t index) {
        auto pulse = reinterpret_cast<Pulseaudio*>(obj);
	return reinterpret_cast<ffi_device*>(obj->get_sink(index));
    }
    ffi_device* pulseaudio_get_sink_name(ffi_pulseaudio* obj, const char* name) {
        auto pulse = reinterpret_cast<Pulseaudio*>(obj);
	return reinterpret_cast<ffi_device*>(obj->get_sink(std::string(name)));
    }
    ffi_device* pulseaudio_get_source_index(ffi_pulseaudio* obj, uint32_t index) {
        auto pulse = reinterpret_cast<Pulseaudio*>(obj);
	return reinterpret_cast<ffi_device*>(obj->get_source(index));
    }
    ffi_device* pulseaudio_get_source_name(ffi_pulseaudio* obj, const char* name) {
        auto pulse = reinterpret_cast<Pulseaudio*>(obj);
	return reinterpret_cast<ffi_device*>(obj->get_source(std::string(name)));
    }
    void pulseaudio_set_volume(ffi_pulseaudio* obj, ffi_device* device, pa_volume_t new_volume) {
        auto pulse = reinterpret_cast<Pulseaudio*>(obj);
	pulse->set_volume(reinterpret_cast<Device&>(device), new_volume);
    }
    void pulseaudio_set_mute(ffi_pulseaudio* obj, ffi_device* device, bool mute) {
        auto pulse = reinterpret_cast<Pulseaudio*>(obj);
	pulse->set_mute(reinterpret_cast<Device&>(device), mute);
    }
    uint32_t device_index(ffi_device* device) {
        return reinterpret_cast<Device*>(device)->index;
    }
    int device_type(ffi_device* device) {
        return reinterpret_cast<Device*>(device)->type;
    }
    const char* device_name(ffi_device* device) {
        return reinterpret_cast<Device*>(device)->name.c_str();
    }
    const char* device_description(ffi_device* device) {
        return reinterpret_cast<Device*>(device)->description.c_str();
    }
    int device_state(ffi_device* device) {
        return reinterpret_cast<Device*>(device)->state;
    }
    pa_cvolume device_volume(ffi_device* device) {
        return reinterpret_cast<Device*>(device)->volume;
    }
    pa_volume_t device_volume_avg(ffi_device* device) {
        return reinterpret_cast<Device*>(device)->volume_avg;
    }

    int device_volume_percent(ffi_device* device) {
        return reinterpret_cast<Device*>(device)->volume_percent;
    }
    bool device_mute(ffi_device* device) {
        return reinterpret_cast<Device*>(device)->mute;
    }

    ffi_device* device_create_source(const pa_source_info* info) {
	return reinterpret_cast<ffi_device*>(new Device(info));
    }
    ffi_device* device_create_sink(const pa_sink_info* info) {
        return reinterpret_cast<ffi_device*>(new Device(info));
    }

    ffi_server_info* server_create() {
        return reinterpet_cast<ffi_server_info*>(new ServerInfo());
    }
    void server_destroy(ffi_server_info* info) {
	auto server = reinterpret_cast<ServerInfo*>(info);
	delete server;
    }
    const char* server_default_source_name(ffi_server_info* info){
        return reinterpret_cast<ServerInfo*>(info)->default_source_name.c_str();
    } 
    const char* server_default_sink_name(ffi_server_info* info) {
        return reinterpret_cast<ServerInfo*>(info)->default_sink_name.c_str();
    }

    void device_destroy(ffi_device* device) {
	auto obj = reinterpret_cast<Device*>(device);
	delete device;
    }
    uint32_t harray_size(haskell_array* arr) {
        return arr->size;
    }
    void* harray_array(haskell_array* arr) {
        return arr->arr;
    }
haskell_array* to_c_array(std::list<void*> list) {
    std::vector<void*> vector = new std::vector(list.size());
    std::copy(list.begin(), list.end(), vector);
    struct haskell_array* arr = (struct haskell_array*) malloc(sizeof(struct haskell_array));
    arr->size = vector.size();
    arr->arr = vector.data();
    return arr;
}
    
}
