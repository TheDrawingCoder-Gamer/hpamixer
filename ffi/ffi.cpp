#include "device.hh"
#include <pulse/pulseaudio.h>
#include <string>
#include <list>
#include <array>
#include <vector>
#include <algorithm>
#include "ffi.h"
#include "pulseaudio.hh"
template <typename D, typename T>
std::vector<D*> magic_list(const std::list<T> &list) {
    auto ret = std::vector<D*>(list.size());
    ret.reserve(list.size());
    for (auto elem : list) {
        ret.push_back(reinterpret_cast<D*>(&elem));
    }
    return ret;
}
template <typename T>
harray_t* to_c_array(std::vector<T> vector) {
    harray_t* arr = (harray_t*) malloc(sizeof(harray_t));
    arr->size = vector.size();
    arr->arr = vector.data();
    return arr;
}

extern "C" {
   

    ffi_pulseaudio* pulseaudio_create(const char* name) {
	return reinterpret_cast< ffi_pulseaudio* >(new Pulseaudio(std::string(name)));
    }
    void pulseaudio_destroy(ffi_pulseaudio* obj) {
        auto pulse = reinterpret_cast< Pulseaudio* >(obj);
	delete pulse;
    }

    harray_t* pulseaudio_get_sinks(ffi_pulseaudio* obj) {
        auto pulse = reinterpret_cast< Pulseaudio* >(obj);
        auto sinks = pulse->get_sinks();

        return to_c_array(magic_list<ffi_device>(sinks));
    }
    harray_t* pulseaudio_get_sources(ffi_pulseaudio* obj) {
        auto pulse = reinterpret_cast< Pulseaudio* >(obj);
        return to_c_array(magic_list<ffi_device>(pulse->get_sources()));
    }
    ffi_device* pulseaudio_get_sink_index(ffi_pulseaudio* obj, uint32_t index) {
        auto pulse = reinterpret_cast< Pulseaudio* >(obj);
        Device* sink = new Device(pulse->get_sink(index));
	return reinterpret_cast<ffi_device*>(sink);
    }
    ffi_device* pulseaudio_get_sink_name(ffi_pulseaudio* obj, const char* name) {
        auto pulse = reinterpret_cast< Pulseaudio* >(obj);
        Device* sink = new Device(pulse->get_sink(std::string(name)));
	return reinterpret_cast<ffi_device*>(sink);
    }
    ffi_device* pulseaudio_get_source_index(ffi_pulseaudio* obj, uint32_t index) {
        auto pulse = reinterpret_cast<Pulseaudio *>(obj);
        Device* source = new Device(pulse->get_source(index));
	return reinterpret_cast<ffi_device *>(source);
    }
    ffi_device* pulseaudio_get_source_name(ffi_pulseaudio* obj, const char* name) {
        auto pulse = reinterpret_cast<Pulseaudio *>(obj);
        Device* source = new Device(pulse->get_source(std::string(name)));
	return reinterpret_cast<ffi_device *>(source);
    }
    ffi_device* pulseaudio_get_default_source(ffi_pulseaudio* obj) {
        auto pulse = reinterpret_cast<Pulseaudio* >(obj);
        Device* source = new Device(pulse->get_default_source());
        return reinterpret_cast<ffi_device*>(source);
    }
    ffi_device* pulseaudio_get_default_sink(ffi_pulseaudio* obj) {
        auto pulse = reinterpret_cast<Pulseaudio* >(obj);
        Device* sink;
        sink = new Device(pulse->get_default_sink());
        return reinterpret_cast<ffi_device*>(sink);
    }
    void pulseaudio_set_volume(ffi_pulseaudio* obj, ffi_device* device, uint32_t new_volume) {
        auto pulse = reinterpret_cast<Pulseaudio *>(obj);
	pulse->set_volume(reinterpret_cast<Device&>(device), new_volume);
    }
    void pulseaudio_set_mute(ffi_pulseaudio* obj, ffi_device* device, bool mute) {
        auto pulse = reinterpret_cast<Pulseaudio *>(obj);
	pulse->set_mute(reinterpret_cast<Device&>(device), mute);
    }
    uint32_t device_index(ffi_device* device) {
        return reinterpret_cast<Device *>(device)->index;
    }
    int device_type(ffi_device* device) {
        return reinterpret_cast<Device* >(device)->type;
    }
    const char* device_name(ffi_device* device) {
        return reinterpret_cast<Device* >(device)->name.c_str();
    }
    const char* device_description(ffi_device* device) {
        return reinterpret_cast<Device* >(device)->description.c_str();
    }
    int device_state(ffi_device* device) {
        return reinterpret_cast<Device *>(device)->state;
    }
    /*
    pa_cvolume device_volume(ffi_device* device) {
        return reinterpret_cast<Device *>(device)->volume;
    }
    */
    uint32_t device_volume_avg(ffi_device* device) {
        return reinterpret_cast<Device *>(device)->volume_avg;
    }

    int device_volume_percent(ffi_device* device) {
        return reinterpret_cast<Device* >(device)->volume_percent;
    }
    bool device_mute(ffi_device* device) {
        return reinterpret_cast<Device* >(device)->mute;
    }
    /*
    ffi_device* device_create_source(const pa_source_info* info) {
        return reinterpret_cast<ffi_device*>(new Device(info));
    }
    ffi_device* device_create_sink(const pa_sink_info* info) {
        return reinterpret_cast<ffi_device*>(new Device(info));
    }
    */
    ffi_server_info* server_create() {
        return reinterpret_cast<ffi_server_info* >(new ServerInfo());
    }
    void server_destroy(ffi_server_info* info) {
	auto server = reinterpret_cast<ServerInfo* >(info);
	delete server;
    }
    const char* server_default_source_name(ffi_server_info* info){
        auto server = reinterpret_cast<ServerInfo* >(info);
        auto name = server->default_source_name;

        return name.c_str();
    } 
    const char* server_default_sink_name(ffi_server_info* info) {
        auto server = reinterpret_cast<ServerInfo* >(info);
        return server->default_sink_name.c_str();
    }

    void device_destroy(ffi_device* device) {
	auto obj = reinterpret_cast<Device*>(device);
	delete obj;
    }
    uint32_t harray_size(harray_t* arr) {
        return arr->size;
    }
    void* harray_array(harray_t* arr) {
        return arr->arr;
    }
    
}
