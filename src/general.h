
#ifndef GENERAL_H
#define GENERAL_H


#include <stdint.h>
#include <string.h> // strlen
#include <assert.h>

#include <new>
#include <initializer_list>
#include <string_view>
#include <vector>

#include <stdio.h>

namespace josh {

using String = std::string_view;

template <typename T>
using Array = std::vector<T>;

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t  u8;

typedef int64_t  s64;
typedef int32_t  s32;
typedef int16_t  s16;
typedef int8_t   s8;

const u8  U8_MAX  = 0xFF;
const u16 U16_MAX = 0xFFFF;
const u32 U32_MAX = 0xFFFFFFFF;

template <typename T>
struct Array_Slice {
    bool list_initialized = false;
    T *data;
    size_t _count;

    Array_Slice() {
        list_initialized = false;
        data = nullptr;
        _count = 0;
    }

    Array_Slice(const Array_Slice<T> &other) {
        list_initialized = false;
        data = other.data;
        _count = other._count;
    }

    Array_Slice(const Array<T> &a) {
        list_initialized = false;
        data   = const_cast<T *>(a.data());
        _count = a.size();
    }

    Array_Slice(std::initializer_list<T> list) {
        _count = list.size();
        data = (T *)malloc(sizeof(T) * _count);

        memcpy(data, list.begin(), sizeof(T) * _count);

        list_initialized = true;
    }

    ~Array_Slice() {
        if (list_initialized && data) free(data);

        data  = nullptr;
        _count = 0;
    }

    const T *begin() const {
        return &data[0];
    }

    const T *end() const {
        if (_count) return &data[_count];
        return nullptr;
    }

    size_t size() const
    {
        return _count;
    }
};

template <typename T>
T ensure_aligned(T value, T alignment) {
    T align_bits = alignment - 1;
    if (value & align_bits) value += (alignment - (value & align_bits));
    return value;
}

struct Data_Buffer {
    struct Chunk {
        u8 *data      = nullptr;
        size_t count     = 0;
        size_t reserved  = 0;
    };

    Array<Chunk> chunks;

    Data_Buffer() {
        new_chunk();
    }

    Chunk *new_chunk(size_t size = 4096) {
        if (size < 4096) size = 4096;
        Chunk c;
        c.data     = (u8 *)malloc(size);
        c.count    = 0;
        c.reserved = size;

        chunks.push_back(c);
        return &chunks[chunks.size()-1];
    }

    void append(const void *src, size_t size) {
        Chunk *c = &chunks[chunks.size()-1];
        if (c->count+size >= c->reserved) c = new_chunk(size);

        memcpy(c->data+c->count, src, size);
        c->count += size;
    }

    void append_string(const String& s) {
        append(s.data(), (u32)s.length());
    }

    void *allocate_bytes_unaligned(size_t size) {
        Chunk *c = &chunks[chunks.size()-1];
        if (c->count+size >= c->reserved) c = new_chunk(size);

        void *result = c->data+c->count;
        c->count += size;
        return result;
    }

    template<typename T>
    T *allocate_unaligned() {
        return (T *)allocate_bytes_unaligned(sizeof(T));
    }

    template<typename T>
    T *allocate() {
        size_t align = alignof(T);

        Chunk *c = &chunks[chunks.size()-1];
        auto count = ensure_aligned(c->count, align);
        if (count+sizeof(T) >= c->reserved) c = new_chunk(sizeof(T));

        c->count = ensure_aligned(c->count, align);
        void *result = c->data+c->count;
        c->count += sizeof(T);
        return (T *)result;
    }

    void append(Data_Buffer *other) {
        for (auto &c : other->chunks) {
            append(c.data, c.count);
        }
    }

    void append_byte(u8 byte) {
        append(&byte, 1);
    }

    u32 size() {
        u32 size = 0;
        for (auto &c : chunks) {
            size += c.count;
        }
        return size;
    }
};

} // namespace josh

#endif