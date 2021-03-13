
#ifndef GENERAL_H
#define GENERAL_H


#include <stdint.h>
#include <string.h> // strlen
#include <assert.h>

#include <new>
#include <initializer_list>
#include <string_view>

#include <stdio.h>

namespace josh {

using String = std::string_view;

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
struct Array {
    T *data = nullptr;
    u32 count    = 0;
    u32 reserved = 0;

    static const u32 DEFAULT_ARRAY_RESERVE_SIZE = 16;

    void reserve(u32 amount) {
        if (amount <= reserved) return;
        if (amount < DEFAULT_ARRAY_RESERVE_SIZE) amount = DEFAULT_ARRAY_RESERVE_SIZE;

        data     = (T *)realloc(data, amount*sizeof(T));
        reserved = amount;
    }

    void resize(u32 amount) {
        auto old_amount = count;
        reserve(amount);
        count = amount;

        for (; old_amount < count; ++old_amount) {
            new (data + old_amount) T();
        }
    }

    void add(T element) {
        if (count+1 >= reserved) reserve(reserved + DEFAULT_ARRAY_RESERVE_SIZE);

        data[count] = element;
        count += 1;
    }

    void reset() {
        free(data);
        data     = nullptr;
        count    = 0;
        reserved = 0;
    }

    T &operator[](size_t index) {
        assert(index <= U32_MAX);
        assert(index < count);
        return data[index];
    }

    T *begin() {
        return &data[0];
    }

    T *end() {
        if (count) return &data[count];
        return nullptr;
    }
};

template <typename T>
struct Array_Slice {
    bool list_initialized = false;
    T *data;
    size_t count;

    Array_Slice() {
        list_initialized = false;
        data = nullptr;
        count = 0;
    }

    Array_Slice(const Array_Slice<T> &other) {
        list_initialized = false;
        data = other.data;
        count = other.count;
    }

    Array_Slice(const Array<T> &a) {
        list_initialized = false;
        data  = a.data;
        count = a.count;
    }

    Array_Slice(std::initializer_list<T> list) {
        count = list.size();
        data = (T *)malloc(sizeof(T) * count);

        memcpy(data, list.begin(), sizeof(T) * count);

        list_initialized = true;
    }

    ~Array_Slice() {
        if (list_initialized && data) free(data);

        data  = nullptr;
        count = 0;
    }

    T *begin() const {
        return &data[0];
    }

    T *end() const {
        if (count) return &data[count];
        return nullptr;
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

        chunks.add(c);
        return &chunks[chunks.count-1];
    }

    void append(const void *src, size_t size) {
        Chunk *c = &chunks[chunks.count-1];
        if (c->count+size >= c->reserved) c = new_chunk(size);

        memcpy(c->data+c->count, src, size);
        c->count += size;
    }

    void append_string(const String& s) {
        append(s.data(), (u32)s.length());
    }

    void *allocate_bytes_unaligned(size_t size) {
        Chunk *c = &chunks[chunks.count-1];
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

        Chunk *c = &chunks[chunks.count-1];
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