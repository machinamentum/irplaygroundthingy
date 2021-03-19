
#ifndef GENERAL_H
#define GENERAL_H


#include <stdint.h>
#include <string.h> // strlen
#include <assert.h>

#include <new>
#include <initializer_list>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <utility>

#include <stdio.h>

namespace josh {

using String = std::string_view;

template <typename T>
using Array = std::vector<T>;

template <typename K, typename V>
using Map = std::unordered_map<K, V>;

template <typename A, typename B>
using Pair = std::pair<A, B>;

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

template <typename T, typename B>
bool fits_into(B b) {
    T a = (T)b;
    return ((B)a) == b;
}

template<typename T, typename B>
T trunc(B value) {
    assert((fits_into<T, B>(value)));
    return static_cast<T>(value);
}

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

template <size_t DEFAULT_CHUNK_SIZE = 8192>
struct Bump_Allocator {
    struct Chunk {
        char *data       = nullptr;
        size_t count     = 0;
        size_t reserved  = 0;
    };

    ~Bump_Allocator() {
        for (auto &c : chunks)
            free(c.data);

        chunks.clear();
    }

    Array<Chunk> chunks;

    Chunk *new_chunk(size_t size = DEFAULT_CHUNK_SIZE) {
        if (size < DEFAULT_CHUNK_SIZE) size = DEFAULT_CHUNK_SIZE;

        chunks.resize(chunks.size()+1);

        Chunk *c = &chunks.back();

        c->data     = (char *)malloc(size);
        c->count    = 0;
        c->reserved = size;

        return c;
    }

    Chunk *get_chunk()
    {
        if (chunks.size() == 0)
            return new_chunk();

        return &chunks[chunks.size()-1];
    }

    char *append(const void *src, size_t size) {
        Chunk *c = get_chunk();
        if (c->count+size >= c->reserved) c = new_chunk(size);

        char *out = c->data+c->count;
        memcpy(out, src, size);
        c->count += size;
        return out;
    }

    char *append_string(const String& s) {
        return append(s.data(), (u32)s.length());
    }

    void *allocate_bytes_unaligned(size_t size) {
        Chunk *c = get_chunk();
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

        Chunk *c = get_chunk();
        auto count = ensure_aligned(c->count, align);
        if (count+sizeof(T) >= c->reserved) c = new_chunk(sizeof(T));

        c->count = ensure_aligned(c->count, align);
        void *result = c->data+c->count;
        c->count += sizeof(T);


        return new (result) T();
    }

    void append(Bump_Allocator<DEFAULT_CHUNK_SIZE> *other) {
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

using Data_Buffer = Bump_Allocator<1024 * 1024>; // 1MB

typedef u64 String_ID;
const static String_ID STRING_ID_EMPTY = 0;

struct String_Table
{
    Bump_Allocator<> string_storage;

    struct String_Entry
    {
        const char *s = nullptr;
        size_t data_sec_offset = 0;
    };

    struct Bucket
    {
        Array<u64>    keys;
        Array<String_Entry> values;
    };

    Bucket *buckets;
    size_t _size = 0;

    static const size_t NUM_BUCKETS = 0x1000;
    static const size_t DEFAULT_BUCKET_SIZE = 0x1000;

    String_Table()
    {
        buckets = reinterpret_cast<Bucket *>(malloc(sizeof(Bucket) * NUM_BUCKETS));

        for (size_t i = 0; i < NUM_BUCKETS; ++i)
            new (buckets + i) Bucket();
    }

    static u32 hash(const String &s)
    {
        const char *data = s.data();
        u32 v = 5381;
        for (size_t i = 0; i < s.length(); ++i)
        {
            v = v * 33 ^ static_cast<u32>(data[i]);
        }
        return v;
    }

    String_Entry *intern(const String &str)
    {
        if (str.length() == 0)
            return nullptr;

        u64 key = hash(str);
        u64 bucket = key & (NUM_BUCKETS-1);
        Bucket &b = buckets[bucket];

        // TODO I think this could likely be faster by using a dense-hash-set/grouping comparison strategy using SIMD
        for (size_t i = 0; i < b.keys.size(); ++i)
        {
            u64 k = b.keys[i];
            if (k == key && b.values[i].s == str)
                return &b.values[i];
        }

        b.keys.push_back(key);
        b.values.push_back(String_Entry{string_storage.append_string(str), U32_MAX});
        _size += 1;
        return &b.values[b.values.size()-1];
    }
};

} // namespace josh

#endif