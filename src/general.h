
#ifndef GENERAL_H
#define GENERAL_H


#include <stdint.h>
#include <string.h> // strlen
#include <assert.h>

#include <new>

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


struct String {
    char  *data;
    size_t length;

    String(const char *s = nullptr) {
        data = const_cast<char *>(s);
        length = data ? strlen(data) : 0;
    }
};

struct Data_Buffer {
    struct Chunk {
        u8 *data      = nullptr;
        u32 count     = 0;
        u32 reserved  = 0;
    };

    Array<Chunk> chunks;

    Data_Buffer() {
        new_chunk();
    }

    Chunk *new_chunk(u32 size = 4096) {
        if (size < 4096) size = 4096;
        Chunk c;
        c.data     = (u8 *)malloc(size);
        c.count    = 0;
        c.reserved = size;

        chunks.add(c);
        return &chunks[chunks.count-1];
    }

    void append(void *src, u32 size) {
        Chunk *c = &chunks[chunks.count-1];
        if (c->count+size >= c->reserved) c = new_chunk(size);

        memcpy(c->data+c->count, src, size);
        c->count += size;
    }

    void *allocate(u32 size) {
        Chunk *c = &chunks[chunks.count-1];
        if (c->count+size >= c->reserved) c = new_chunk(size);

        void *result = c->data+c->count;
        c->count += size;
        return result;
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

#endif