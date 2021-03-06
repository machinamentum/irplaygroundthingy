
#ifndef LINKER_OBJECT_H
#define LINKER_OBJECT_H

#include "general.h"

namespace josh {

struct Global_Value;
struct Compilation_Unit;
struct IR_Context;

struct Target {

    enum Cpu_Arch : u8 {
        CPU_UNDEFINED = 0,
        CPU_X86_64    = 1,
        CPU_AAarch64  = 2,
    };

    enum OS : u8 {
        WINDOWS,
        MACOSX,
        LINUX,
        C_VIRTUAL_OS,
    };

    OS os;
    Cpu_Arch cpuarch;

    bool is_win32() const { return os == WINDOWS; }
    bool is_macOS() const { return os == MACOSX;  }
    bool is_system_v() const { return os == MACOSX || os == LINUX; }
    bool is_c() const { return os == C_VIRTUAL_OS; }

    bool is_x64() const { return cpuarch == CPU_X86_64; }
    bool is_aarch64() const { return cpuarch == CPU_AAarch64; }
};

inline
Target get_host_target() {
    Target target;

#ifdef _WIN32
    target.os      = Target::WINDOWS;
#elif __APPLE__
    target.os      = Target::MACOSX;
#else
    target.os      = Target::LINUX;
#endif

#ifdef __aarch64__
    target.cpuarch = Target::CPU_AAarch64;
#else
    target.cpuarch = Target::CPU_X86_64;
#endif

    return target;
}

struct Relocation {
    enum Type : u8 {
        RIP_DATA = 0,
        RIP_CALL,
        ABSOLUTE,
        PAGEOFFSET,
    } type = RIP_DATA;

    u8 size   = 0;
    u32 symbol_index = 0;
    u32 offset = 0;
    u32 addend = 0;
};

struct Section {
    String name;
    String segment;
    Data_Buffer data;
    u8 section_number = 0;
    u32 symbol_index = 0;

    Array<Relocation> relocations;
    bool is_pure_instructions = false;
    bool is_writable = false;

    // format specific
    void *mach_section = nullptr;
};

struct Symbol {
    String_ID linkage_name;
    u8 section_number  = 0;
    u32 section_offset  = 0;
    bool is_externally_defined  = false;
    bool is_externally_visible  = true;
    bool is_function = false;
    bool is_section  = false;
};

struct Linker_Object {
    Target target;
    bool use_absolute_addressing = false;
    Array<Section> sections;
    Array<String>  string_table;
    Array<Symbol>  symbol_table;
};

u32 get_symbol_index(Linker_Object *object, Global_Value *value);

void generate_linker_object(IR_Context *context, Compilation_Unit *unit, Linker_Object *object, u32 *text_index, u32 *data_index);

void emit_obj_file(IR_Context *context, Compilation_Unit *unit);


typedef void *DLL_Handle;
DLL_Handle dll_open(const char *path);
void *dll_find_symbol(DLL_Handle handle, const char *name);

typedef void *(JIT_Lookup_Symbol_Callback)(Compilation_Unit *unit, const char *symbol_name);

void  jit_generate_code(IR_Context *context, Compilation_Unit *unit, JIT_Lookup_Symbol_Callback cb = nullptr);
void *jit_lookup_address(IR_Context *context, const String &symbol_name);

void do_jit_and_run_program_main(IR_Context *context, Compilation_Unit *unit, JIT_Lookup_Symbol_Callback cb = nullptr);

} // namespace josh

#endif
