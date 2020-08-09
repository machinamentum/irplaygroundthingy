
#ifndef LINKER_OBJECT_H
#define LINKER_OBJECT_H

#include "general.h"

struct Global_Value;
struct Compilation_Unit;

struct Target {

    enum Cpu_Arch {
        CPU_UNDEFINED = 0,
        CPU_X86_64    = 1,
        CPU_AAarch64  = 2,
    };

    enum {
        WINDOWS,
        MACOSX,
        LINUX,
    };

    u8 os;
    u8 cpuarch;

    bool is_win32() { return os == WINDOWS; }
    bool is_macOS() { return os == MACOSX;  }
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

    target.cpuarch = Target::CPU_X86_64;

    return target;
}

struct Relocation {
    bool is_for_rip_call = false;
    bool is_rip_relative = false;
    u32 size   = 0;
    u32 symbol_index = 0;
    u32 offset = 0;
    u64 addend = 0;
};

struct Section {
    String name;
    String segment;
    Data_Buffer data;
    u8 section_number;
    u32 symbol_index = 0;

    Array<Relocation> relocations;
    bool is_pure_instructions = false;
    bool is_writable = false;

    // format specific
    void *mach_section = nullptr;

    Section() { }
};

struct Symbol {
    String linkage_name;
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

void generate_linker_object(Compilation_Unit *unit, Linker_Object *object, u32 *text_index, u32 *data_index);

void emit_obj_file(Compilation_Unit *unit);

void do_jit_and_run_program_main(Compilation_Unit *unit);

#endif
