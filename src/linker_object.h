
#ifndef LINKER_OBJECT_H
#define LINKER_OBJECT_H

#include "general.h"

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
    u32 section_number;
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
    u32 section_number  = 0;
    u32 section_offset  = 0;
    bool is_externally_defined  = false;
    bool is_externally_visible  = true;
    bool is_function = false;
    bool is_section  = false;
};

enum Cpu_Arch {
    CPU_UNDEFINED = 0,
    CPU_X86_64    = 1,
    CPU_AAarch64  = 2,
};

struct Linker_Object {
    Cpu_Arch cpuarch;
    bool use_absolute_addressing = false;
    Array<Section> sections;
    Array<String>  string_table;
    Array<Symbol>  symbol_table;
};

#endif
