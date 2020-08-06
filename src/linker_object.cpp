#include "linker_object.h"
#include "ir.h"

u32 get_symbol_index(Linker_Object *object, Global_Value *value) {
    if (value->symbol_index == 0) {
        value->symbol_index = object->symbol_table.count;
        Symbol s;
        s.linkage_name = value->name;
        object->symbol_table.add(s);
    }

    return value->symbol_index;
}

void emit_function(Linker_Object *object, Section *code_section, Section *data_section, Function *function);

void generate_linker_object(Compilation_Unit *unit, Linker_Object *object, u32 *text_index, u32 *data_index) {
    u32 data_sec_index = 0;
    u32 text_sec_index = 0;

    {
        Section data_sec = {};

        if (object->target.is_macOS()) {
            data_sec.name    = "__data";
            data_sec.segment = "__DATA";
        } else {
            data_sec.name = ".data";
        }
        data_sec.section_number = object->sections.count+1;
        data_sec.symbol_index = object->symbol_table.count;
        data_sec.is_writable = true;

        Symbol sym;
        sym.linkage_name = "__data";
        sym.section_number = data_sec.section_number;
        sym.section_offset = 0;
        sym.is_externally_defined = false;
        sym.is_function = false;
        sym.is_section  = true;

        object->symbol_table.add(sym);

        data_sec_index = data_sec.section_number-1;

        object->sections.add(data_sec);
    }

    {
        Section text_sec = {};

        if (object->target.is_macOS()) {
            text_sec.name    = "__text";
            text_sec.segment = "__TEXT";
        } else {
            text_sec.name = ".text";
        }
        text_sec.section_number = object->sections.count+1;
        text_sec.symbol_index = object->symbol_table.count;
        text_sec.is_pure_instructions = true;

        Symbol sym;
        sym.linkage_name = "__text";
        sym.section_number = text_sec.section_number;
        sym.section_offset = 0;
        sym.is_externally_defined = false;
        sym.is_function = false;
        sym.is_section  = true;

        object->symbol_table.add(sym);

        text_sec_index = text_sec.section_number-1;

        object->sections.add(text_sec);
    }

    for (auto func : unit->functions) {
        emit_function(object, &object->sections[text_sec_index], &object->sections[data_sec_index], func);
    }

    if (text_index) *text_index = text_sec_index;
    if (data_index) *data_index = data_sec_index;
}

void emit_obj_file(Compilation_Unit *unit) {
    Linker_Object object = {};
    object.target = unit->target;

    generate_linker_object(unit, &object, nullptr, nullptr);

    void emit_macho_file(Linker_Object *object);
    void emit_coff_file(Linker_Object *object);

    if (object.target.is_win32()) {
        emit_coff_file(&object);
    } else if (object.target.is_macOS()) {
        emit_macho_file(&object);
    }
}

#ifndef _WIN32
#include <sys/mman.h>
#include <dlfcn.h>

void do_jit_and_run_program_main(Compilation_Unit *unit) {
    // @Cutnpaste from emit_obj_file
    Linker_Object object = {};
    object.target = unit->target;
    object.use_absolute_addressing = true;

    u32 data_sec_index = 0;
    u32 text_sec_index = 0;

    generate_linker_object(unit, &object, &text_sec_index, &data_sec_index);

    auto code_section = &object.sections[text_sec_index];
    auto data_section = &object.sections[data_sec_index];

    char *text_memory = (char *)mmap(nullptr, code_section->data.size(), PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0);
    char *data_memory = (char *)malloc(data_section->data.size());

    int written = 0;
    for (auto &c : code_section->data.chunks) {
        memcpy(text_memory + written, c.data, c.count);
        written += c.count;
    }

    written = 0;
    for (auto &c : data_section->data.chunks) {
        memcpy(data_memory + written, c.data, c.count);
        written += c.count;
    }

    Array<char *> section_memory;
    section_memory.add(data_memory);
    section_memory.add(text_memory);

    void *process_handle = dlopen(nullptr, RTLD_LAZY);

    for (auto reloc : code_section->relocations) {
        bool rip = reloc.is_for_rip_call || reloc.is_rip_relative;

        if (rip) assert(reloc.size == 4);
        else     assert(reloc.size == 8);

        auto target = text_memory + reloc.offset;
        auto symbol = &object.symbol_table[reloc.symbol_index];

        if (symbol->is_externally_defined) {
            auto symbol_target = (char *)dlsym(process_handle, symbol->linkage_name.data);
            assert(symbol_target);

            if   (rip) *(u32 *)target = (symbol_target - target);
            else       *(u64 *)target = (u64) symbol_target;
        } else {
            auto symbol_sec    = section_memory[symbol->section_number-1];
            auto symbol_target = symbol_sec;

            if (rip) *(u32 *)target += (symbol_target - target);
            else     *(u64 *)target += (u64) symbol_target;
        }
    }

    Symbol *main_symbol = nullptr;
    for (auto &sym : object.symbol_table) {
        if (strcmp(sym.linkage_name.data, "main") == 0) {
            main_symbol = &sym;
        }
    }

    assert(main_symbol);

    void (*prog_main)() = (void (*)())(text_memory + main_symbol->section_offset);
    prog_main();
}
#endif
