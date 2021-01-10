#include "linker_object.h"
#include "ir.h"

using namespace josh;

#include <stdio.h>

#ifndef _WIN32
#include <sys/mman.h>
#include <dlfcn.h>

DLL_Handle josh::dll_open(const char *path) {
    return dlopen(path, RTLD_LAZY);
}

void *josh::dll_find_symbol(DLL_Handle handle, const char *name) {
    return dlsym(handle, name);
}

#else
#include <Windows.h>
#include <Memoryapi.h>

DLL_Handle josh::dll_open(const char *path) {
    if (path == nullptr) return GetModuleHandle(NULL);

    return LoadLibraryA(path);
}

void *dll_find_symbol(DLL_Handle handle, const char *name) {
    return GetProcAddress(handle, name);
}

#endif

namespace josh {

u32 get_symbol_index(Linker_Object *object, Global_Value *value) {
    if (value->symbol_index == 0) {
        value->symbol_index = static_cast<u32>(object->symbol_table.count);
        Symbol s;
        s.linkage_name = value->name;
        object->symbol_table.add(s);

        assert(object->symbol_table.count <= U32_MAX);
    }

    return value->symbol_index;
}

void x64_emit_function(Linker_Object *object, Section *code_section, Section *data_section, Function *function);
// void AArch64_emit_function(Linker_Object *object, Section *code_section, Section *data_section, Function *function);

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
        data_sec.section_number = static_cast<u8>(object->sections.count+1);
        data_sec.symbol_index = static_cast<u32>(object->symbol_table.count);
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
        text_sec.section_number = static_cast<u8>(object->sections.count+1);
        text_sec.symbol_index = static_cast<u32>(object->symbol_table.count);
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
        if (object->target.is_x64())
            x64_emit_function(object, &object->sections[text_sec_index], &object->sections[data_sec_index], func);
        // else if (object->target.is_aarch64())
        //     AArch64_emit_function(object, &object->sections[text_sec_index], &object->sections[data_sec_index], func);
    }

    assert(object->symbol_table.count <= U32_MAX);
    assert(object->sections.count     <= U8_MAX);

    if (text_index) *text_index = text_sec_index;
    if (data_index) *data_index = data_sec_index;

    printf("Number of .text bytes: %d\n", object->sections[text_sec_index].data.size());
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

void do_jit_and_run_program_main(Compilation_Unit *unit, JIT_Lookup_Symbol_Callback lookup_sym) {
    assert(sizeof(void *) == 8); // 32-bit mode unsupported right now @TODO

    // @Cutnpaste from emit_obj_file
    Linker_Object object = {};
    object.target = unit->target;
    object.use_absolute_addressing = true;

    u32 data_sec_index = 0;
    u32 text_sec_index = 0;

    generate_linker_object(unit, &object, &text_sec_index, &data_sec_index);

    auto code_section = &object.sections[text_sec_index];
    auto data_section = &object.sections[data_sec_index];

    Array<DLL_Handle> dlls_to_search;

    dlls_to_search.add(dll_open(nullptr));

#ifndef _WIN32
    char *text_memory = (char *)mmap(nullptr, code_section->data.size(), PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0);
#else
    char *text_memory = (char *)VirtualAlloc(nullptr, code_section->data.size(), MEM_COMMIT, PAGE_READWRITE);
    {
        DWORD old_prot;
        VirtualProtect(text_memory, code_section->data.size(), PAGE_EXECUTE_READWRITE, &old_prot);

        dlls_to_search.add(dll_open("msvcrt.dll"));
    }
#endif
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

    bool error = false;

    for (auto reloc : code_section->relocations) {
        bool rip = reloc.is_for_rip_call || reloc.is_rip_relative;

        if (rip) assert(reloc.size == 4);
        else     assert(reloc.size == 8);

        // RIP addressing doesnt work here because we cannot gaurantee that data symbols are
        // within 2gbs of the target. @FixMe we could gaurantee this for RIP-relative calls
        // into code within the same .text section.
        assert(!rip);

        auto target = text_memory + reloc.offset;
        auto symbol = &object.symbol_table[reloc.symbol_index];

        if (symbol->is_externally_defined) {
            char *symbol_target = nullptr;

            if (lookup_sym) {
                symbol_target = (char *)lookup_sym(unit, symbol->linkage_name.data);
            }

            if (!symbol_target) {
                for (auto handle : dlls_to_search) {
                    symbol_target = (char *)dll_find_symbol(handle, symbol->linkage_name.data);
                    if (symbol_target) break;
                }
            }

            if (!symbol_target) {
                printf("Could not find externally-defined symbol '%s'\n", symbol->linkage_name.data);
                error = true;
                continue;
            }

            if   (rip) *(u32 *)target = (u32)(symbol_target - target);
            else       *(u64 *)target = (u64) symbol_target;
        } else {
            auto symbol_sec    = section_memory[symbol->section_number-1];
            auto symbol_target = symbol_sec + symbol->section_offset;

            if (rip) *(u32 *)target += (u32)(symbol_target - target);
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

    if (error) return;

    void (*prog_main)() = (void (*)())(text_memory + main_symbol->section_offset);
    prog_main();
}

} // namespace josh
