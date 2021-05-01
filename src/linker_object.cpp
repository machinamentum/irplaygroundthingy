#include "linker_object.h"
#include "ir.h"
#include "x64.h"
#include "aarch64.h"
#include "ir.h"

using namespace josh;

#include <stdio.h>

#ifdef __APPLE__
#include <libkern/OSCacheControl.h>
#include <pthread.h>
#endif

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

void *josh::dll_find_symbol(DLL_Handle handle, const char *name) {
    return GetProcAddress(handle, name);
}

#endif

namespace josh {

u32 get_symbol_index(Linker_Object *object, Global_Value *value) {
    if (value->symbol_index == 0) {
        value->symbol_index = static_cast<u32>(object->symbol_table.size());
        Symbol s;
        s.linkage_name = value->name;
        object->symbol_table.push_back(s);

        assert(object->symbol_table.size() <= U32_MAX);
    }

    return value->symbol_index;
}

void generate_linker_object(IR_Context *context, Compilation_Unit *unit, Linker_Object *object, u32 *text_index, u32 *data_index) {
    u32 data_sec_index = 0;
    u32 text_sec_index = 0;

    {
        object->sections.resize(object->sections.size()+1);
        Section &data_sec = object->sections.back();

        if (object->target.is_macOS()) {
            data_sec.name    = "__data";
            data_sec.segment = "__DATA";
        } else {
            data_sec.name = ".data";
        }
        data_sec.section_number = static_cast<u8>(object->sections.size());
        data_sec.symbol_index = static_cast<u32>(object->symbol_table.size());
        data_sec.is_writable = true;

        Symbol sym;
        sym.linkage_name = context->intern("__data");
        sym.section_number = data_sec.section_number;
        sym.section_offset = 0;
        sym.is_externally_defined = false;
        sym.is_function = false;
        sym.is_section  = true;

        object->symbol_table.push_back(sym);

        data_sec_index = data_sec.section_number-1;
    }

    {
        object->sections.resize(object->sections.size()+1);
        Section &text_sec = object->sections.back();

        if (object->target.is_macOS()) {
            text_sec.name    = "__text";
            text_sec.segment = "__TEXT";
        } else {
            text_sec.name = ".text";
        }
        text_sec.section_number = static_cast<u8>(object->sections.size());
        text_sec.symbol_index = static_cast<u32>(object->symbol_table.size());
        text_sec.is_pure_instructions = true;

        Symbol sym;
        sym.linkage_name = context->intern("__text");
        sym.section_number = text_sec.section_number;
        sym.section_offset = 0;
        sym.is_externally_defined = false;
        sym.is_function = false;
        sym.is_section  = true;

        object->symbol_table.push_back(sym);

        text_sec_index = text_sec.section_number-1;
    }

    // Always start data section with a null byte to provide a consistent location for empty strings.
    object->sections[data_sec_index].data.append_byte(0);

    if (object->target.is_x64()) {
        X64_Emitter emitter;
        emitter.code_section = &object->sections[text_sec_index];
        emitter.data_section = &object->sections[data_sec_index];

        for (auto func : unit->functions)
            x64_emit_function(&emitter, object, func);

        for (const auto &[target, func] : emitter.rip_call_fixup_targets) {
            s32 *addr = emitter.code_section->data.get_pointer_from_offset<s32>(target);
            *addr = static_cast<s32>(emitter.function_text_locations[func] - target - 4);
        }

        u32 text_symbol_index = object->sections[text_sec_index].symbol_index;
        for (const auto &[target, func] : emitter.absolute_call_fixup_targets) {
            size_t func_offset = emitter.function_text_locations[func];

            // Absolute address use still needs to use a relocation because the .text may be moved to anywhere
            Relocation reloc;
            reloc.is_for_rip_call = false;
            reloc.offset = trunc<u32>(target);
            reloc.symbol_index = text_symbol_index;
            reloc.size = 8;
            reloc.addend = 0; // @TODO

            u64 *addr = emitter.code_section->data.get_pointer_from_offset<u64>(target);
            *addr = func_offset;

            object->sections[text_sec_index].relocations.push_back(reloc);
        }
    } else if (object->target.is_aarch64()) {
        AArch64_Emitter emitter;
        emitter.code_section = &object->sections[text_sec_index];
        emitter.data_section = &object->sections[data_sec_index];

        for (auto func : unit->functions)
            AArch64_emit_function(&emitter, object, func);

        for (const auto &[target, func] : emitter.rip_call_fixup_targets) {
            s32 *addr = emitter.code_section->data.get_pointer_from_offset<s32>(target);
            *addr = static_cast<s32>(emitter.function_text_locations[func] - target - 4);
        }

        u32 text_symbol_index = object->sections[text_sec_index].symbol_index;
        for (const auto &[target, func] : emitter.absolute_call_fixup_targets) {
            size_t func_offset = emitter.function_text_locations[func];

            // Absolute address use still needs to use a relocation because the .text may be moved to anywhere
            Relocation reloc;
            reloc.is_for_rip_call = false;
            reloc.offset = trunc<u32>(target);
            reloc.symbol_index = text_symbol_index;
            reloc.size = 8;
            reloc.addend = 0; // @TODO

            u64 *addr = emitter.code_section->data.get_pointer_from_offset<u64>(target);
            *addr = func_offset;

            object->sections[text_sec_index].relocations.push_back(reloc);
        }
    }

    assert(object->symbol_table.size() <= U32_MAX);
    assert(object->sections.size()     <= U8_MAX);

    if (text_index) *text_index = text_sec_index;
    if (data_index) *data_index = data_sec_index;

    printf("Number of .text bytes: %d\n", object->sections[text_sec_index].data.size());
    printf("Number of .data bytes: %d\n", object->sections[data_sec_index].data.size());
}

void emit_obj_file(IR_Context *context, Compilation_Unit *unit) {
    Linker_Object object = {};
    object.target = unit->target;

    generate_linker_object(context, unit, &object, nullptr, nullptr);

    void emit_macho_file(IR_Context *context, Linker_Object *object);
    void emit_coff_file(IR_Context *context, Linker_Object *object);

    if (object.target.is_win32()) {
        emit_coff_file(context, &object);
    } else if (object.target.is_macOS()) {
        emit_macho_file(context, &object);
    }
}

void jit_generate_code(IR_Context *context, Compilation_Unit *unit, JIT_Lookup_Symbol_Callback lookup_sym) {
    assert(sizeof(void *) == 8); // 32-bit mode unsupported right now @TODO

    // @Cutnpaste from emit_obj_file
    Linker_Object object = {};
    object.target = unit->target;
    object.use_absolute_addressing = true;

    u32 data_sec_index = 0;
    u32 text_sec_index = 0;

    generate_linker_object(context, unit, &object, &text_sec_index, &data_sec_index);

    auto code_section = &object.sections[text_sec_index];
    auto data_section = &object.sections[data_sec_index];

    Array<DLL_Handle> dlls_to_search;

    dlls_to_search.push_back(dll_open(nullptr));

#ifndef _WIN32
    char *text_memory = (char *)mmap(nullptr, code_section->data.size(), PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0);

#ifdef __APPLE__
    pthread_jit_write_protect_np(false);
#endif
#else
    char *text_memory = (char *)VirtualAlloc(nullptr, code_section->data.size(), MEM_COMMIT, PAGE_READWRITE);
    {
        DWORD old_prot;
        VirtualProtect(text_memory, code_section->data.size(), PAGE_EXECUTE_READWRITE, &old_prot);

        dlls_to_search.push_back(dll_open("msvcrt.dll"));
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
    section_memory.push_back(data_memory);
    section_memory.push_back(text_memory);

    Map<String_ID, char *> symbol_map;

    bool error = false;

// stolen from aarch64.cpp
#define DATA_IMM_ADR_IMMHI(imm) ((((imm) >> 2) & 0x7FFFF) << 5) // 19 bits >.>
#define DATA_IMM_ADR_IMMLO(imm) (((imm) & 0b11) << 29)

    const auto fixup_address = [&object](void *target, void *symbol_target, const Relocation &reloc) {
        bool rip = reloc.is_for_rip_call || reloc.is_rip_relative;

        if (object.target.is_x64()) {
            intptr_t rip_value = (intptr_t)((intptr_t)symbol_target - (intptr_t)target);
            assert(!rip);
            if (rip) *(s32 *) target = rip_value + reloc.addend;
            else     *(u64 *) target = ((u64)symbol_target) + reloc.addend;
        } else if (object.target.is_aarch64()) {
            if      (reloc.is_for_rip_call) {
                intptr_t rip_value = (intptr_t)((intptr_t)symbol_target - (intptr_t)target);
                assert(fits_into_bits((rip_value / 4), 26));
                *(u32 *) target = (*(u32 *)target) | ((rip_value / 4) & 0x3FFFFFF); // assume writing into bl imm26 field
            }
            else if (reloc.is_rip_relative) {
                intptr_t rip_value = (intptr_t(symbol_target) / 4096) - (intptr_t(target) / 4096);
                assert(fits_into_bits(rip_value, 21));
                *(u32 *) target = (*(u32 *)target) | DATA_IMM_ADR_IMMHI(rip_value) | DATA_IMM_ADR_IMMLO(rip_value); // assume writing into adrp
            }
            else if (reloc.is_for_page_offset) {
                u32 value = intptr_t(symbol_target) % 4096;

                u32 v = ((*(u32 *)target) >> 10) & 0x0FFF;
                u32 i = ((*(u32 *)target) & ~(0x0FFF << 10)); // clear imm12 bits
                assert(fits_into_bits_unsigned(v + value + reloc.addend, 12));
                *(u32 *) target = i | ((v + value + reloc.addend) << 10); //assume writing into add
            }
            else {
                assert(false);
            }
        }
    };

    for (size_t i = 0; i < code_section->relocations.size(); ++i) {
        const Relocation &reloc = code_section->relocations[i];

        bool rip = reloc.is_for_rip_call || reloc.is_rip_relative;
        bool is_page_offset = reloc.is_for_page_offset;

        if (object.target.is_x64()) {
            if (rip) assert(reloc.size == 4);
            else     assert(reloc.size == 8);
        }

        // RIP addressing doesnt work here because we cannot gaurantee that data symbols are
        // within 2gbs of the target, or within 128MB on ARM. @FixMe we could gaurantee this for RIP-relative calls
        // into code within the same .text section.
        if (object.target.is_x64())
            assert(!rip);

        auto target = text_memory + reloc.offset;
        auto symbol = &object.symbol_table[reloc.symbol_index];
        char *symbol_target = nullptr;

        if (symbol->is_externally_defined) {
            if (!symbol_map.count(symbol->linkage_name))
            {
                String linkage_name = context->get_string(symbol->linkage_name);

                if (lookup_sym) {
                    symbol_target = (char *)lookup_sym(unit, linkage_name.data());
                }

                if (!symbol_target) {
                    for (auto handle : dlls_to_search) {
                        symbol_target = (char *)dll_find_symbol(handle, linkage_name.data());
                        if (symbol_target) break;
                    }
                }

                if (!symbol_target) {
                    printf("Could not find externally-defined symbol '%s'\n", linkage_name.data());
                    error = true;
                    continue;
                }

                symbol_map[symbol->linkage_name] = symbol_target;
            } else {
                symbol_target = symbol_map[symbol->linkage_name];
            }
        } else {
            auto symbol_sec = section_memory[symbol->section_number-1];
            symbol_target   = symbol_sec + symbol->section_offset;
        }
        assert(symbol_target);
        fixup_address(target, symbol_target, reloc);
    }

#ifdef __APPLE__
    pthread_jit_write_protect_np(true);
    sys_icache_invalidate(text_memory, code_section->data.size());
#endif

    for (auto &symbol : object.symbol_table) {
        if (symbol.is_externally_defined)
            continue;

        auto symbol_sec    = section_memory[symbol.section_number-1];
        auto symbol_target = symbol_sec + symbol.section_offset;

        context->jit_symbols[symbol.linkage_name] = {symbol_target};
    }
}

void *jit_lookup_address(IR_Context *context, const String &symbol_name) {
    String_ID id = context->intern(symbol_name);
    if (!id)
        return nullptr;

    return context->jit_symbols[id].entry_ptr;
}

void do_jit_and_run_program_main(IR_Context *context, Compilation_Unit *unit, JIT_Lookup_Symbol_Callback lookup_sym) {
    jit_generate_code(context, unit, lookup_sym);

    void (*prog_main)() = (void (*)())(jit_lookup_address(context, "main"));
    prog_main();
}

} // namespace josh
