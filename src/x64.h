
#ifndef JOSH_X64_EMITTER
#define JOSH_X64_EMITTER

#include "general.h"

namespace josh {

struct X64_Emitter
{
    struct String_Entry {
        const char *name = nullptr;
        u32 data_sec_offset = U32_MAX;

        bool operator== (const String &s) {
            return name == s;
        }
    };

    // Whole program data
    String_Table<String_Entry> string_table;
    Array<Pair<size_t, Function *>> absolute_call_fixup_targets;
    Array<Pair<size_t, Function *>> rip_call_fixup_targets;
    Map<Function *, size_t> function_text_locations;

    // Per function data below

    bool emitting_last_block = false;

    Section *data_section;
    Section *code_section;
    Data_Buffer function_buffer;
    Data_Buffer *codeptr;

    // For code gen
    Array<Register> register_usage;
    Array<Register> xmm_usage;

    // @Temporary this information is the same across all functions,
    // we should put these in Target or something...
    Array<u8>       parameter_registers;

    s32 stack_size = 0;
    s32 largest_call_stack_adjustment = 0;

    Array<size_t> epilogue_jump_target_fixups;
    Array<s32 *>  epilogue_jump_target_fixup_pointers;
};

void x64_emit_function(X64_Emitter *emitter, Linker_Object *object, Function *function);

} // namespace josh

#endif