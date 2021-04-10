
#ifndef X64_EMITTER
#define X64_EMITTER

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

    josh::String_Table<String_Entry> string_table;

    Section *data_section;
    Section *code_section;

    // For code gen
    Array<Register> register_usage;
    Array<Register> xmm_usage;

    // @Temporary this information is the same across all functions,
    // we should put these in Target or something...
    Array<u8>       parameter_registers;


    Array<s32 *>    stack_size_fixups;
    s32 stack_size = 0;
    s32 largest_call_stack_adjustment = 0;
};

void x64_emit_function(X64_Emitter *emitter, Linker_Object *object, Function *function);

} // namespace josh

#endif