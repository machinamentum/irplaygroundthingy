#include "general.h"
#include "linker_object.h"

#include <assert.h>

enum {
    VALUE_CONSTANT,

    INSTRUCTION_FIRST,
    
    INSTRUCTION_CALL = INSTRUCTION_FIRST,
    INSTRUCTION_RETURN,
    INSTRUCTION_ALLOCA,
    INSTRUCTION_STORE,
    INSTRUCTION_LOAD,
    INSTRUCTION_ADD,
    INSTRUCTION_SUB,

    INSTRUCTION_LAST,
};

struct Type {
    enum {
        INTEGER,
        FLOAT,
        POINTER,
        FUNCTION,
    };

    u32 type;
    u32 size;
    u32 alignment;

    union {
        Type *pointer_to;
    };
};

Type *make_integer_type(u32 size) {
    Type *type      = new Type();
    type->type      = Type::INTEGER;
    type->size      = size;
    type->alignment = type->size;
    return type;
}

Type *make_pointer_type(Type *pointee) {
    Type *type = new Type();
    type->type = Type::POINTER;
    type->size = 8; // @TargetInfo
    type->alignment = type->size;
    type->pointer_to = pointee;
    return type;
}

struct Value {
    u32 type;
    Type *value_type;
};

struct Constant : Value {
    Constant() { type = VALUE_CONSTANT; }

    enum {
        STRING,
        INTEGER
    };

    u32 constant_type;

    String string_value;
    u64 integer_value;
};

Constant *make_string_constant(String value) {
    Constant *con = new Constant();
    con->constant_type = Constant::STRING;
    con->string_value = value;
    con->value_type = make_pointer_type(make_integer_type(1));
    return con;
}

Constant *make_integer_constant(u64 value) {
    Constant *con = new Constant();
    con->constant_type = Constant::INTEGER;
    con->integer_value = value;
    con->value_type = make_integer_type(8);
    return con;
}

struct Function;
struct Instruction;

struct Register {
    u8 machine_reg;
    bool is_free = true;
    Instruction *currently_holding_result_of_instruction = nullptr;
};

struct Instruction : Value {
    Register *result_stored_in    = nullptr;
    u32 result_spilled_onto_stack = 0xFFFFFFFF;
};

struct Instruction_Call : Instruction {
    Instruction_Call() { type = INSTRUCTION_CALL; }

    Value *call_target;
    Array<Value *> parameters;
};

struct Instruction_Return : Instruction {
    Instruction_Return() { type = INSTRUCTION_RETURN; }

    Value *return_value;
};

struct Instruction_Alloca : Instruction {
    Instruction_Alloca() { type = INSTRUCTION_ALLOCA; }

    Type *alloca_type = nullptr;

    u64 stack_offset = 0;
};

struct Instruction_Store : Instruction {
    Instruction_Store() { type = INSTRUCTION_STORE; }

    Value *store_target = nullptr;
    Value *source_value = nullptr;
};

struct Instruction_Load : Instruction {
    Instruction_Load() { type = INSTRUCTION_LOAD; }

    Value *pointer_value = nullptr;
};

struct Instruction_Math_Binary_Op : Instruction {
    Value *lhs;
    Value *rhs;
};

struct Instruction_Add : Instruction_Math_Binary_Op {
    Instruction_Add() { type = INSTRUCTION_ADD; }
};

struct Instruction_Sub : Instruction_Math_Binary_Op {
    Instruction_Sub() { type = INSTRUCTION_SUB; }
};

Instruction_Add *make_add(Value *lhs, Value *rhs) {
    // @Incomplete assert that lhs and rhs types match
    Instruction_Add *add = new Instruction_Add();
    add->value_type = lhs->value_type;
    add->lhs = lhs;
    add->rhs = rhs;
    return add;
}

Instruction_Sub *make_sub(Value *lhs, Value *rhs) {
    // @Incomplete assert that lhs and rhs types match
    Instruction_Sub *sub = new Instruction_Sub();
    sub->value_type = lhs->value_type;
    sub->lhs = lhs;
    sub->rhs = rhs;
    return sub;
}

Instruction_Load *make_load(Value *pointer_value) {
    assert(pointer_value->value_type->type == Type::POINTER);
    Instruction_Load *load = new Instruction_Load();
    load->pointer_value = pointer_value;
    load->value_type    = pointer_value->value_type->pointer_to;
    return load;
}

Instruction_Store *make_store(Value *source_value, Value *store_target) {
    Instruction_Store *store = new Instruction_Store();
    store->source_value = source_value;
    store->store_target = store_target;
    return store;
}

Instruction_Alloca *make_alloca(Type *alloca_type) {
    Instruction_Alloca *_alloca = new Instruction_Alloca();
    _alloca->alloca_type = alloca_type;
    _alloca->value_type = make_pointer_type(alloca_type);
    return _alloca;
}

struct Basic_Block {
    Array<Instruction *> instructions;
};

struct Global_Value : Constant {
    String name;
    u64 symbol_index = 0;
};

const u8 RAX = 0;
const u8 RCX = 1;
const u8 RDX = 2;
const u8 RBX = 3;
const u8 RSP = 4;
const u8 RBP = 5;
const u8 RSI = 6;
const u8 RDI = 7;
const u8 R8  = 8;
const u8 R9  = 9;

void move_reg64_to_memory(Data_Buffer *dataptr, u8 src, u8 dst, u32 disp);

struct Function : Global_Value {
    Array<Type *> parameter_types;
    Array<Basic_Block *> blocks;

    // For code gen
    Array<Register> register_usage;

    Register *get_free_register() {
        for (auto &reg : register_usage) {
            if (reg.is_free) {
                reg.is_free = false;
                return &reg;
            }
        }

        return nullptr;
    }

    Register *claim_register(Data_Buffer *dataptr, u8 machine_reg, Instruction *claimer) {
        Register *reg = &register_usage[machine_reg];
        maybe_spill_register(dataptr, reg);

        if (claimer) {
            reg->currently_holding_result_of_instruction = claimer;
            claimer->result_stored_in = reg;
        }

        reg->is_free = false;
        return reg;
    }

    void free_register(Register *reg) {
        reg->currently_holding_result_of_instruction = nullptr;
        reg->is_free = true;
    }

    void maybe_spill_register(Data_Buffer *dataptr, Register *reg) {
        if (reg->currently_holding_result_of_instruction) {
            auto inst = reg->currently_holding_result_of_instruction;
            inst->result_stored_in = nullptr;
            inst->result_spilled_onto_stack = this->stack_size;
            this->stack_size += 8; // @TargetInfo
            reg->currently_holding_result_of_instruction = nullptr;

            move_reg64_to_memory(dataptr, reg->machine_reg, RBP, inst->result_spilled_onto_stack);
        }

        reg->is_free = true;
    }

    Array<u32 *>    stack_size_fixups;
    u32 stack_size = 0;
};

struct Compilation_Unit {
    Array<Function *> functions;
};

u32 get_symbol_index(Linker_Object *object, Global_Value *value) {
    if (value->symbol_index == 0) {
        value->symbol_index = object->symbol_table.count;
        Symbol s;
        s.linkage_name = value->name;
        object->symbol_table.add(s);
    }

    return value->symbol_index;
}

u8 get_next_system_v_abi_register(u8 *index) {
    u8 value = *index;

    (*index) += 1;

    switch (value) {
        case 0: return RDI;
        case 1: return RSI;
        case 2: return RDX;
        case 3: return RCX;
        case 4: return R8;
        case 5: return R9;
        default: return 0xFF;
    }
}

u8 get_next_win64_abi_register(u8 *index) {
    u8 value = *index;

    (*index) += 1;

    switch (value) {
        case 0: return RCX;
        case 1: return RDX;
        case 2: return R8;
        case 3: return R9;
        default: return 0xFF;
    }
}

// This magic is best explained on OSDev https://wiki.osdev.org/X86-64_Instruction_Encoding
// REX prefixes the instruction, ModRM seems to post-fix the instruction but before data operands,
// and both seem to be optional... and the processor magically understands these things and parsers
// instructions correctly ????
#define REX(W, R, X, B) (0b01000000 | ((W) << 3) | ((R) << 2) | ((X) << 1) | (B))
#define ModRM(mod, reg, rm) (((mod) << 6) | ((reg) << 3) | (rm))

void move_reg64_to_reg64(Data_Buffer *dataptr, u8 src, u8 dst) {
    dataptr->append_byte(REX(1, (dst & 0b1000) >> 3, 0, (src & 0b1000) >> 3));
    dataptr->append_byte(0x8B);
    dataptr->append_byte(ModRM(0b11, dst & 0b0111, src & 0b0111));
}

void move_reg64_to_memory(Data_Buffer *dataptr, u8 src, u8 dst, u32 disp) {
    dataptr->append_byte(REX(1, (src & 0b1000) >> 3, 0, (dst & 0b1000) >> 3));
    dataptr->append_byte(0x89);
    dataptr->append_byte(ModRM(0b10, src & 0b0111, dst & 0b0111));
    u32 *value = (u32  *)dataptr->allocate(4);
    *value = disp;
}

void move_memory_to_reg64(Data_Buffer *dataptr, u8 dst, u8 src, u32 disp) {
    dataptr->append_byte(REX(1, (dst & 0b1000) >> 3, 0, (src & 0b1000) >> 3));
    dataptr->append_byte(0x8B);
    dataptr->append_byte(ModRM(0b10, dst & 0b0111, src & 0b0111));
    u32 *value = (u32  *)dataptr->allocate(4);
    *value = disp;
}

void move_imm64_to_reg64(Data_Buffer *dataptr, u64 imm, u8 reg) {
    dataptr->append_byte(REX(1, 0, 0, 0));
    dataptr->append_byte(0xB8 + reg);
    u64 *value = (u64 *)dataptr->allocate(8);
    *value = imm;
}

// Need to allocate 4 bytes of space after this call
void lea_rip_relative_into_reg64(Data_Buffer *dataptr, u8 reg) {
    dataptr->append_byte(REX(1, (reg & 0b1000) >> 3, 0, 0));
    dataptr->append_byte(0x8D);
    dataptr->append_byte(ModRM(0b00, (reg & 0b0111), 0b101));
}

void lea_into_reg64(Data_Buffer *dataptr, u8 dst, u8 src, u32 disp) {
    dataptr->append_byte(REX(1, (dst & 0b1000) >> 3, 0, (src & 0b1000) >> 3));
    dataptr->append_byte(0x8D);
    dataptr->append_byte(ModRM(0b10, (dst & 0b0111), src & 0b0111));
    u32 *value = (u32  *)dataptr->allocate(4);
    *value = disp;
}

void pop_reg64(Data_Buffer *dataptr, u8 reg) {
    dataptr->append_byte(REX(1, 0, 0, (reg & 0b1000) >> 3));
    dataptr->append_byte(0x58 + (reg & 0b0111));
}

void push_reg64(Data_Buffer *dataptr, u8 reg) {
    dataptr->append_byte(REX(1, 0, 0, (reg & 0b1000) >> 3));
    dataptr->append_byte(0x50 + (reg & 0b0111));
}

u32 *sub_imm32_from_reg64(Data_Buffer *dataptr, u8 reg, u32 value) {
    dataptr->append_byte(REX(1, 0, 0, 0));
    dataptr->append_byte(0x81);
    dataptr->append_byte(ModRM(0b11, 5,  reg & 0b0111));
    u32 *operand = (u32 *)dataptr->allocate(4);
    *operand = value;

    return operand;
}

u32 *add_imm32_to_reg64(Data_Buffer *dataptr, u8 reg, u32 value) {
    dataptr->append_byte(REX(1, 0, 0, 0));
    dataptr->append_byte(0x81);
    dataptr->append_byte(ModRM(0b11, 0,  reg & 0b0111));
    u32 *operand = (u32 *)dataptr->allocate(4);
    *operand = value;

    return operand;
}

void add_reg64_to_reg64(Data_Buffer *dataptr, u8 src, u8 dst) {
    dataptr->append_byte(REX(1, (src & 0b1000) >> 3, 0, (dst & 0b1000) >> 3));
    dataptr->append_byte(0x01);
    dataptr->append_byte(ModRM(0b11, (src & 0b0111),  (dst & 0b0111)));
}

void sub_reg64_from_reg64(Data_Buffer *dataptr, u8 src, u8 dst) {
    dataptr->append_byte(REX(1, (src & 0b1000) >> 3, 0, (dst & 0b1000) >> 3));
    dataptr->append_byte(0x29);
    dataptr->append_byte(ModRM(0b11, (src & 0b0111),  (dst & 0b0111)));
}


u8 load_instruction_result(Function *function, Data_Buffer *dataptr, Instruction *inst) {
    if (auto reg = inst->result_stored_in) {
        return reg->machine_reg;
    } else {
        reg = function->get_free_register();
        if (!reg) {
            reg = &function->register_usage[RAX];
            function->maybe_spill_register(dataptr, reg);
            reg->is_free = false;
        }

        function->claim_register(dataptr, reg->machine_reg, inst);

        move_memory_to_reg64(dataptr, reg->machine_reg, RBP, inst->result_spilled_onto_stack);
        inst->result_spilled_onto_stack = 0xFFFFFFFF;
        return reg->machine_reg;
    }
}


u8 emit_load_of_value(Linker_Object *object, Function *function, Section *code_section, Section *data_section, Value *value) {
    if (value->type == VALUE_CONSTANT) {
        auto constant = static_cast<Constant *>(value);

        Register *reg = function->get_free_register();
        if (!reg) {
            reg = &function->register_usage[RAX];
            function->maybe_spill_register(&code_section->data, reg);
            reg->is_free = false;
        }

        if (constant->constant_type == Constant::STRING) {
            auto data_sec_offset = data_section->data.size();

            // copy the string characters into the data section
            void *data_target = data_section->data.allocate(constant->string_value.length);
            memcpy(data_target, constant->string_value.data, constant->string_value.length);
            data_section->data.append_byte(0);


            if (object->use_absolute_addressing) {
                // @Cutnpaste move_imm64_to_reg64
                code_section->data.append_byte(REX(1, 0, 0, 0));
                code_section->data.append_byte(0xB8 + reg->machine_reg);

                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.offset = code_section->data.size();
                reloc.symbol_index = data_section->symbol_index;
                reloc.size = 8;

                u64 *addr = (u64 *)code_section->data.allocate(8);
                *addr = data_sec_offset;;
                reloc.addend = data_sec_offset;

                code_section->relocations.add(reloc);
            } else {
                // lea data-section-location(%rip), %reg
                lea_rip_relative_into_reg64(&code_section->data, reg->machine_reg);

                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.is_rip_relative = true;
                reloc.offset = code_section->data.size();
                reloc.symbol_index = data_section->symbol_index;
                reloc.size = 4;

                code_section->relocations.add(reloc);

                u32 *value = (u32 *)code_section->data.allocate(4);
                *value = data_sec_offset;
            }

            // if (_register != RAX) move_reg64_to_reg64(&code_section->data, RAX, _register);
        } else if (constant->constant_type == Constant::INTEGER) {
            move_imm64_to_reg64(&code_section->data, constant->integer_value, reg->machine_reg);
        }

        return reg->machine_reg;
    } else if (value->type == INSTRUCTION_ALLOCA) {
        auto _alloca = static_cast<Instruction_Alloca *>(value);

        Register *reg = function->get_free_register();
        if (!reg) {
            reg = &function->register_usage[RAX];
            function->maybe_spill_register(&code_section->data, reg);
            reg->is_free = false;
        }

        function->claim_register(&code_section->data, reg->machine_reg, _alloca);

        lea_into_reg64(&code_section->data, reg->machine_reg, RBP, _alloca->stack_offset);
        return reg->machine_reg;
    } else if (value->type >= INSTRUCTION_FIRST && value->type <= INSTRUCTION_LAST) {
        auto inst = static_cast<Instruction *>(value);
        return load_instruction_result(function, &code_section->data, inst);
    }

    assert(false);
    return 0;
}

enum {
    TARGET_UNKNOWN,
    TARGET_WIN32,
    TARGET_MACOSX,
};

// @Temporary until we have a means to describe the target/target-triple
#ifdef _WIN32
u32 target_system = TARGET_WIN32;
#else
u32 target_system = TARGET_MACOSX;
#endif


u8 emit_instruction(Linker_Object *object, Function *function, Section *code_section, Section *data_section, Instruction *inst) {
    switch (inst->type) {
        case INSTRUCTION_ALLOCA: {
            break;
        }

        case INSTRUCTION_STORE: {
            auto store = static_cast<Instruction_Store *>(inst);

            u8 target = emit_load_of_value(object, function, code_section, data_section, store->store_target);
            u8 source = emit_load_of_value(object, function, code_section, data_section, store->source_value);
            move_reg64_to_memory(&code_section->data, source, target, 0);
            break;
        }

        case INSTRUCTION_LOAD: {
            auto load = static_cast<Instruction_Load *>(inst);

            u8 source = emit_load_of_value(object, function, code_section, data_section, load->pointer_value);

            Register *reg = function->get_free_register();
            if (!reg) {
                reg = &function->register_usage[RAX];
                function->maybe_spill_register(&code_section->data, reg);
                reg->is_free = false;
            }

            function->claim_register(&code_section->data, reg->machine_reg, inst);

            move_memory_to_reg64(&code_section->data, reg->machine_reg, source, 0);
            return reg->machine_reg;
        }

        case INSTRUCTION_SUB:
        case INSTRUCTION_ADD: {
            auto add = static_cast<Instruction_Add *>(inst);

            // Ensure two registers are free for the operation
            function->maybe_spill_register(&code_section->data, &function->register_usage[RAX]);
            function->maybe_spill_register(&code_section->data, &function->register_usage[RCX]);

            u8 lhs_reg = emit_load_of_value(object, function, code_section, data_section, add->lhs);
            u8 rhs_reg = emit_load_of_value(object, function, code_section, data_section, add->rhs);

            Register *reg = &function->register_usage[lhs_reg];
            function->maybe_spill_register(&code_section->data, reg);
            reg->is_free = false;

            function->claim_register(&code_section->data, reg->machine_reg, inst);

            if      (inst->type == INSTRUCTION_ADD) add_reg64_to_reg64(&code_section->data, rhs_reg, lhs_reg);
            else if (inst->type == INSTRUCTION_SUB) sub_reg64_from_reg64(&code_section->data, rhs_reg, lhs_reg);
            return reg->machine_reg;
        }

        case INSTRUCTION_CALL: {
            auto call = static_cast<Instruction_Call *>(inst);

            if (target_system == TARGET_WIN32) {
                // shadow space for the callee to spill registers...
                sub_imm32_from_reg64(&code_section->data, RSP, 32);
            }

            u8 index = 0;
            for (auto p : call->parameters) {
                u8 param_reg = 0xFF;

                if (target_system == TARGET_WIN32) {
                    param_reg = get_next_win64_abi_register(&index);
                } else {
                    param_reg = get_next_system_v_abi_register(&index);
                }

                assert(param_reg != 0xFF);

                u8 result = emit_load_of_value(object, function, code_section, data_section, p);

                if (result != param_reg) move_reg64_to_reg64(&code_section->data, result, param_reg);
            }

            // load number of floating point parameters into %al
            code_section->data.append_byte(REX(1, 0, 0, 0));
            code_section->data.append_byte(0xB8 + RAX);
            u64 *value = (u64 *)code_section->data.allocate(8);
            *value = 0;

            if (object->use_absolute_addressing) {
                function->maybe_spill_register(&code_section->data, &function->register_usage[RBX]);

                // @Cutnpaste move_imm64_to_reg64
                code_section->data.append_byte(REX(1, 0, 0, 0));
                code_section->data.append_byte(0xB8 + RBX);

                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.offset = code_section->data.size();
                reloc.symbol_index = get_symbol_index(object, static_cast<Function *>(call->call_target));
                reloc.size = 8;

                u64 *addr = (u64 *)code_section->data.allocate(8);
                *addr = 0;
                reloc.addend = 0; // @TODO

                code_section->relocations.add(reloc);

                code_section->data.append_byte(REX(1, 0, 0, 0));
                code_section->data.append_byte(0xFF); // callq reg
                code_section->data.append_byte(ModRM(0b11, 2, RBX));
            } else {
                code_section->data.append_byte(REX(1, 0, 0, 0));
                code_section->data.append_byte(0xE8); // callq rip-relative
                // code_section->data.append_byte(ModRM(0b00, 0b000, 0b101));

                Relocation reloc;
                reloc.is_for_rip_call = true;
                reloc.offset = code_section->data.size();
                reloc.symbol_index = get_symbol_index(object, static_cast<Function *>(call->call_target));
                reloc.size = 4;

                u32 *addr = (u32 *)code_section->data.allocate(4);
                *addr = 0;
                reloc.addend = 0; // @TODO

                code_section->relocations.add(reloc);
            }

            if (target_system == TARGET_WIN32) {
                add_imm32_to_reg64(&code_section->data, RSP, 32);
            }

            return RAX;
        }

        case INSTRUCTION_RETURN: {
            u32 *stack_size_target = add_imm32_to_reg64(&code_section->data, RSP, 0);
            function->stack_size_fixups.add(stack_size_target);


            // :WastefulPushPops:
            pop_reg64(&code_section->data, RSI);
            pop_reg64(&code_section->data, RCX);
            pop_reg64(&code_section->data, RDX);
            pop_reg64(&code_section->data, RBX);
            pop_reg64(&code_section->data, RAX);
            pop_reg64(&code_section->data, RDI);
            

            pop_reg64(&code_section->data, RBP);

            code_section->data.append_byte(0xC3);
            break;
        }

        default: assert(false);
    }

    return 0;
}

void emit_function(Linker_Object *object, Section *code_section, Section *data_section, Function *function) {
    u32 symbol_index = get_symbol_index(object, function);
    Symbol *sym = &object->symbol_table[symbol_index];
    sym->is_function = true;
    sym->is_externally_defined = (function->blocks.count == 0);
    if (sym->is_externally_defined) return;

    if (!sym->is_externally_defined) sym->section_number = code_section->section_number;
    sym->section_offset = code_section->data.size();

    function->register_usage.resize(RBX+1);

    u8 count = RAX;
    for (auto &reg : function->register_usage) {
        reg.machine_reg = count;
        reg.is_free = true;

        ++count;
    }

    push_reg64(&code_section->data, RBP);

    // :WastefulPushPops: @Cleanup pushing all the registers we may need is
    // a bit excessive and wasteful.
    push_reg64(&code_section->data, RDI);
    push_reg64(&code_section->data, RAX);
    push_reg64(&code_section->data, RBX);
    push_reg64(&code_section->data, RDX);
    push_reg64(&code_section->data, RCX);
    push_reg64(&code_section->data, RSI);
    
    move_reg64_to_reg64(&code_section->data, RSP, RBP);

    function->stack_size = 0;

    for (auto block : function->blocks) {
        for (auto inst : block->instructions) {
            if (inst->type == INSTRUCTION_ALLOCA) {
                auto _alloca = static_cast<Instruction_Alloca *>(inst);

                _alloca->stack_offset = function->stack_size;
                function->stack_size += _alloca->alloca_type->size;
            }
        }
    }


    u32 *stack_size_target = sub_imm32_from_reg64(&code_section->data, RSP, 0);

    for (auto block : function->blocks) {
        for (auto inst : block->instructions) {
            emit_instruction(object, function, code_section, data_section, inst);
        }
    }

    // Ensure stack is 16-byte aligned.
    if ((function->stack_size % 16)) function->stack_size += 16 - (function->stack_size % 16);

    *stack_size_target = function->stack_size;

    for (auto fixup : function->stack_size_fixups) {
        *fixup = function->stack_size;
    }
}

void generate_linker_object(Compilation_Unit *unit, Linker_Object *object, u32 *text_index, u32 *data_index) {
    u32 data_sec_index = 0;
    u32 text_sec_index = 0;

    {
        Section data_sec = {};

        if (target_system == TARGET_MACOSX) {
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

        if (target_system == TARGET_MACOSX) {
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

    generate_linker_object(unit, &object, nullptr, nullptr);

    void emit_macho_file(Linker_Object *object);
    void emit_coff_file(Linker_Object *object);

    if (target_system == TARGET_WIN32) {
        emit_coff_file(&object);
    } else if (target_system == TARGET_MACOSX) {
        emit_macho_file(&object);
    }
}

#ifndef _WIN32
#include <sys/mman.h>
#include <dlfcn.h>

void do_jit_and_run_program_main(Compilation_Unit *unit) {
    // @Cutnpaste from emit_obj_file
    Linker_Object object = {};
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

    void (*prog_main)() = (void (*)())text_memory;
    prog_main();
}
#endif

int main(int argc, char **argv) {
    Compilation_Unit unit;
    Function *printf_func = new Function();
    printf_func->name = "printf";

    Function *main_func = new Function();
    main_func->name = "main";

    Basic_Block *block = new Basic_Block();
    main_func->blocks.add(block);

    Instruction_Alloca *_alloca = make_alloca(make_integer_type(8));
    block->instructions.add(_alloca);
    block->instructions.add(make_store(make_integer_constant(1234), _alloca));

    Instruction_Load *load = make_load(_alloca);
    block->instructions.add(load);

    Instruction_Add *add = make_add(load, make_integer_constant(2345));
    block->instructions.add(add);

    Instruction_Sub *sub = make_sub(add, make_integer_constant(79));
    block->instructions.add(sub);

    Instruction_Call *call = new Instruction_Call();
    call->call_target = printf_func;
    call->parameters.add(make_string_constant("Hello World: %d\n"));
    call->parameters.add(sub);

    block->instructions.add(call);
    block->instructions.add(new Instruction_Return());

    unit.functions.add(printf_func);
    unit.functions.add(main_func);


    emit_obj_file(&unit);
    // do_jit_and_run_program_main(&unit);

    return 0;
}

