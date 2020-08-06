
#ifndef IR_H
#define IR_H

#include "linker_object.h"

enum {
    VALUE_CONSTANT,
    VALUE_BASIC_BLOCK,

    INSTRUCTION_FIRST,
    
    INSTRUCTION_CALL = INSTRUCTION_FIRST,
    INSTRUCTION_RETURN,
    INSTRUCTION_ALLOCA,
    INSTRUCTION_STORE,
    INSTRUCTION_LOAD,
    INSTRUCTION_ADD,
    INSTRUCTION_SUB,
    INSTRUCTION_GEP,
    INSTRUCTION_BRANCH,

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

inline
Type *make_integer_type(u32 size) {
    Type *type      = new Type();
    type->type      = Type::INTEGER;
    type->size      = size;
    type->alignment = type->size;
    return type;
}

inline
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

inline
Constant *make_string_constant(String value) {
    Constant *con = new Constant();
    con->constant_type = Constant::STRING;
    con->string_value = value;
    con->value_type = make_pointer_type(make_integer_type(1));
    return con;
}

inline
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
    u32 array_size   = 1;
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

struct Instruction_GEP : Instruction {
    Instruction_GEP() { type = INSTRUCTION_GEP; }

    Value *pointer_value = nullptr;
    Value *index         = nullptr;
};

struct Instruction_Branch : Instruction {
    Instruction_Branch() { type = INSTRUCTION_BRANCH; }

    Value *condition      = nullptr;
    Value *true_target    = nullptr;
    Value *failure_target = nullptr;
};

inline
Instruction_Branch *make_branch(Value *condition_or_null, Value *true_target, Value *failure_target = nullptr) {
    Instruction_Branch *branch = new Instruction_Branch();
    branch->condition      = condition_or_null;
    branch->true_target    = true_target;
    branch->failure_target = failure_target;
    return branch;
}

inline
Instruction_GEP *make_gep(Value *pointer_value, Value *index) {
    Instruction_GEP *gep = new Instruction_GEP();
    gep->pointer_value = pointer_value;
    gep->index         = index;
    gep->value_type = pointer_value->value_type;
    return gep;
}

inline
Instruction_Add *make_add(Value *lhs, Value *rhs) {
    // @Incomplete assert that lhs and rhs types match
    Instruction_Add *add = new Instruction_Add();
    add->value_type = lhs->value_type;
    add->lhs = lhs;
    add->rhs = rhs;
    return add;
}

inline
Instruction_Sub *make_sub(Value *lhs, Value *rhs) {
    // @Incomplete assert that lhs and rhs types match
    Instruction_Sub *sub = new Instruction_Sub();
    sub->value_type = lhs->value_type;
    sub->lhs = lhs;
    sub->rhs = rhs;
    return sub;
}

inline
Instruction_Load *make_load(Value *pointer_value) {
    assert(pointer_value->value_type->type == Type::POINTER);
    Instruction_Load *load = new Instruction_Load();
    load->pointer_value = pointer_value;
    load->value_type    = pointer_value->value_type->pointer_to;
    return load;
}

inline
Instruction_Store *make_store(Value *source_value, Value *store_target) {
    Instruction_Store *store = new Instruction_Store();
    store->source_value = source_value;
    store->store_target = store_target;
    return store;
}

inline
Instruction_Alloca *make_alloca(Type *alloca_type, u32 array_size = 1) {
    Instruction_Alloca *_alloca = new Instruction_Alloca();
    _alloca->alloca_type = alloca_type;
    _alloca->value_type = make_pointer_type(alloca_type);
    _alloca->array_size = array_size;
    return _alloca;
}

struct Basic_Block : Value {
    Basic_Block() { type = VALUE_BASIC_BLOCK; }
    Array<Instruction *> instructions;

    // For code gen
    u64 text_location = 0;
    Array<u64>   text_locations_needing_addr_fixup;
    Array<u32 *> text_ptrs_for_fixup;
};

struct Global_Value : Constant {
    String name;
    u64 symbol_index = 0;
};


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
        void maybe_spill_register(Function *func, Data_Buffer *dataptr, Register *reg);
        maybe_spill_register(this, dataptr, reg);

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


    Array<u32 *>    stack_size_fixups;
    u32 stack_size = 0;
};

struct Compilation_Unit {
    Target target;
    Array<Function *> functions;
};


#endif
