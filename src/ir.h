
#ifndef IR_H
#define IR_H

#include "linker_object.h"

struct Function;
struct Instruction;
struct Basic_Block;

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
    INSTRUCTION_MUL,
    INSTRUCTION_,
    INSTRUCTION_GEP,
    INSTRUCTION_BRANCH,
    INSTRUCTION_DIV,

    INSTRUCTION_LAST,
};

struct Type {
    enum {
        VOID,
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

        Type *result_type;
    };
};

inline
Type *make_void_type() {
    Type *type = new Type();
    type->type = Type::VOID;
    return type;
}

// @TODO parameter types.
inline
Type *make_func_type(Type *result_type) {
    Type *type = new Type();
    type->type = Type::FUNCTION;
    type->result_type = result_type;
    return type;
}

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

    String name;
    u32 uses = 0;
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

    bool is_integer() { return constant_type == INTEGER; }
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
Constant *make_integer_constant(u64 value, Type *value_type = make_integer_type(8)) {
    Constant *con = new Constant();
    con->constant_type = Constant::INTEGER;
    con->integer_value = value;
    con->value_type = value_type;

    assert(value_type->type == Type::INTEGER);
    return con;
}

struct Register {
    u8 machine_reg;
    bool is_free = true;
    Instruction *currently_holding_result_of_instruction = nullptr;
};

struct Instruction : Value {
    Register *result_stored_in    = nullptr;
    s32 result_spilled_onto_stack = 0;

    Basic_Block *inserted_into_block = nullptr;
    u32 insertion_index = 0xFFFFFFFF;
    bool emitted = false;
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
    s32 stack_offset = 0;
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

struct Instruction_Mul : Instruction_Math_Binary_Op {
    Instruction_Mul() { type = INSTRUCTION_MUL; }
};

struct Instruction_Div : Instruction_Math_Binary_Op {
    Instruction_Div() { type = INSTRUCTION_DIV; }

    bool signed_division;
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

struct Basic_Block : Value {
    Basic_Block() { type = VALUE_BASIC_BLOCK; }
    Array<Instruction *> instructions;

    Function *inserted_into_function = nullptr;
    u32 insertion_index = 0xFFFFFFFF;

    // For code gen
    u64 text_location = 0;
    Array<u64>   text_locations_needing_addr_fixup;
    Array<u32 *> text_ptrs_for_fixup;

    bool has_terminator() {
        if (instructions.count == 0) return false;

        auto last = instructions[instructions.count-1];
        return last->type == INSTRUCTION_BRANCH || last->type == INSTRUCTION_RETURN;
    }

    void insert(Instruction *inst) {
        assert(!has_terminator());
        assert(inst->inserted_into_block == nullptr);

        inst->inserted_into_block = this;
        inst->insertion_index = this->instructions.count;
        this->instructions.add(inst);
    }
};

struct Global_Value : Constant {
    String name;
    u32 symbol_index = 0;
};


struct Function : Global_Value {
    enum Intrinsics {
        NOT_INTRINSIC,
        DEBUG_BREAK,
    };
    u32 intrinsic_id = NOT_INTRINSIC;

    Array<Type *> parameter_types;
    Array<Basic_Block *> blocks;

    // For code gen
    Array<Register> register_usage;

    // @Temporary this information is the same across all functions,
    // we should put these in Target or something...
    Array<u8>       parameter_registers;

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
    s32 stack_size = 0;
    s32 largest_call_stack_adjustment = 0;

    void insert(Basic_Block *block) {
        block->insertion_index = this->blocks.count;
        block->inserted_into_function = this;

        this->blocks.add(block);
    }
};

struct Compilation_Unit {
    Target target;
    Array<Function *> functions;
};



inline
Instruction_Branch *make_branch(Value *condition_or_null, Value *true_target, Value *failure_target = nullptr) {
    Instruction_Branch *branch = new Instruction_Branch();
    branch->condition      = condition_or_null;
    branch->true_target    = true_target;
    branch->failure_target = failure_target;

    if (condition_or_null) condition_or_null->uses++;
    if (failure_target)    failure_target->uses++;
    true_target->uses++;
    return branch;
}

inline
Instruction_GEP *make_gep(Value *pointer_value, Value *index) {
    Instruction_GEP *gep = new Instruction_GEP();
    gep->pointer_value = pointer_value;
    gep->index         = index;
    gep->value_type = pointer_value->value_type;

    pointer_value->uses++;
    index->uses++;
    return gep;
}

inline
Instruction_Add *make_add(Value *lhs, Value *rhs) {
    // @Incomplete assert that lhs and rhs types match
    Instruction_Add *add = new Instruction_Add();
    add->value_type = lhs->value_type;
    add->lhs = lhs;
    add->rhs = rhs;
    add->value_type = lhs->value_type;

    lhs->uses++;
    rhs->uses++;
    return add;
}

inline
Instruction_Sub *make_sub(Value *lhs, Value *rhs) {
    // @Incomplete assert that lhs and rhs types match
    Instruction_Sub *sub = new Instruction_Sub();
    sub->value_type = lhs->value_type;
    sub->lhs = lhs;
    sub->rhs = rhs;
    sub->value_type = lhs->value_type;

    lhs->uses++;
    rhs->uses++;
    return sub;
}

inline
Instruction_Mul *make_mul(Value *lhs, Value *rhs) {
    // @Incomplete assert that lhs and rhs types match
    Instruction_Mul *sub = new Instruction_Mul();
    sub->value_type = lhs->value_type;
    sub->lhs = lhs;
    sub->rhs = rhs;
    sub->value_type = lhs->value_type;

    lhs->uses++;
    rhs->uses++;
    return sub;
}

inline
Instruction_Div *make_div(Value *lhs, Value *rhs, bool signed_division) {
    // @Incomplete assert that lhs and rhs types match
    Instruction_Div *div = new Instruction_Div();
    div->value_type = lhs->value_type;
    div->lhs = lhs;
    div->rhs = rhs;
    div->value_type = lhs->value_type;
    div->signed_division = signed_division;

    lhs->uses++;
    rhs->uses++;
    return div;
}


inline
Instruction_Load *make_load(Value *pointer_value) {
    assert(pointer_value->value_type->type == Type::POINTER);
    Instruction_Load *load = new Instruction_Load();
    load->pointer_value = pointer_value;
    load->value_type    = pointer_value->value_type->pointer_to;

    pointer_value->uses++;
    return load;
}

inline
Instruction_Store *make_store(Value *source_value, Value *store_target) {
    assert(store_target->value_type->type == Type::POINTER);

    Instruction_Store *store = new Instruction_Store();
    store->source_value = source_value;
    store->store_target = store_target;

    source_value->uses++;
    store_target->uses++;
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


// @TODO parameter types
inline
Instruction_Call *make_call(Function *func, const Array_Slice<Value *> &parameters = Array_Slice<Value *>()) {
    assert(func->value_type && func->value_type->type == Type::FUNCTION);
    Instruction_Call *call = new Instruction_Call();
    call->call_target = func;
    call->value_type = func->value_type->result_type;

    for (const auto v : parameters) {
        call->parameters.add(v);
        v->uses++;
    }
    return call;
}

inline
Instruction_Return *make_return(Value *retval = nullptr) {
    Instruction_Return *ret = new Instruction_Return();
    ret->return_value = retval;
    if (retval) retval->uses++;
    return ret;
}

inline
Constant *is_constant(Value *value) {
    return (value->type == VALUE_CONSTANT) ? static_cast<Constant *>(value) : nullptr;
}

inline
Instruction *is_instruction(Value *value) {
    if (value->type >= INSTRUCTION_FIRST && value->type <= INSTRUCTION_LAST) return static_cast<Instruction *>(value);

    return nullptr;
}

struct IR_Manager {
    Type *i8;
    Type *i16;
    Type *i32;
    Type *i64;

    Basic_Block *block = nullptr;

    IR_Manager() {
        i8  = make_integer_type(1);
        i16 = make_integer_type(2);
        i32 = make_integer_type(4);
        i64 = make_integer_type(8);
    }

    void set_block(Basic_Block *block) {
        this->block = block;
    }

    Instruction_Alloca *insert_alloca(Type *alloca_type, u32 array_size = 1) {
        auto _alloca = make_alloca(alloca_type, array_size);
        block->insert(_alloca);
        return _alloca;
    }

    Instruction_Load *insert_load(Value *pointer_value) {
        auto load = make_load(pointer_value);
        block->insert(load);
        return load;
    }

    Instruction_Store *insert_store(Value *source_value, Value *store_target) {
        auto store = make_store(source_value, store_target);
        block->insert(store);
        return store;
    }

    Instruction_Add *insert_add(Value *lhs, Value *rhs) {
        auto add = make_add(lhs, rhs);
        block->insert(add);
        return add;
    }

    Instruction_Sub *insert_sub(Value *lhs, Value *rhs) {
        auto sub = make_sub(lhs, rhs);
        block->insert(sub);
        return sub;
    }

    Instruction_Mul *insert_mul(Value *lhs, Value *rhs) {
        auto mul = make_mul(lhs, rhs);
        block->insert(mul);
        return mul;
    }

    Instruction_Div *insert_div(Value *lhs, Value *rhs, bool signed_division) {
        auto div = make_div(lhs, rhs, signed_division);
        block->insert(div);
        return div;
    }

    Instruction_Branch *insert_branch(Value *condition_or_null, Value *true_target, Value *failure_target = nullptr) {
        auto br = make_branch(condition_or_null, true_target, failure_target);
        block->insert(br);
        return br;
    }

    Instruction_Branch *insert_branch(Value *target) {
        return insert_branch(nullptr, target, nullptr);
    }

    Instruction_Call *insert_call(Function *func, const Array_Slice<Value *> &parameters = Array_Slice<Value *>()) {
        auto call = make_call(func, parameters);
        block->insert(call);
        return call;
    }

    Instruction_Return *insert_return(Value *retval = nullptr) {
        auto ret = make_return(retval);
        block->insert(ret);
        return ret;
    }

    Instruction_GEP *insert_gep(Value *pointer_value, Value *index) {
        auto gep = make_gep(pointer_value, index);
        block->insert(gep);
        return gep;
    }
};


#endif
