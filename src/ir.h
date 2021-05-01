
#ifndef JOSH_IR_H
#define JOSH_IR_H

#include "linker_object.h"


namespace josh {

struct Value;
struct Function;
struct Instruction;
struct Basic_Block;

struct IR_Context {
    String_Table<String, /*null-terminate string entries*/ true> string_interner;
    Bump_Allocator<> node_storage;

    template <typename T>
    T *new_node() {
        return node_storage.allocate<T>();
    }

    String_ID intern(const String &str) {
        return string_interner.intern(str);
    }

    String get_string(String_ID id) {
        return string_interner.lookup(id);
    }

    struct JIT_Symbol_Info {
        void *entry_ptr;
    };

    char *text_memory = nullptr;
    Map<String_ID, JIT_Symbol_Info> jit_symbols;
};

enum {
    VALUE_CONSTANT,
    VALUE_GLOBAL,
    VALUE_BASIC_BLOCK,
    VALUE_ARGUMENT,

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
    enum _Type : u32 {
        VOID,
        INTEGER,
        FLOAT,
        POINTER,
        FUNCTION,
        STRUCT,
    } type;

    u32 size;
    u32 alignment;

    template<typename T>
    T *as() {
        return (type == T::TYPE) ? static_cast<T *>(this) : nullptr;
    }
};

struct Void_Type : Type {
    static const Type::_Type TYPE = VOID;
    Void_Type() { type = TYPE; }
};

struct Integer_Type : Type {
    static const Type::_Type TYPE = INTEGER;
    Integer_Type() { type = TYPE; }
};

struct Float_Type : Type {
    static const Type::_Type TYPE = FLOAT;
    Float_Type() { type = TYPE; }
};

struct Pointer_Type : Type {
    static const Type::_Type TYPE = POINTER;
    Pointer_Type() { type = TYPE; }

    Type *pointer_to;
};

struct Function_Type : Type {
    static const Type::_Type TYPE = FUNCTION;
    Function_Type() { type = TYPE; }

    Array<Type *> parameters;
    Type *result_type;
    bool is_varargs;
};

struct Struct_Type : Type {
    static const Type::_Type TYPE = STRUCT;
    Struct_Type() { type = TYPE; }

    Array<Type *> members;
    // TODO packedness
    // bool packed = false;

    u32 offset_of(size_t index) {
        assert(index < members.size());

        u32 offset_within_struct = 0;
        for (size_t i = 0; i < index; ++i) {
            auto m = members[i];

             if (offset_within_struct % m->alignment)
                offset_within_struct += m->alignment - (offset_within_struct % m->alignment);

            offset_within_struct += m->size;
        }

        Type *mem = members[index];

        if (offset_within_struct % mem->alignment)
             offset_within_struct += mem->alignment - (offset_within_struct % mem->alignment);

        return offset_within_struct;
    }
};

inline Struct_Type *make_struct_type(const Array_Slice<Type *> &members = Array_Slice<Type *>() /*, bool packed = false*/) {
    Struct_Type *str = new Struct_Type();
    str->size = 0;
    str->alignment = 1;
    // str->packed = packed;

    for (auto m : members) {
        str->members.push_back(m);

        if (str->size % m->alignment)
            str->size += m->alignment - (str->size % m->alignment);

        str->size += m->size;

        // TODO account for packedness
        if (str->alignment < m->alignment)
            str->alignment = m->alignment;
    }

    // TODO we may want to differentiate between size and stride of a struct
    if (str->size % str->alignment)
        str->size += str->alignment - (str->size % str->alignment);

    return str;
}

inline
Type *make_void_type() {
    Type *type = new Type();
    type->type = Type::VOID;
    return type;
}

inline
Function_Type *make_func_type(Type *result_type, const Array_Slice<Type *> &parameters = Array_Slice<Type *>(), bool is_varargs = false) {
    Function_Type *type = new Function_Type();

    type->result_type = result_type;
    type->size = 8; // @TargetInfo
    type->alignment = type->size;

    for (auto p : parameters) {
        type->parameters.push_back(p);
    }
    type->is_varargs = is_varargs;
    return type;
}

inline
Integer_Type *make_integer_type(u32 size) {
    assert(size >= 1 && size <= 8);
    Integer_Type *type      = new Integer_Type();
    type->size      = size;
    type->alignment = type->size;
    return type;
}

inline
Float_Type *make_float_type(u32 size) {
    Float_Type *type      = new Float_Type();
    type->size      = size;
    type->alignment = size; 
    return type;
}

inline
Pointer_Type *make_pointer_type(Type *pointee) {
    Pointer_Type *type = new Pointer_Type();
    type->size = 8; // @TargetInfo
    type->alignment = type->size;
    type->pointer_to = pointee;
    return type;
}

inline
bool types_match(Type *a, Type *b) {

    if (a->type != b->type)           return false;
    if (a->size != b->size)           return false;
    if (a->alignment != b->alignment) return false;

    if (a->type == Type::POINTER) {
        return types_match(static_cast<Pointer_Type *>(a)->pointer_to, static_cast<Pointer_Type *>(b)->pointer_to);
    }

    if (a->type == Type::FUNCTION) {
        Function_Type *afunc = static_cast<Function_Type *>(a);
        Function_Type *bfunc = static_cast<Function_Type *>(b);

        if (afunc->parameters.size() != bfunc->parameters.size()) return false;
        if (!types_match(afunc->result_type, bfunc->result_type)) return false;
        if (afunc->is_varargs != bfunc->is_varargs) return false;

        for (u32 i = 0; i < afunc->parameters.size(); ++i) {
            if (!types_match(afunc->parameters[i], bfunc->parameters[i])) return false;
        }

        return true;
    }

    if (a->type == Type::STRUCT) {
        Struct_Type *astr = static_cast<Struct_Type *>(a);
        Struct_Type *bstr = static_cast<Struct_Type *>(b);

        // Right now, if two structs have all the same fields, they match
        if (astr->members.size() != bstr->members.size())
            return false;

        for (size_t i = 0; i < astr->members.size(); ++i) {
            if (!types_match(astr->members[i], bstr->members[i]))
                return false;
        }

        return true;
    }

    return true;
}

struct Register {
    u8 machine_reg;
    bool is_free = true;
    bool used    = false;
    Value *currently_holding_result_of_instruction = nullptr;
};

struct Value {
    u32 type;
    Type *value_type;

    String_ID name;
    u32 uses = 0;
    Register *result_stored_in    = nullptr;
    s32 result_spilled_onto_stack = 0;
};

struct Constant : Value {
    Constant() { type = VALUE_CONSTANT; }

    enum {
        STRING,
        INTEGER,
        FLOAT,
        POINTER, // uses integer_value
    };

    u32 constant_type;

    union {
        String string_value;
        u64    integer_value;
        double float_value;
    };

    bool is_integer() { return constant_type == INTEGER; }
};

inline
Constant *make_string_constant(IR_Context *context, const String &value) {
    Constant *con = context->new_node<Constant>();
    con->constant_type = Constant::STRING;
    char *buffer = (char *)malloc(value.length()); // @Leak
    memcpy(buffer, value.data(), value.length());
    con->string_value = String{buffer, value.length()};
    con->value_type = make_pointer_type(make_integer_type(1));
    return con;
}

inline
Constant *make_integer_constant(IR_Context *context, u64 value, Type *value_type = make_integer_type(8)) {
    Constant *con = context->new_node<Constant>();
    con->constant_type = Constant::INTEGER;
    con->integer_value = value;
    con->value_type = value_type;

    assert(value_type->type == Type::INTEGER);
    return con;
}

inline
Constant *make_float_constant(IR_Context *context, double value, Type *value_type = make_float_type(8)) {
    Constant *con = context->new_node<Constant>();
    con->constant_type = Constant::FLOAT;
    con->float_value = value;
    con->value_type = value_type;

    assert(value_type->type == Type::FLOAT);
    return con;
}

inline
Constant *make_pointer_constant(IR_Context *context, u64 value, Type *value_type) {
    Constant *con = context->new_node<Constant>();
    con->constant_type = Constant::INTEGER;
    con->integer_value = value;
    con->value_type = value_type;

    assert(value_type->type == Type::POINTER);
    return con;
}

struct Instruction : Value {
    Basic_Block *inserted_into_block = nullptr;
    size_t insertion_index = SIZE_T_MAX;
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
    s32 offset = 0;
    bool is_struct_member_ptr = false;
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
    size_t insertion_index = SIZE_T_MAX;

    // For code gen
    u64 text_location = 0;
    Array<u64>   text_locations_needing_addr_fixup;
    Array<u32 *> text_ptrs_for_fixup;

    bool has_terminator() {
        if (instructions.size() == 0) return false;

        auto last = instructions[instructions.size()-1];
        return last->type == INSTRUCTION_BRANCH || last->type == INSTRUCTION_RETURN;
    }

    void insert(Instruction *inst) {
        assert(!has_terminator());
        assert(inst->inserted_into_block == nullptr);

        inst->inserted_into_block = this;
        inst->insertion_index = this->instructions.size();
        this->instructions.push_back(inst);
    }
};

struct Global_Value : Constant {
    Global_Value() { type = VALUE_GLOBAL; }

    String_ID name;
    u32 symbol_index = 0;
};

struct Argument : Value {
    Argument() { type = VALUE_ARGUMENT; }

    s32 copied_to_stack_offset = 0;
};

struct Function : Global_Value {
    enum Intrinsics {
        NOT_INTRINSIC,
        DEBUG_BREAK,
    };
    u32 intrinsic_id = NOT_INTRINSIC;

    enum Linkage {
        EXTERNAL,
        INTERNAL,
    };

    Linkage linkage = EXTERNAL;

    Array<Argument *> arguments;
    Array<Basic_Block *> blocks;

    void insert(Basic_Block *block) {
        block->insertion_index = this->blocks.size();
        block->inserted_into_function = this;

        this->blocks.push_back(block);
    }
};

struct Compilation_Unit {
    Target target;
    Array<Function *> functions;
};

inline
Constant *is_constant(Value *value) {
    return (value->type == VALUE_CONSTANT) ? static_cast<Constant *>(value) : nullptr;
}

inline
Instruction *is_instruction(Value *value) {
    if (value->type >= INSTRUCTION_FIRST && value->type <= INSTRUCTION_LAST) return static_cast<Instruction *>(value);

    return nullptr;
}

inline
Argument *make_arg(IR_Context *context, Type *type) {
    Argument *arg = context->new_node<Argument>();
    arg->value_type = type;
    return arg;
}

inline
Instruction_Branch *make_branch(IR_Context *context, Value *condition_or_null, Value *true_target, Value *failure_target = nullptr) {
    Instruction_Branch *branch = context->new_node<Instruction_Branch>();
    branch->condition      = condition_or_null;
    branch->true_target    = true_target;
    branch->failure_target = failure_target;

    if (condition_or_null) condition_or_null->uses++;
    if (failure_target)    failure_target->uses++;
    true_target->uses++;
    return branch;
}


struct Gep_Info { Type *ty; s32 offset; bool is_struct_member_ptr; };

inline
Gep_Info get_gep_value_type_and_offset(Type *ty, const Array_Slice<Value *> &indices, size_t depth, s32 offset = 0, bool is_member = false) {
    if (depth == indices.size())
        return {make_pointer_type(ty), offset, is_member};

    if (Struct_Type *str = ty->as<Struct_Type>()) {
        Constant *in = static_cast<Constant *>(indices.data[depth]);
        assert(is_constant(in));
        assert(in->constant_type == Constant::INTEGER);
        assert(in->integer_value >= 0 && in->integer_value < str->members.size());

        u32 offset_within_struct = str->offset_of(in->integer_value);
        Type *mem = str->members[in->integer_value];

        return get_gep_value_type_and_offset(mem, indices, depth + 1, offset + static_cast<s32>(offset_within_struct), true);
    } else if (Pointer_Type *ptr = ty->as<Pointer_Type>()) {
        assert(depth == 0);
        return get_gep_value_type_and_offset(ptr->pointer_to, indices, depth + 1, offset);
    }

    assert(false);
    return {nullptr, 0, false};
}

inline
Instruction_GEP *make_gep(IR_Context *context, Value *pointer_value, const Array_Slice<Value *> &indices = Array_Slice<Value *>()) {
    Instruction_GEP *gep = context->new_node<Instruction_GEP>();
    gep->pointer_value = pointer_value;

    assert(indices.size() > 0);
    gep->index = indices.data[0];
    gep->index->uses++;

    assert(pointer_value->value_type->as<Pointer_Type>());

    auto [ty, offset, is_member] = get_gep_value_type_and_offset(pointer_value->value_type, indices, 0);
    gep->value_type = ty;
    gep->offset = offset;
    gep->is_struct_member_ptr = is_member;

    pointer_value->uses++;
    return gep;
}

inline
Instruction_Add *make_add(IR_Context *context, Value *lhs, Value *rhs) {
    assert(types_match(lhs->value_type, rhs->value_type));
    assert(lhs->value_type->as<Integer_Type>());

    Instruction_Add *add = context->new_node<Instruction_Add>();
    add->value_type = lhs->value_type;
    add->lhs = lhs;
    add->rhs = rhs;
    add->value_type = lhs->value_type;

    lhs->uses++;
    rhs->uses++;
    return add;
}

inline
Instruction_Sub *make_sub(IR_Context *context, Value *lhs, Value *rhs) {
    assert(types_match(lhs->value_type, rhs->value_type));
    assert(lhs->value_type->as<Integer_Type>());

    Instruction_Sub *sub = context->new_node<Instruction_Sub>();
    sub->value_type = lhs->value_type;
    sub->lhs = lhs;
    sub->rhs = rhs;
    sub->value_type = lhs->value_type;

    lhs->uses++;
    rhs->uses++;
    return sub;
}

inline
Instruction_Mul *make_mul(IR_Context *context, Value *lhs, Value *rhs) {
    assert(types_match(lhs->value_type, rhs->value_type));
    assert(lhs->value_type->as<Integer_Type>());

    Instruction_Mul *sub = context->new_node<Instruction_Mul>();
    sub->value_type = lhs->value_type;
    sub->lhs = lhs;
    sub->rhs = rhs;
    sub->value_type = lhs->value_type;

    lhs->uses++;
    rhs->uses++;
    return sub;
}

inline
Instruction_Div *make_div(IR_Context *context, Value *lhs, Value *rhs, bool signed_division) {
    assert(types_match(lhs->value_type, rhs->value_type));
    assert(lhs->value_type->as<Integer_Type>());

    Instruction_Div *div = context->new_node<Instruction_Div>();
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
Instruction_Load *make_load(IR_Context *context, Value *pointer_value) {
    assert(pointer_value->value_type->as<Pointer_Type>());
    Instruction_Load *load = context->new_node<Instruction_Load>();
    load->pointer_value = pointer_value;
    load->value_type    = pointer_value->value_type->as<Pointer_Type>()->pointer_to;

    assert(load->value_type->type != Type::VOID);

    pointer_value->uses++;
    return load;
}

inline
Instruction_Store *make_store(IR_Context *context, Value *source_value, Value *store_target) {
    assert(store_target->value_type->type == Type::POINTER);

    assert(source_value->value_type->type != Type::VOID);

    Instruction_Store *store = context->new_node<Instruction_Store>();
    store->source_value = source_value;
    store->store_target = store_target;

    source_value->uses++;
    store_target->uses++;
    return store;
}

inline
Instruction_Alloca *make_alloca(IR_Context *context, Type *alloca_type, u32 array_size = 1) {
    Instruction_Alloca *_alloca = context->new_node<Instruction_Alloca>();
    _alloca->alloca_type = alloca_type;
    _alloca->value_type = make_pointer_type(alloca_type);
    _alloca->array_size = array_size;
    return _alloca;
}

inline
Instruction_Call *make_call(IR_Context *context, Value *target, const Array_Slice<Value *> &parameters = Array_Slice<Value *>()) {
    assert(target->value_type && target->value_type->type == Type::FUNCTION);
    auto func_type = target->value_type->as<Function_Type>();

    Instruction_Call *call = context->new_node<Instruction_Call>();
    call->call_target = target;
    call->value_type = func_type->result_type;
    target->uses++;

    if (parameters.size() < func_type->parameters.size())
        printf("Expected %zu arguments, got %zu\n", func_type->parameters.size(), parameters.size());

    if (!func_type->is_varargs && parameters.size() > func_type->parameters.size())
        printf("Expected %zu arguments, got %zu\n", func_type->parameters.size(), parameters.size());

    if (func_type->is_varargs) assert(parameters.size() >= func_type->parameters.size());
    else                                assert(parameters.size() == func_type->parameters.size());

    for (const auto v : parameters) {
        call->parameters.push_back(v);
        v->uses++;
    }

    for (u32 i = 0; i < func_type->parameters.size(); ++i) {
        auto vt = call->parameters[i]->value_type;
        auto ft = func_type->parameters[i];

        assert(types_match(vt, ft));
    }
    return call;
}

inline
Instruction_Return *make_return(IR_Context *context, Value *retval = nullptr) {
    Instruction_Return *ret = context->new_node<Instruction_Return>();
    ret->return_value = retval;
    if (retval) retval->uses++;
    return ret;
}

struct IR_Manager {
    Type *i8;
    Type *i16;
    Type *i32;
    Type *i64;

    Basic_Block *block = nullptr;
    IR_Context *context = nullptr;

    IR_Manager(IR_Context *_context) {
        context = _context;
        i8  = make_integer_type(1);
        i16 = make_integer_type(2);
        i32 = make_integer_type(4);
        i64 = make_integer_type(8);
    }

    void set_block(Basic_Block *block) {
        this->block = block;
    }

    Function *make_function(const String &name, Function_Type *func_type) {
        Function *func = context->node_storage.allocate<Function>();
        func->name = context->intern(name);
        func->value_type = func_type;
        return func;
    }

    Function *make_intrinsic(Function::Intrinsics id) {
        assert(id != Function::NOT_INTRINSIC);
        Function *func = context->node_storage.allocate<Function>();
        func->intrinsic_id = id;

        switch (id) {
            case Function::NOT_INTRINSIC:
            case Function::DEBUG_BREAK:
                func->name = context->intern("__debugbreak");
                func->value_type = make_func_type(make_void_type());
        }

        return func;
    }

    Instruction_Alloca *insert_alloca(Type *alloca_type, u32 array_size = 1) {
        auto _alloca = make_alloca(context, alloca_type, array_size);
        block->insert(_alloca);
        return _alloca;
    }

    Instruction_Load *insert_load(Value *pointer_value) {
        auto load = make_load(context, pointer_value);
        block->insert(load);
        return load;
    }

    Instruction_Store *insert_store(Value *source_value, Value *store_target) {
        auto store = make_store(context, source_value, store_target);
        block->insert(store);
        return store;
    }

    Value *insert_add(Value *lhs, Value *rhs) {
        Constant *clhs = is_constant(lhs);
        Constant *crhs = is_constant(rhs);

        if (clhs && crhs && clhs->is_integer() && crhs->is_integer()) {
            assert(types_match(lhs->value_type, rhs->value_type));
            assert(lhs->value_type->as<Integer_Type>());

            return make_integer_constant(context, clhs->integer_value + crhs->integer_value, lhs->value_type);
        }

        auto add = make_add(context, lhs, rhs);
        block->insert(add);
        return add;
    }

    Value *insert_sub(Value *lhs, Value *rhs) {
        Constant *clhs = is_constant(lhs);
        Constant *crhs = is_constant(rhs);

        if (clhs && crhs && clhs->is_integer() && crhs->is_integer()) {
            assert(types_match(lhs->value_type, rhs->value_type));
            assert(lhs->value_type->as<Integer_Type>());

            return make_integer_constant(context, clhs->integer_value - crhs->integer_value, lhs->value_type);
        }

        auto sub = make_sub(context, lhs, rhs);
        block->insert(sub);
        return sub;
    }

    Value *insert_mul(Value *lhs, Value *rhs) {
        Constant *clhs = is_constant(lhs);
        Constant *crhs = is_constant(rhs);

        if (clhs && crhs && clhs->is_integer() && crhs->is_integer()) {
            assert(types_match(lhs->value_type, rhs->value_type));
            assert(lhs->value_type->as<Integer_Type>());

            return make_integer_constant(context, clhs->integer_value * crhs->integer_value, lhs->value_type);
        }

        auto mul = make_mul(context, lhs, rhs);
        block->insert(mul);
        return mul;
    }

    Value *insert_div(Value *lhs, Value *rhs, bool signed_division) {
        Constant *clhs = is_constant(lhs);
        Constant *crhs = is_constant(rhs);

        if (clhs && crhs && clhs->is_integer() && crhs->is_integer()) {
            assert(types_match(lhs->value_type, rhs->value_type));
            assert(lhs->value_type->as<Integer_Type>());

            if (signed_division)
                // FIXME this should check the size of the incoming integer type and properly cast up if smaller than 64 bits.
                return make_integer_constant(context, static_cast<u64>((s64)clhs->integer_value / (s64)crhs->integer_value), lhs->value_type);
            else
                return make_integer_constant(context, clhs->integer_value / crhs->integer_value, lhs->value_type);
        }

        auto div = make_div(context, lhs, rhs, signed_division);
        block->insert(div);
        return div;
    }

    Instruction_Branch *insert_branch(Value *condition_or_null, Value *true_target, Value *failure_target = nullptr) {
        auto br = make_branch(context, condition_or_null, true_target, failure_target);
        block->insert(br);
        return br;
    }

    Instruction_Branch *insert_branch(Value *target) {
        return insert_branch(nullptr, target, nullptr);
    }

    Instruction_Call *insert_call(Function *func, const Array_Slice<Value *> &parameters = Array_Slice<Value *>()) {
        auto call = make_call(context, func, parameters);
        block->insert(call);
        return call;
    }

    Instruction_Return *insert_return(Value *retval = nullptr) {
        auto ret = make_return(context, retval);
        block->insert(ret);
        return ret;
    }

    Instruction_GEP *insert_gep(Value *pointer_value, Value *index) {
        auto gep = make_gep(context, pointer_value, {index});
        block->insert(gep);
        return gep;
    }

    Instruction_GEP *insert_gep(Value *pointer_value, const Array_Slice<Value *> &indices = Array_Slice<Value *>()) {
        auto gep = make_gep(context, pointer_value, indices);
        block->insert(gep);
        return gep;
    }
};

} // namespace josh

#endif
