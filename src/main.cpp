#include <stdint.h>
#include <assert.h>
#include <stdio.h>

#include <new>

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t  u8;

typedef int64_t  s64;
typedef int32_t  s32;
typedef int16_t  s16;
typedef int8_t   s8;

struct mach_header_64 {
    u32 magic;
    u32 cputype;
    u32 cpusubtype;
    u32 filetype;
    u32 ncmds;
    u32 sizeofcmds;
    u32 flags;
    u32 reserved;
};

struct load_command {
    
};


struct symtab_command {
    u32 cmd;
    u32 cmdsize;
    u32 symoff;
    u32 nsyms;
    u32 stroff;
    u32 strsize;
};

struct segment_command_64 {
    u32 cmd;
    u32 cmdsize;
    char segname[16];
    u64 vmaddr;
    u64 vmsize;
    u64 fileoff;
    u64 filesize;
    u32 maxprot;
    u32 initprot;
    u32 nsects;
    u32 flags;
};

struct section_64 {
    char sectname[16];
    char segname[16];
    u64 addr;
    u64 size;
    u32 offset;
    u32 align;
    u32 reloff;
    u32 nreloc;
    u32 flags;
    u32 reserved1;
    u32 reserved2;
    u32 reserved3;
};

struct relocation_info {
    u32 r_address;
    u32 r_info;
};

struct nlist_64 {
    u32 n_strx;
    u8  n_type;
    u8  n_sect;
    u16 n_desc;
    u64 n_value;
};

#define RELOC_LEN1 0
#define RELOC_LEN2 1
#define RELOC_LEN4 2
#define RELOC_LEN8 3

#define R_INFO(symbol_index, pcrel, len, ext, type) ((symbol_index & 0xFFFFFF) | (pcrel << 24) | (len << 25) | (ext << 27) | (type << 28))

const u32 MH_MAGIC_64 = 0xFEEDFACF;
const u32 MH_CIGAM_64 = 0xCFFAEDFE;

const u32 CPU_TYPE_x86_64  = 0x1000007;
const u32 CPU_TYPE_AArch64 = 0x100000B;

const u32 MH_OBJECT   = 1;
const u32 MH_EXECUTE  = 2;
const u32 MH_FVMLIB   = 3;
const u32 MH_CORE     = 4;
const u32 MH_PRELOAD  = 5;
const u32 MH_DYLIB    = 6;
const u32 MH_DYLINKER = 7;
const u32 MH_BUNDLE   = 8;

const u32 LC_SEGMENT         = 0x1;
const u32 LC_SYMTAB          = 0x2;
const u32 LC_SYMSEG          = 0x3;
const u32 LC_THREAD          = 0x4;
const u32 LC_UNIXTHREAD      = 0x5;
const u32 LC_LOADFVMLIB      = 0x6;
const u32 LC_IDFVMLIB        = 0x7;
const u32 LC_IDENT           = 0x8;
const u32 LC_FVMFILE         = 0x9;
const u32 LC_PREPAGE         = 0xA;
const u32 LC_DYSYMTAB        = 0xB;
const u32 LC_LOAD_DYLIB      = 0xC;
const u32 LC_ID_DYLIB        = 0xD;
const u32 LC_LOAD_DYLINKER   = 0xE;
const u32 LC_ID_DYLINKER     = 0xF;
const u32 LC_PREBOUND_DYLIB  = 0x10;
const u32 LC_SEGMENT_64      = 0x19;

const u32 X86_64_RELOC_UNSIGNED   = 0;
const u32 X86_64_RELOC_SIGNED     = 1;
const u32 X86_64_RELOC_BRANCH     = 2;
const u32 X86_64_RELOC_GOT_LOAD   = 3;
const u32 X86_64_RELOC_GOT        = 4;
const u32 X86_64_RELOC_SUBTRACTOR = 5;

const u8 N_EXT  = 0x01;
const u8 N_SECT = 0x0E;

const u8 NO_SECT = 0;

const u32 S_ATTR_PURE_INSTRUCTIONS = 0x80000000;

template <typename T>
struct Array {
    T *data = nullptr;
    u64 count    = 0;
    u64 reserved = 0;

    static const u64 DEFAULT_ARRAY_RESERVE_SIZE = 16;

    void reserve(u64 amount) {
        if (amount <= reserved) return;
        if (amount < DEFAULT_ARRAY_RESERVE_SIZE) amount = DEFAULT_ARRAY_RESERVE_SIZE;

        data     = (T *)realloc(data, amount*sizeof(T));
        reserved = amount;
    }

    void resize(u64 amount) {
        auto old_amount = count;
        reserve(amount);
        count = amount;

        for (; old_amount < count; ++old_amount) {
            new (data + old_amount) T();
        }
    }

    void add(T element) {
        if (count+1 >= reserved) reserve(reserved + DEFAULT_ARRAY_RESERVE_SIZE);

        data[count] = element;
        count += 1;
    }

    void reset() {
        free(data);
        data     = nullptr;
        count    = 0;
        reserved = 0;
    }

    T &operator[](u64 index) {
        assert(index < count);
        return data[index];
    }

    T *begin() {
        return &data[0];
    }

    T *end() {
        if (count) return &data[count];
        return nullptr;
    }
};

#include <stdlib.h>
#include <string.h>

struct String {
    char *data;
    u64   length;

    String(const char *s = nullptr) {
        data = const_cast<char *>(s);
        length = data ? strlen(data) : 0;
    }
};

struct Data_Buffer {
    struct Chunk {
        u8 *data      = nullptr;
        u64 count     = 0;
        u64 reserved  = 0;
    };

    Array<Chunk> chunks;

    Data_Buffer() {
        new_chunk();
    }

    Chunk *new_chunk(u64 size = 4096) {
        if (size < 4096) size = 4096;
        Chunk c;
        c.data     = (u8 *)malloc(size);
        c.count    = 0;
        c.reserved = size;

        chunks.add(c);
        return &chunks[chunks.count-1];
    }

    void append(void *src, u64 size) {
        Chunk *c = &chunks[chunks.count-1];
        if (c->count+size >= c->reserved) c = new_chunk(size);

        memcpy(c->data+c->count, src, size);
        c->count += size;
    }

    void *allocate(u64 size) {
        Chunk *c = &chunks[chunks.count-1];
        if (c->count+size >= c->reserved) c = new_chunk(size);

        void *result = c->data+c->count;
        c->count += size;
        return result;
    }

    void append(Data_Buffer *other) {
        for (auto &c : other->chunks) {
            append(c.data, c.count);
        }
    }

    void append_byte(u8 byte) {
        append(&byte, 1);
    }

    u64 size() {
        u64 size = 0;
        for (auto &c : chunks) {
            size += c.count;
        }
        return size;
    }
};

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
    Array<Section> sections;
    Array<String>  string_table;
    Array<Symbol>  symbol_table;
};

enum {
    VALUE_CONSTANT,

    INSTRUCTION_FIRST,
    
    INSTRUCTION_CALL = INSTRUCTION_FIRST,
    INSTRUCTION_RETURN,
    INSTRUCTION_ALLOCA,
    INSTRUCTION_STORE,
    INSTRUCTION_LOAD,

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
            claimer->result_spilled_onto_stack = 0;
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


            // lea data-section-location(%rip), %reg
            lea_rip_relative_into_reg64(&code_section->data, reg->machine_reg);

            Relocation reloc;
            reloc.is_for_rip_call = false;
            reloc.is_rip_relative = true;
            reloc.offset = code_section->data.size();
            reloc.symbol_index = data_section->symbol_index;
            reloc.size = 4;

            code_section->relocations.add(reloc);

            u64 *value = (u64 *)code_section->data.allocate(4);
            *value = data_sec_offset;

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

        case INSTRUCTION_CALL: {
            auto call = static_cast<Instruction_Call *>(inst);

            u8 index = 0;
            for (auto p : call->parameters) {
                u8 param_reg = get_next_system_v_abi_register(&index);
                u8 result = emit_load_of_value(object, function, code_section, data_section, p);

                if (result != param_reg) move_reg64_to_reg64(&code_section->data, result, param_reg);
            }

            // load number of floating point parameters into %al
            code_section->data.append_byte(REX(1, 0, 0, 0));
            code_section->data.append_byte(0xB8 + RAX);
            u64 *value = (u64 *)code_section->data.allocate(8);
            *value = 0;


            code_section->data.append_byte(REX(1, 0, 0, 0));
            code_section->data.append_byte(0xE8); // callq
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
    if (!sym->is_externally_defined) sym->section_number = code_section->section_number;
    sym->section_offset = code_section->data.size();

    function->register_usage.resize(RBX);

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

void emit_macho_file(Linker_Object *object) {
    Data_Buffer buffer;

    mach_header_64 *header = (mach_header_64 *)buffer.allocate(sizeof(mach_header_64));
    header->magic = MH_MAGIC_64;
    header->cputype = CPU_TYPE_x86_64;
    header->cpusubtype = 3;
    header->filetype = MH_OBJECT;
    header->ncmds = 2;
    // header->sizeofcmds =;
    header->flags    = 0x2000;
    header->reserved = 0;

    auto load_cmds_start = buffer.size();

    auto symtab_cmd = (symtab_command *)buffer.allocate(sizeof(symtab_command));
    symtab_cmd->cmd     = LC_SYMTAB;
    symtab_cmd->cmdsize = sizeof(symtab_command);
    // symtab_cmd->symoff = ;
    symtab_cmd->nsyms = object->symbol_table.count;
    // symtab_cmd->stroff  = ;
    // symtab_cmd->strsize = ;

    auto segment_cmd = (segment_command_64 *)buffer.allocate(sizeof(segment_command_64));
    segment_cmd->cmd     = LC_SEGMENT_64;
    segment_cmd->cmdsize = sizeof(segment_command_64) + sizeof(section_64)*object->sections.count;
    memset(segment_cmd->segname, 0, 16);
    // memcpy(segment_cmd->segname, "__TEXT", 6);

    segment_cmd->vmaddr = 0;
    segment_cmd->maxprot  = 0;
    segment_cmd->initprot = 0;
    segment_cmd->nsects   = object->sections.count;
    segment_cmd->flags = 0;

    for (auto &sect : object->sections) {
        section_64  *section = (section_64 *)buffer.allocate(sizeof(section_64));
        memset(section, 0, sizeof(section_64));

        sect.mach_section = section;

        memcpy(section->sectname, sect.name.data, sect.name.length);
        memcpy(section->segname,  sect.segment.data, sect.segment.length);
        section->size = sect.data.size();
        section->align = 0; // @TOOD
        section->nreloc = sect.relocations.count;
        section->flags = 0;

        if (sect.is_pure_instructions) section->flags |= S_ATTR_PURE_INSTRUCTIONS | 0x400;

        section->reserved1 = 0;
        section->reserved2 = 0;
        section->reserved3 = 0;
    }

    header->sizeofcmds = buffer.size()-load_cmds_start;

    segment_cmd->fileoff = buffer.size();

    u64 addr = 0;

    for (auto &sect : object->sections) {
        section_64 *section = (section_64 *)sect.mach_section;

        section->addr = addr;
        section->offset = buffer.size();

        buffer.append(&sect.data);

        addr += section->size;

        section->reloff = buffer.size();
        section->nreloc = sect.relocations.count;

        for (auto &reloc : sect.relocations) {
            relocation_info *info = (relocation_info *)buffer.allocate(sizeof(relocation_info));
            info->r_address = reloc.offset;

            u32 size = 0;
            if      (reloc.size == 1) size = RELOC_LEN1;
            else if (reloc.size == 2) size = RELOC_LEN2;
            else if (reloc.size == 4) size = RELOC_LEN4;
            else if (reloc.size == 8) size = RELOC_LEN8;
            else assert(false);

            u32 pcrel = 0;
            if (reloc.is_for_rip_call || reloc.is_rip_relative) pcrel = 1;

            u32 type = X86_64_RELOC_UNSIGNED;
            if      (reloc.is_for_rip_call) type = X86_64_RELOC_BRANCH;
            else if (reloc.is_rip_relative) type = X86_64_RELOC_SIGNED;

            info->r_info = R_INFO(reloc.symbol_index, pcrel, size, 1, type);
        }
    }

    segment_cmd->filesize =  buffer.size() - segment_cmd->fileoff;
    segment_cmd->vmsize = segment_cmd->filesize; // @TODO

    Data_Buffer string_buffer;
    string_buffer.append_byte(0);

    symtab_cmd->symoff = buffer.size();

    for (auto &symbol : object->symbol_table) {
        auto sym = (nlist_64 *)buffer.allocate(sizeof(nlist_64));

        if (symbol.linkage_name.length) {
            sym->n_strx = string_buffer.size();
            if (!symbol.is_section) string_buffer.append_byte('_');
            string_buffer.append(symbol.linkage_name.data, symbol.linkage_name.length);
            string_buffer.append_byte(0);
        } else {
            sym->n_strx = 0;
        }

        if (symbol.is_externally_defined) {
            sym->n_type = N_EXT;
            sym->n_sect = NO_SECT;
        } else {
            sym->n_type = 0;
            if (symbol.section_number)        sym->n_type |= N_SECT;
            if (symbol.is_externally_visible) sym->n_type |= N_EXT;
            sym->n_sect = symbol.section_number;
        }

        sym->n_desc = 0;

        u64 section_addr = 0;
        if (symbol.section_number) section_addr = ((section_64 *)object->sections[symbol.section_number-1].mach_section)->addr;
        sym->n_value = symbol.section_offset + section_addr;
    }

    symtab_cmd->stroff = buffer.size();
    symtab_cmd->strsize = string_buffer.size();

    buffer.append(&string_buffer);

    FILE *file = fopen("test.o", "wb");
    for (auto &c : buffer.chunks) {
        int amount = c.count;
        do {
            int written = fwrite(c.data, 1, amount, file);
            amount -= written;
        } while (amount > 0);
    }
    fclose(file);
}

void emit_obj_file(Compilation_Unit *unit) {
    Linker_Object object = {};

    u32 data_sec_index = 0;
    u32 text_sec_index = 0;

    {
        Section data_sec = {};
        data_sec.name = "__data";
        data_sec.segment = "__DATA";
        data_sec.section_number = object.sections.count+1;
        data_sec.symbol_index = object.symbol_table.count;

        Symbol sym;
        sym.linkage_name = "__data";
        sym.section_number = data_sec.section_number;
        sym.section_offset = 0;
        sym.is_externally_defined = false;
        sym.is_function = false;
        sym.is_section  = true;

        object.symbol_table.add(sym);

        data_sec_index = data_sec.section_number-1;

        object.sections.add(data_sec);
    }

    {
        Section text_sec = {};
        text_sec.name = "__text";
        text_sec.segment = "__TEXT";
        text_sec.section_number = object.sections.count+1;
        text_sec.symbol_index = object.symbol_table.count;
        text_sec.is_pure_instructions = true;

        Symbol sym;
        sym.linkage_name = "__text";
        sym.section_number = text_sec.section_number;
        sym.section_offset = 0;
        sym.is_externally_defined = false;
        sym.is_function = false;
        sym.is_section  = true;

        object.symbol_table.add(sym);

        text_sec_index = text_sec.section_number-1;

        object.sections.add(text_sec);
    }

    for (auto func : unit->functions) {
        emit_function(&object, &object.sections[text_sec_index], &object.sections[data_sec_index], func);
    }


    emit_macho_file(&object);
}


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

    Instruction_Call *call = new Instruction_Call();
    call->call_target = printf_func;
    call->parameters.add(make_string_constant("Hello World: %d\n"));
    call->parameters.add(load);

    block->instructions.add(call);
    block->instructions.add(new Instruction_Return());

    unit.functions.add(printf_func);
    unit.functions.add(main_func);


    emit_obj_file(&unit);

    return 0;
}

