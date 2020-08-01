#include <stdint.h>
#include <assert.h>
#include <stdio.h>

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
        reserve(amount);
        count = amount;
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
    INSTRUCTION_CALL,
    INSTRUCTION_RETURN,
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

struct Value {
    u32 type;
    Type *value_type;
};

struct Constant : Value {
    Constant() { type = VALUE_CONSTANT; }

    enum {
        STRING
    };

    u32 constant_type;

    String string_value;
};

Constant *make_string_constant(String value) {
    Constant *con = new Constant();
    con->constant_type = Constant::STRING;
    con->string_value = value;
    return con;
}

struct Function;

struct Instruction : Value {
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

struct Basic_Block {
    Array<Instruction *> instructions;
};

struct Global_Value : Constant {
    String name;
    u64 symbol_index = 0;
};

struct Function : Global_Value {
    Array<Type *> parameter_types;
    Array<Basic_Block *> blocks;
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

const u8 RAX = 0;
const u8 RBP = 5;
const u8 RDI = 7;

#define REX(W, R, X, B) (0b01000000 | (W << 3) | (R << 2) | (X << 1) | B)

#define ModRM(mod, reg, rm) ((mod << 6) | (reg << 3) | rm)

void emit_load_of_value(Linker_Object *object, Section *code_section, Section *data_section, Value *value, int _register) {
    if (value->type == VALUE_CONSTANT) {
        auto constant = static_cast<Constant *>(value);

        if (constant->constant_type == Constant::STRING) {
            auto data_sec_offset = data_section->data.size();

            // copy the string into the data section
            void *data_target = data_section->data.allocate(constant->string_value.length);
            memcpy(data_target, constant->string_value.data, constant->string_value.length);
            data_section->data.append_byte(0);


            // lea data-section-location(%rip), %reg
            code_section->data.append_byte(REX(1, 0, 0, 0));
            code_section->data.append_byte(0x8D + RAX);
            code_section->data.append_byte(ModRM(0b00, 0, 0b101));

            Relocation reloc;
            reloc.is_for_rip_call = false;
            reloc.is_rip_relative = true;
            reloc.offset = code_section->data.size();
            reloc.symbol_index = data_section->symbol_index;
            reloc.size = 4;

            code_section->relocations.add(reloc);

            u64 *value = (u64 *)code_section->data.allocate(4);
            *value = data_sec_offset;

            code_section->data.append_byte(REX(1, 0, 0, 0));
            code_section->data.append_byte(0x8B);
            code_section->data.append_byte(ModRM(0b11, _register, RAX));

        } else assert(false);
    }
}

void emit_instruction(Linker_Object *object, Section *code_section, Section *data_section, Instruction *inst) {
    switch (inst->type) {
        case INSTRUCTION_CALL: {
            auto call = static_cast<Instruction_Call *>(inst);

            emit_load_of_value(object, code_section, data_section, call->parameters[0], RDI);

            code_section->data.append_byte(REX(1, 0, 0, 0));
            code_section->data.append_byte(0xB8 + RAX);
            u64 *value = (u64 *)code_section->data.allocate(8);
            *value = 0;


            code_section->data.append_byte(REX(1, 0, 0, 0));
            code_section->data.append_byte(0xE8);
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

            break;
        }

        case INSTRUCTION_RETURN: {
            code_section->data.append_byte(REX(1, 0, 0, 1));
            code_section->data.append_byte(0x58 + RBP); // popq rbp

            code_section->data.append_byte(0xC3);
            break;
        }

        default: assert(false);
    }
}

void emit_function(Linker_Object *object, Section *code_section, Section *data_section, Function *function) {
    u32 symbol_index = get_symbol_index(object, function);
    Symbol *sym = &object->symbol_table[symbol_index];
    sym->is_function = true;
    sym->is_externally_defined = (function->blocks.count == 0);
    if (!sym->is_externally_defined) sym->section_number = code_section->section_number;
    sym->section_offset = code_section->data.size();

    code_section->data.append_byte(REX(1, 0, 0, 0));
    code_section->data.append_byte(0x50+RBP); // pushq rbp

    for (auto block : function->blocks) {
        for (auto inst : block->instructions) {
            emit_instruction(object, code_section, data_section, inst);
        }
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

    Instruction_Call *call = new Instruction_Call();
    call->call_target = printf_func;
    call->parameters.add(make_string_constant("Hello World\n"));

    block->instructions.add(call);
    block->instructions.add(new Instruction_Return());

    unit.functions.add(printf_func);
    unit.functions.add(main_func);


    emit_obj_file(&unit);

    return 0;
}

