#include "general.h"
#include "linker_object.h"
#include "ir.h"
#include "x64.h"

using namespace josh;

#include <stdio.h>

#define REG_WIDTH 8

enum Integer_Register : u8
{
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8  = 8,
    R9  = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
};

enum XMM_Register : u8
{
    XMM0  = 0,
    XMM1  = 1,
    XMM2  = 2,
    XMM3  = 3,
    XMM4  = 4,
    XMM5  = 5,
    XMM6  = 6,
    XMM7  = 7,
    XMM8  = 8,
    XMM9  = 9,
    XMM10 = 10,
    XMM11 = 11,
    XMM12 = 12,
    XMM13 = 13,
    XMM14 = 14,
    XMM15 = 15,
};

struct Address_Info {
    u8 machine_reg = 0xFF;
    s32 disp       = 0;

    u8 base_reg    = 0xFF;
    u8 index_reg   = 0xFF; // if index_reg != 0xFF then we use SIB
    u8 scale       = 1;    // must be 1, 2, 4, or 8
};

Address_Info addr_register_disp(u8 machine_reg, s32 disp = 0) {
    Address_Info info;
    info.machine_reg = machine_reg;
    info.disp        = disp;
    return info;
}

// This magic is best explained on OSDev https://wiki.osdev.org/X86-64_Instruction_Encoding
// REX prefixes the instruction, ModRM seems to post-fix the instruction but before data operands,
// and both seem to be optional... and the processor magically understands these things and parses
// instructions correctly ????
#define REX(W, R, X, B) ((u8)(0x40 | ((W) << 3) | ((R) << 2) | ((X) << 1) | (B)))
#define ModRM(mod, reg, rm) ((u8)(((mod) << 6) | ((reg) << 3) | (rm)))
#define SIB(scale, index, base) ((u8)(((scale) << 6) | ((index) << 3) | (base)))

#define BIT3(v) ((v & (1 << 3)) >> 3)
#define LOW3(v) (v & 0x7)

// Despite the name, if RBP or R14 (LOW3() bits are 0b101)
// then there is a 32-bit displacement anyways.
const u8 MOD_INDIRECT_NO_DISP    = 0x0;
const u8 MOD_INDIRECT_8BIT_DISP  = 0x1;
const u8 MOD_INDIRECT_32BIT_DISP = 0x2;
const u8 MOD_DIRECT              = 0x3;

static
u8 get_exponent(u8 i) {
    u8 shift = 0;
    while (!((i >> shift) & 1)) shift += 1;
    return shift;
}

void _single_register_operand_instruction(Data_Buffer *dataptr, u8 opcode, u8 reg, u32 size) {
    if (size == 2) dataptr->append_byte(0x66);
    dataptr->append_byte(REX((size == 8) ? 1 : 0, 0, 0, BIT3(reg)));

    dataptr->append_byte(opcode + LOW3(reg));
}

bool is_RSP_thru_RDI(u8 reg) {
    return reg >= RSP && reg <= RDI;
}

s32 *_two_register_operand_instruction(Data_Buffer *dataptr, u8 opcode, bool is_direct_register_use, u8 operand1, Address_Info operand2, u32 size, bool _0F_op = false) {
    u8 mod = MOD_INDIRECT_32BIT_DISP;
    if       (is_direct_register_use) mod = MOD_DIRECT;
    else {
        if (operand2.disp == 0) mod = MOD_INDIRECT_NO_DISP;
        else if (fits_into<s8>(operand2.disp)) {
            mod = MOD_INDIRECT_8BIT_DISP;
            operand2.disp = (s8) operand2.disp;
        }

        // preserve [base + (index * s) + disp] form since MOD_INDIRECT_NO_DISP
        // takes the form [index * s] when RBP is used.
        if (operand2.machine_reg == RSP && operand2.base_reg == RBP && operand2.disp == 0) {
            mod = MOD_INDIRECT_8BIT_DISP;
        }
    }

    if (size == 2) dataptr->append_byte(0x66);

    u8 op2_reg = operand2.machine_reg;
    u8 index_reg = 0;
    if (mod != MOD_DIRECT && LOW3(op2_reg) == RSP) {
        op2_reg   = operand2.base_reg;
        index_reg = operand2.index_reg;

        if (mod == MOD_INDIRECT_NO_DISP) assert(!(op2_reg >= RSP && op2_reg < R8));
    }

    // If we don't need to extend any of the register bits and we can operator in 32/16/8 bit modes,
    // then we can omit the REX byte. 
    bool need_rex = false;
    if (size == 8 || ((BIT3(operand1) | BIT3(index_reg) | BIT3(op2_reg)) != 0)) need_rex = true;
    // If we're using 8-bit mode, we have to use REX if we want to use registers RSP through RDI
    // otherwise, the processor uses the 8-bit high registers of AH through DH.
    else if (size == 1 && (is_RSP_thru_RDI(operand1) || is_RSP_thru_RDI(index_reg) || is_RSP_thru_RDI(op2_reg))) need_rex = true;
    
    if (need_rex) dataptr->append_byte(REX((size == 8) ? 1 : 0, BIT3(operand1), BIT3(index_reg), BIT3(op2_reg)));

    if (_0F_op) dataptr->append_byte(0x0F);
    dataptr->append_byte(opcode);
    dataptr->append_byte(ModRM(mod, LOW3(operand1), LOW3(operand2.machine_reg)));

    if (mod != MOD_DIRECT && LOW3(operand2.machine_reg) == RSP) {
        if (mod == MOD_INDIRECT_NO_DISP) assert(LOW3(op2_reg) != RBP);

        u8 scale = get_exponent(operand2.scale);
        assert(scale <= 0x3);
        dataptr->append_byte(SIB(scale, LOW3(index_reg), LOW3(op2_reg)));
    }

    if (mod == MOD_INDIRECT_8BIT_DISP) {
        s8 *disp = dataptr->allocate_unaligned<s8>();
        *disp = trunc<s8>(operand2.disp);
        return nullptr;
    }

    if (mod == MOD_INDIRECT_32BIT_DISP || (mod == MOD_INDIRECT_NO_DISP && LOW3(op2_reg) == RBP)) {
        s32 *disp = dataptr->allocate_unaligned<s32>();
        *disp = operand2.disp;
        return disp;
    }

    return nullptr;
}

void _move_bidrectional(Data_Buffer *dataptr, u8 value_reg, Address_Info info, u32 size, bool to_memory) {
    u8 op = (size == 1) ? 0x88 : 0x89;
    if (!to_memory) op += 0x02;

    _two_register_operand_instruction(dataptr, op, false, value_reg, info, size);
}

s32 *_move_float_bidirectional(Data_Buffer *dataptr, u8 value_reg, Address_Info info, u32 size, bool to_memory) {
    assert(size == 4 || size == 8);

    u8 op = 0x10;
    if (to_memory) op += 0x01;

    if (size == 4) dataptr->append_byte(0xF3);
    else           dataptr->append_byte(0xF2);
    return _two_register_operand_instruction(dataptr, op, false, value_reg, info, size, true);
}

void move_xmm_to_memory(Data_Buffer *dataptr, u8 src, Address_Info info, u32 size) {
    _move_float_bidirectional(dataptr, src, info, size, true);
}

s32 *move_memory_to_xmm(Data_Buffer *dataptr, u8 dst, Address_Info info, u32 size) {
    return _move_float_bidirectional(dataptr, dst, info, size, false);
}

void move_xmm_to_xmm(Data_Buffer *dataptr, u8 src, u8 dst) {
    u32 size = 8;
    u8 op = 0x10;

    if (size == 4) dataptr->append_byte(0xF3);
    else           dataptr->append_byte(0xF2);
    _two_register_operand_instruction(dataptr, op, true, dst, addr_register_disp(src), size, true);
}

//66 REX.W 0F 6E /r MOVQ xmm, r/m64

void movq_xmm_to_reg(Data_Buffer *dataptr, u8 src, u8 dst, u32 size) {
    assert(size == 4 || size == 8);

    // For whatever reason, this instruction is always prefixed with 0x66 despite not supporting
    // 16-bit float moves.
    dataptr->append_byte(0x66);

    _two_register_operand_instruction(dataptr, 0x7E, true, src, addr_register_disp(dst), size, true);
}

void movq_reg_to_xmm(Data_Buffer *dataptr, u8 src, u8 dst, u32 size) {
    assert(size == 4 || size == 8);

    // For whatever reason, this instruction is always prefixed with 0x66 despite not supporting
    // 16-bit float moves.
    dataptr->append_byte(0x66);

    _two_register_operand_instruction(dataptr, 0x6E, true, src, addr_register_disp(dst), size, true);
}

void move_reg64_to_reg64(Data_Buffer *dataptr, u8 src, u8 dst) {
    _two_register_operand_instruction(dataptr, 0x8B, true, dst, addr_register_disp(src), 8);
}

void move_reg_to_memory(Data_Buffer *dataptr, u8 src, Address_Info info, u32 size) {
    _move_bidrectional(dataptr, src, info, size, true);
}

void move_memory_to_reg(Data_Buffer *dataptr, u8 dst, Address_Info info, u32 size) {
    _move_bidrectional(dataptr, dst, info, size, false);
}

// RCX = count
// RDI = dst
// RSI = src
void rep_move_string(Data_Buffer *dataptr, u32 size) {
    if (size == 2) dataptr->append_byte(0x66);

    u8 op = (size == 1) ? 0xA4 : 0xA5;

    dataptr->append_byte(0xF3);
    dataptr->append_byte(REX((size == 8) ? 1 : 0, 0, 0, 0));
    dataptr->append_byte(op);
}

u64 *move_imm64_to_reg64(Data_Buffer *dataptr, u64 value, u8 reg, u32 size = 8) {
    u8 op = (size == 1) ? 0xB0 : 0xB8;

    _single_register_operand_instruction(dataptr, op, reg, size);
    
    u8 *operand = (u8 *)dataptr->allocate_bytes_unaligned(size);
    
    if (size == 1) *       operand = trunc<u8> (value);
    if (size == 2) *(u16 *)operand = trunc<u16>(value);
    if (size == 4) *(u32 *)operand = trunc<u32>(value);
    if (size == 8) *(u64 *)operand = value;

    return (u64 *)operand;
}

void move_imm32_sext_to_memory(Data_Buffer *dataptr, s32 value, Address_Info info, u32 size) {
    //FIXME the name of this function is misleading because only the size = 8 variant does sign extension
    // We can also save a byte if size = 4 I think, since we don't need to REX prefix unless we need BIT3(reg) registers
    u8 op = (size == 1) ? 0xC6 : 0xC7;
    _two_register_operand_instruction(dataptr, op, false, RAX, info, size);

    if (size > 4) size = 4;
    s8 *operand = (s8 *)dataptr->allocate_bytes_unaligned(size);
    
    if (size == 1) *       operand = trunc<s8> (value);
    if (size == 2) *(s16 *)operand = trunc<s16>(value);
    if (size == 4 || size == 8) *(s32 *)operand = value;
}

void move_memory_to_reg_zero_ext(Data_Buffer *dataptr, u8 value_reg, Address_Info info, u32 size) {
    if (size <= 2) {
        u8 op = (size == 1) ? 0xB6 : 0xB7;

        size = 4; // always load into 32-bit register
        _two_register_operand_instruction(dataptr, op, false, value_reg, info, size, true);
    } else {
        move_memory_to_reg(dataptr, value_reg, info, size);
    }
}

void move_reg_to_reg_zero_ext(Data_Buffer *dataptr, u8 src, u8 dst, u32 srcsize, u32 dstsize) {
    assert(srcsize <= 2);
    assert(dstsize >= 2);
    assert(dstsize > srcsize);

    u8 op = (srcsize == 1) ? 0xB6 : 0xB7;

    _two_register_operand_instruction(dataptr, op, true, dst, addr_register_disp(src), dstsize, true);
}

void move_imm_to_reg(Data_Buffer *dataptr, u64 value, u8 reg) {
    if (false && fits_into<u8>(value)) { // This doesn't actually save us any more instruction bytes over the u32 variant, and actually introduces more bytes
        move_imm64_to_reg64(dataptr, value, reg, 1);
        move_reg_to_reg_zero_ext(dataptr, reg, reg, 1, 8);
    } else if (false && fits_into<u16>(value)) { // This doesn't actually save us any more instruction bytes over the u32 variant
        move_imm64_to_reg64(dataptr, value, reg, 2);
        move_reg_to_reg_zero_ext(dataptr, reg, reg, 2, 8);
    } else if (fits_into<u32>(value))
        move_imm64_to_reg64(dataptr, value, reg, 4);
    else
        move_imm64_to_reg64(dataptr, value, reg, 8);
}

void xor_reg64_to_reg64(Data_Buffer *dataptr, u8 src, u8 dst, u32 size);
// same as move_imm64_to_reg64, but we are allowed to clear the register using xor if 0 or try to stuff the value
// into as small a register as possible then zext if needed
void move_imm_to_reg_or_clear(Data_Buffer *dataptr, u64 value, u8 reg) {
    if (value)
        move_imm_to_reg(dataptr, value, reg);
    else
        xor_reg64_to_reg64(dataptr, reg, reg, 4);
}


// Need to allocate 4 bytes of space after this call
s32 *lea_rip_relative_into_reg64(Data_Buffer *dataptr, u8 reg) {
    return _two_register_operand_instruction(dataptr, 0x8D, false, reg, addr_register_disp(RBP), 8);
}

void lea_into_reg64(Data_Buffer *dataptr, u8 dst, Address_Info source) {
    _two_register_operand_instruction(dataptr, 0x8D, false, dst, source, 8);
}

void _push_pop_impl(Data_Buffer *dataptr, u8 opcode, u8 reg, u8 size) {
    if (size == REG_WIDTH && BIT3(reg) == 0) // optimal path, one instruction push/pop
        dataptr->append_byte(opcode + LOW3(reg));
    else
        _single_register_operand_instruction(dataptr, 0x58, reg, size);
}

void pop(Data_Buffer *dataptr, u8 reg, u8 size = 8) {
    _push_pop_impl(dataptr, 0x58, reg, size);
}

void push(Data_Buffer *dataptr, u8 reg, u8 size = 8) {
    _push_pop_impl(dataptr, 0x50, reg, size);
}

s32 *_mathop_imm32_reg64(Data_Buffer *dataptr, u8 reg, s32 value, u32 size, u8 op_select) {
    u8 op = (size == 1) ? 0x80 : 0x81;
    _two_register_operand_instruction(dataptr, op, true, op_select, addr_register_disp(reg), size);

    if (size > 4) size = 4;
    u8 *operand = (u8 *)dataptr->allocate_bytes_unaligned(size);
    
    if (size == 1) *       operand = static_cast<u8> (value);
    if (size == 2) *(u16 *)operand = static_cast<u16>(value);
    if (size == 4 || size == 8) *(s32 *)operand = value;

    return (s32 *)operand;
}

const u8 MATH_OP_SELECT_SUB = 5;
const u8 MATH_OP_SELECT_ADD = 0;

s32 *sub_imm32_from_reg64(Data_Buffer *dataptr, u8 reg, s32 value, u32 size) {
    return _mathop_imm32_reg64(dataptr, reg, value, size, MATH_OP_SELECT_SUB);
}

s32 *add_imm32_to_reg64(Data_Buffer *dataptr, u8 reg, s32 value, u32 size) {
    return _mathop_imm32_reg64(dataptr, reg, value, size, MATH_OP_SELECT_ADD);
}

void imul_reg64_with_imm32(Data_Buffer *dataptr, u8 reg, s32 value, u32 size) {
    if (size < 2) size = 2; // @FixMe maybe, imul in this form does not support 1-byte multiplication

    u8 op = 0x69;
    _two_register_operand_instruction(dataptr, op, true, reg, addr_register_disp(reg), size);

    if (size > 4) size = 4;
    s8 *operand = (s8 *)dataptr->allocate_bytes_unaligned(size);

    if (size == 2) *(s16 *)operand = static_cast<s16>(value);
    if (size == 4 || size == 8) *(s32 *)operand = value;
}

void imul_reg64_with_reg64(Data_Buffer *dataptr, u8 src, u8 dst, u32 size) {
    if (size < 2) size = 2; // @FixMe maybe, imul in this form does not support 1-byte multiplication

    u8 op = 0xAF;
    _two_register_operand_instruction(dataptr, op, true, dst, addr_register_disp(src), size, true);
}

void add_reg64_to_reg64(Data_Buffer *dataptr, u8 src, u8 dst, u32 size) {
    u8 op = (size == 1) ? 0x00 : 0x01;
    _two_register_operand_instruction(dataptr, op, true, src, addr_register_disp(dst), size);
}

void xor_reg64_to_reg64(Data_Buffer *dataptr, u8 src, u8 dst, u32 size) {
    u8 op = (size == 1) ? 0x32 : 0x33;
    _two_register_operand_instruction(dataptr, op, true, src, addr_register_disp(dst), size);
}

void div_reg64_with_rax(Data_Buffer *dataptr, u8 src, u32 size, bool signed_division) {
    u8 op = (size == 1) ? 0xF6 : 0xF7;
    u8 version = 6; // unsigned
    if (signed_division) version = 7;
    _two_register_operand_instruction(dataptr, op, true, version, addr_register_disp(src), size);
}

void sub_reg64_from_reg64(Data_Buffer *dataptr, u8 src, u8 dst, u32 size) {
    u8 op = (size == 1) ? 0x28 : 0x29;
    _two_register_operand_instruction(dataptr, op, true, src, addr_register_disp(dst), size);
}

void debugbreak(Data_Buffer *dataptr) {
    dataptr->append_byte(0xCC);
}

void cwd_cdq(Data_Buffer *dataptr, u32 size) {
    // 1 byte sign extension not supported, it seems this is because 1-byte
    // division uses the entire 16-bit AX register instead of using DL:AL together
    assert(size >= 2);
    _single_register_operand_instruction(dataptr, 0x99, RAX, size);
}

void move_memory_value_to_register(Data_Buffer *dataptr, u8 value_reg, Address_Info info, Type *type) {
    if (type->type == Type::FLOAT)
        move_memory_to_xmm(dataptr, value_reg, info, type->size);
    else if (auto str = type->as<Struct_Type>()) // Struct types are tracked by memory reference for now
        move_memory_to_reg_zero_ext(dataptr, value_reg, info, 8 /*PointerSize TargetInfo*/);
    else
        move_memory_to_reg_zero_ext(dataptr, value_reg, info, type->size);
}

void move_register_value_to_memory(Data_Buffer *dataptr, u8 value_reg, Address_Info info, Type *type) {
    if (type->type == Type::FLOAT)
        move_xmm_to_memory(dataptr, value_reg, info, type->size);
    else if (auto str = type->as<Struct_Type>()) // Struct types are tracked by memory reference for now
        move_reg_to_memory(dataptr, value_reg, info, 8 /*PointerSize TargetInfo*/);
    else
        move_reg_to_memory(dataptr, value_reg, info, type->size);
}

Register *get_free_register(X64_Emitter *emitter) {
    for (auto &reg : emitter->register_usage) {
        if (reg.is_free) {
            reg.is_free = false;
            return &reg;
        }
    }

    return nullptr;
}

Register *get_free_xmm_register(X64_Emitter *emitter) {
    for (auto &reg : emitter->xmm_usage) {
        if (reg.is_free) {
            reg.is_free = false;
            return &reg;
        }
    }

    return nullptr;
}

void maybe_spill_register(X64_Emitter *emitter, Register *reg) {
    if (reg->currently_holding_result_of_instruction) {
        auto inst = reg->currently_holding_result_of_instruction;
        inst->result_stored_in = nullptr;

        if (inst->result_spilled_onto_stack == 0 && inst->uses) {
            emitter->stack_size += 8; // @TargetInfo
            inst->result_spilled_onto_stack = -emitter->stack_size;
            move_register_value_to_memory(&emitter->function_buffer, reg->machine_reg, addr_register_disp(RBP, inst->result_spilled_onto_stack), inst->value_type);
        }

        reg->currently_holding_result_of_instruction = nullptr;
    }

    reg->is_free = true;
}

Register *claim_register(X64_Emitter *emitter, Register *reg, Value *claimer) {
    maybe_spill_register(emitter, reg);

    if (claimer) {
        reg->currently_holding_result_of_instruction = claimer;
        claimer->result_stored_in = reg;
    }

    reg->is_free = false;
    reg->used    = true;
    return reg;
}

void free_register(Register *reg) {
    reg->currently_holding_result_of_instruction = nullptr;
    reg->is_free = true;
}

Register *get_free_or_suggested_register(X64_Emitter *emitter, u8 suggested_register, bool force_use_suggested, Value *inst) {
    Register *reg = nullptr;

    if (!force_use_suggested) {
        reg = get_free_register(emitter);
        if (!reg) {

            u32 uses = U32_MAX;
            Register *regi;
            for (auto &reg : emitter->register_usage) {
                if (reg.machine_reg == RSP || reg.machine_reg == RBP) continue;

                if (reg.currently_holding_result_of_instruction) {
                    auto inst = reg.currently_holding_result_of_instruction;

                    if (inst->uses < uses) {
                        uses = inst->uses;
                        regi = &reg;
                    }
                }
            }

            reg = regi;
        }
    }

    if (!reg) reg = &emitter->register_usage[suggested_register];

    maybe_spill_register(emitter, reg);
    if (inst) return claim_register(emitter, reg, inst);

    
    return reg;
}

u8 load_instruction_result(X64_Emitter *emitter, Value *inst, u8 suggested_register, bool force_use_suggested) {
    if (auto reg = inst->result_stored_in) {
        assert(reg->currently_holding_result_of_instruction == inst);
        return reg->machine_reg;
    } else {
        assert(!inst->result_stored_in);
        reg = get_free_or_suggested_register(emitter, suggested_register, force_use_suggested, inst);

        assert(inst->result_spilled_onto_stack != 0);
        move_memory_value_to_register(&emitter->function_buffer, reg->machine_reg, addr_register_disp(RBP, inst->result_spilled_onto_stack), inst->value_type);
        return reg->machine_reg;
    }
}


u8 emit_load_of_value(X64_Emitter *emitter, Linker_Object *object, Section *code_section, Value *value, u8 suggested_register = RAX, bool force_use_suggested = false) {
    if (value->type == VALUE_CONSTANT) {
        auto constant = static_cast<Constant *>(value);

        Register *reg = get_free_or_suggested_register(emitter, suggested_register, force_use_suggested, nullptr);
        reg->is_free = false;

        Section *data_section = emitter->data_section;

        if (constant->constant_type == Constant::STRING) {
            u32 data_sec_offset = data_section->data.size();

            // copy the string characters into the data section
            assert(constant->string_value.length() <= U32_MAX);

            if (auto str_id = emitter->string_table.intern(constant->string_value)) {
                auto &entry = emitter->string_table.lookup(str_id);
                if (entry.data_sec_offset == U32_MAX) { // New entry
                    size_t length = static_cast<u32>(constant->string_value.length());
                    void *data_target = data_section->data.allocate_bytes_unaligned(length);
                    memcpy(data_target, constant->string_value.data(), length);
                    data_section->data.append_byte(0);
                    entry.data_sec_offset = data_sec_offset;
                } else {
                    data_sec_offset = entry.data_sec_offset;
                }
            } else {
                // Empty string, assume at data section offset 0.
                data_sec_offset = 0;
            }

            if (object->use_absolute_addressing) {
                move_imm64_to_reg64(&emitter->function_buffer, data_sec_offset, reg->machine_reg, 8);
    
                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.offset = emitter->function_buffer.size() - 8;
                reloc.symbol_index = data_section->symbol_index;
                reloc.size = 8;
                reloc.addend = static_cast<u64>(data_sec_offset);

                code_section->relocations.push_back(reloc);
            } else {
                // lea data-section-location(%rip), %reg
                s32 *value = lea_rip_relative_into_reg64(&emitter->function_buffer, reg->machine_reg);
                *value = static_cast<s32>(data_sec_offset);

                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.is_rip_relative = true;
                reloc.offset = emitter->function_buffer.size() - 4;
                reloc.symbol_index = data_section->symbol_index;
                reloc.size = 4;

                code_section->relocations.push_back(reloc);
            }

            // if (_register != RAX) move_reg64_to_reg64(&emitter->function_buffer, RAX, _register);
        } else if (constant->constant_type == Constant::INTEGER) {
            move_imm_to_reg_or_clear(&emitter->function_buffer, constant->integer_value, reg->machine_reg);
        } else if (constant->constant_type == Constant::FLOAT) {
            Register *xmm = get_free_xmm_register(emitter);
            if (!xmm) {
                xmm = claim_register(emitter, &emitter->xmm_usage[XMM0], value);
            } else {
                xmm = claim_register(emitter, xmm, value);
            }

            // copy the float bytes into the data section
            if (constant->value_type->size == 8) {
                double *data_target = data_section->data.allocate<double>();
                *data_target = constant->float_value;
            } else if (constant->value_type->size == 4) {
                float *data_target = data_section->data.allocate<float>();
                *data_target = (float)constant->float_value;
            } else {
                assert(false);
            }

            u32 data_sec_offset = data_section->data.size() - constant->value_type->size;
            assert(data_sec_offset >= 0);

            if (object->use_absolute_addressing) {
                move_imm64_to_reg64(&emitter->function_buffer, data_sec_offset, reg->machine_reg, 8);
    
                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.offset = emitter->function_buffer.size() - 8;
                reloc.symbol_index = data_section->symbol_index;
                reloc.size = 8;
                reloc.addend = static_cast<u64>(data_sec_offset);

                code_section->relocations.push_back(reloc);

                move_memory_to_xmm(&emitter->function_buffer, xmm->machine_reg, addr_register_disp(reg->machine_reg), constant->value_type->size);
            } else {
                s32 *value = move_memory_to_xmm(&emitter->function_buffer, xmm->machine_reg, addr_register_disp(RBP), constant->value_type->size);
                *value = static_cast<s32>(data_sec_offset);

                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.is_rip_relative = true;
                reloc.offset = emitter->function_buffer.size() - 4;
                reloc.symbol_index = data_section->symbol_index;
                reloc.size = 4;

                code_section->relocations.push_back(reloc);
            }

            free_register(reg);
            return xmm->machine_reg;
        }

        return reg->machine_reg;
    } else if (value->type == VALUE_BASIC_BLOCK) {
        Basic_Block *block = static_cast<Basic_Block *>(value);

        Register *reg = get_free_or_suggested_register(emitter, suggested_register, force_use_suggested, nullptr);
        reg->is_free = false;

        s32 *value = lea_rip_relative_into_reg64(&emitter->function_buffer, reg->machine_reg);

        auto offset = emitter->function_buffer.size() - 4;
        block->text_locations_needing_addr_fixup.push_back(offset);

        block->text_ptrs_for_fixup.push_back((u32 *)value);
        return reg->machine_reg;
    } else if (value->type == INSTRUCTION_ALLOCA) {
        auto _alloca = static_cast<Instruction_Alloca *>(value);

        if (_alloca->result_stored_in) return _alloca->result_stored_in->machine_reg;
        if (_alloca->result_spilled_onto_stack != 0) return load_instruction_result(emitter, _alloca, suggested_register, force_use_suggested);

        Register *reg = get_free_or_suggested_register(emitter, suggested_register, force_use_suggested, _alloca);

        assert(_alloca->stack_offset != 0);
        lea_into_reg64(&emitter->function_buffer, reg->machine_reg, addr_register_disp(RBP, _alloca->stack_offset));
        return reg->machine_reg;
    } else if (value->type == VALUE_ARGUMENT) {
        auto arg = static_cast<Argument *>(value);
        if (arg->copied_to_stack_offset) {
            Address_Info info = addr_register_disp(RBP, arg->copied_to_stack_offset);
            Register *reg = get_free_or_suggested_register(emitter, suggested_register, force_use_suggested, nullptr);
            lea_into_reg64(&emitter->function_buffer, reg->machine_reg, info);
            return reg->machine_reg;
        }

        return load_instruction_result(emitter, value, suggested_register, force_use_suggested);
    } else if (value->type >= INSTRUCTION_FIRST && value->type <= INSTRUCTION_LAST) {
        auto inst = static_cast<Instruction *>(value);
        return load_instruction_result(emitter, inst, suggested_register, force_use_suggested);
    }

    assert(false);
    return 0;
}

u8 maybe_get_instruction_register(Value *value) {
    if (Instruction *inst = is_instruction(value)) {
        if (inst->result_stored_in) return inst->result_stored_in->machine_reg;
    }

    return 0xFF;
}

Address_Info get_address_value_of_pointer(X64_Emitter *emitter, Linker_Object *object, Section *code_section, Value *value, u8 suggested_register = RAX) {
    assert(value->value_type->type == Type::POINTER);

    if (value->type == INSTRUCTION_ALLOCA) {
        auto _alloca = static_cast<Instruction_Alloca *>(value);
        return addr_register_disp(RBP, _alloca->stack_offset);
    }
    else if (value->type == VALUE_ARGUMENT) {
        auto arg = static_cast<Argument *>(value);
        if (arg->copied_to_stack_offset)
            return addr_register_disp(RBP, arg->copied_to_stack_offset);
    }

// We could theoretically do this and save some instructions
// but this ends up generating a large amount of bytes
// due to having to write the displacement bytes. I don't yet know
// if code in that form is faster due to fewer instructions or due
// to smaller code size. Note also that that emit_load_of_value of
// gep->index could also generate additional instructions...
#if 0
    else if (value->type == INSTRUCTION_GEP) {
        Instruction_GEP *gep = static_cast<Instruction_GEP *>(value);

        u32 size = gep->pointer_value->value_type->pointer_to->size;

        if (size > 8) {
            // we cant claim ownership of the index register so the result of this calculation
            // would be nontrivial, just get the stored instruction result
            u8 machine_reg = emit_load_of_value(emitter, object, code_section, value, suggested_register);
            return addr_register_disp(machine_reg);
        }

        u8 target = maybe_get_instruction_register(gep->index);

        Address_Info source = get_address_value_of_pointer(emitter, object, code_section, gep->pointer_value, (target == RAX) ? RCX : RAX);
        // gep->pointer_value->uses--;

        if (target == 0xFF) target = emit_load_of_value(emitter, object, code_section, gep->index,         (source.machine_reg == RAX) ? RCX : RAX);
        // gep->index->uses--;

        assert(source.machine_reg != target);

        Address_Info info;
        info.machine_reg = RSP; // SIB
        info.disp        = source.disp;
        info.base_reg    = source.machine_reg;
        info.index_reg   = target;
        info.scale       = (u8) size;

        return info;
    }
#endif

    {
        u8 machine_reg = emit_load_of_value(emitter, object, code_section, value, suggested_register);
        return addr_register_disp(machine_reg);
    }
}

u8 emit_instruction(X64_Emitter *emitter, Linker_Object *object, Function *function, Basic_Block *current_block, Section *code_section, Instruction *inst) {
    switch (inst->type) {
        case INSTRUCTION_ALLOCA: {
            auto _alloca = static_cast<Instruction_Alloca *>(inst);

            // Register *reg = get_free_or_suggested_register(emitter, RAX, false, inst);

            assert(_alloca->stack_offset != 0);
            // lea_into_reg64(&emitter->function_buffer, reg->machine_reg, addr_register_disp(RBP, -_alloca->stack_offset));
            return 0;
        }

        case INSTRUCTION_STORE: {
            auto store = static_cast<Instruction_Store *>(inst);

            if (Struct_Type *str = store->source_value->value_type->as<Struct_Type>()) {
                u8 source = emit_load_of_value(emitter, object, code_section, store->source_value, RSI, true);
                u8 target = emit_load_of_value(emitter, object, code_section, store->store_target, RDI, true);

                // Spill all three registers because this instruction modifies all of them
                maybe_spill_register(emitter, &emitter->register_usage[RSI]);
                maybe_spill_register(emitter, &emitter->register_usage[RDI]);
                maybe_spill_register(emitter, &emitter->register_usage[RCX]);

                if (source != RSI)
                    move_reg64_to_reg64(&emitter->function_buffer, source, RSI);

                if (target != RDI)
                    move_reg64_to_reg64(&emitter->function_buffer, source, RDI);

                move_imm_to_reg_or_clear(&emitter->function_buffer, str->size, RCX);
                rep_move_string(&emitter->function_buffer, 1);
                return 0;
            }

            Address_Info target_info = get_address_value_of_pointer(emitter, object, code_section, store->store_target, RAX);

            Constant *constant = is_constant(store->source_value);
            Type *pointee = static_cast<Pointer_Type *>(store->store_target->value_type)->pointer_to;
            if (constant && constant->is_integer()) {
                s32 value = trunc<s32>(static_cast<s64>(constant->integer_value));
                move_imm32_sext_to_memory(&emitter->function_buffer, value, target_info, pointee->size);
            } else {
                u8 source = maybe_get_instruction_register(store->source_value);
                if (source == 0xFF) source = emit_load_of_value(emitter, object, code_section, store->source_value, (target_info.machine_reg == RAX) ? RCX : RAX);

                assert(target_info.machine_reg != source);
                move_register_value_to_memory(&emitter->function_buffer, source, target_info, pointee);
            }

            store->store_target->uses--;
            store->source_value->uses--;
            break;
        }

        case INSTRUCTION_LOAD: {
            auto load = static_cast<Instruction_Load *>(inst);

            Address_Info source = get_address_value_of_pointer(emitter, object, code_section, load->pointer_value);

            u8 target_reg = source.machine_reg;
            if (target_reg == RBP || target_reg == RSP) target_reg = RAX;

            load->pointer_value->uses--;
            Register *reg = get_free_or_suggested_register(emitter, RAX, false, inst);

            if (load->value_type->as<Struct_Type>())
                lea_into_reg64(&emitter->function_buffer, reg->machine_reg, source);
            else
                move_memory_value_to_register(&emitter->function_buffer, reg->machine_reg, source, load->value_type);
            return reg->machine_reg;
        }

        case INSTRUCTION_GEP: {
            Instruction_GEP *gep = static_cast<Instruction_GEP *>(inst);

            s32 constant_disp = 0;
            bool use_constant_disp = false;
            if (Constant *con = is_constant(gep->index); con && fits_into<s32>(con->integer_value)) {
                assert(con->constant_type == Constant::INTEGER);

                use_constant_disp = true;
                constant_disp = con->integer_value;
            }

            u8 target = 0xFF;
            if (!use_constant_disp)
                target = maybe_get_instruction_register(gep->index);

            Address_Info source = get_address_value_of_pointer(emitter, object, code_section, gep->pointer_value, (target == RAX) ? RCX : RAX);
            gep->pointer_value->uses--;

            if (target == 0xFF && !use_constant_disp) target = emit_load_of_value(emitter, object, code_section, gep->index,         (source.machine_reg == RAX) ? RCX : RAX);
            gep->index->uses--;

            assert(source.machine_reg != target);

            Register *reg;

            if (!use_constant_disp)
                reg = claim_register(emitter, &emitter->register_usage[target], inst);
            else
                reg = get_free_or_suggested_register(emitter, (source.machine_reg == RAX) ? RCX : RAX, false, inst);

            Type *pointee = static_cast<Pointer_Type *>(gep->pointer_value->value_type)->pointer_to;
            u32 size = pointee->size;

            if (gep->is_struct_member_ptr) {
                size = 1;
            }

            if (size > 8 && !use_constant_disp) {
                imul_reg64_with_imm32(&emitter->function_buffer, reg->machine_reg, static_cast<s32>(pointee->size), 8);
                size = 1;
            }

            Address_Info info;

            if (!use_constant_disp) {
                info.machine_reg = RSP; // SIB
                info.disp        = source.disp + gep->offset + constant_disp;
                info.base_reg    = source.machine_reg;
                info.index_reg   = target;
                info.scale       = (u8) size;
            } else {
                info.machine_reg = source.machine_reg;
                info.disp        = (source.disp + gep->offset + constant_disp) * size;
                info.scale       = 1;
            }

            if   (source.disp) lea_into_reg64(&emitter->function_buffer, reg->machine_reg, info);
            // FIXME this no longer works if gep->offset is nonzero
            else               add_reg64_to_reg64(&emitter->function_buffer, source.machine_reg, reg->machine_reg, gep->value_type->size);

            return 0;
        }

        case INSTRUCTION_DIV: {
            auto div = static_cast<Instruction_Div *>(inst);

            u8 lhs_reg = maybe_get_instruction_register(div->lhs);
            u8 rhs_reg = maybe_get_instruction_register(div->rhs);

            if (lhs_reg == 0xFF) lhs_reg = emit_load_of_value(emitter, object, code_section, div->lhs, RAX, true);
            div->lhs->uses--;

            // Result always in RAX
            claim_register(emitter, &emitter->register_usage[RAX], inst);

            if (lhs_reg != RAX) {
                move_reg64_to_reg64(&emitter->function_buffer, lhs_reg, RAX);
                lhs_reg = RAX;
            }

            if (rhs_reg == 0xFF) rhs_reg = emit_load_of_value(emitter, object, code_section, div->rhs, RCX);
            div->rhs->uses--;

            assert(rhs_reg != RAX);
            assert(rhs_reg != RDX);

            if (div->value_type->size >= 2) {
                maybe_spill_register(emitter, &emitter->register_usage[RDX]);
                if   (div->signed_division) cwd_cdq(&emitter->function_buffer, div->value_type->size);
                else                        xor_reg64_to_reg64(&emitter->function_buffer, RDX, RDX, 8);
            } else {
                // @TODO sign-extend AL value into AH for one-byte division
            }

            div_reg64_with_rax(&emitter->function_buffer, rhs_reg, div->value_type->size, div->signed_division);
            return 0;
        }

        case INSTRUCTION_ADD:
        case INSTRUCTION_SUB:
        case INSTRUCTION_MUL: {
            auto add = static_cast<Instruction_Add *>(inst);

            u8 lhs_reg = maybe_get_instruction_register(add->lhs);
            u8 rhs_reg = maybe_get_instruction_register(add->rhs);

            if (lhs_reg == 0xFF) lhs_reg = emit_load_of_value(emitter, object, code_section, add->lhs, (rhs_reg == RAX) ? RCX : RAX);
            add->lhs->uses--;

            Constant *constant = is_constant(add->rhs);
            if (constant && constant->is_integer()) {
                claim_register(emitter, &emitter->register_usage[lhs_reg], inst);
                s64 value = static_cast<s64>(constant->integer_value);

                if      (inst->type == INSTRUCTION_ADD) add_imm32_to_reg64   (&emitter->function_buffer, lhs_reg, trunc<s32>(value), add->value_type->size);
                else if (inst->type == INSTRUCTION_SUB) sub_imm32_from_reg64 (&emitter->function_buffer, lhs_reg, trunc<s32>(value), add->value_type->size);
                else if (inst->type == INSTRUCTION_MUL) imul_reg64_with_imm32(&emitter->function_buffer, lhs_reg, trunc<s32>(value), add->value_type->size);
                return 0;
            }

            if (rhs_reg == 0xFF) rhs_reg = emit_load_of_value(emitter, object, code_section, add->rhs, (lhs_reg == RAX) ? RCX : RAX);
            add->rhs->uses--;

            claim_register(emitter, &emitter->register_usage[lhs_reg], inst);

            if      (inst->type == INSTRUCTION_ADD) add_reg64_to_reg64   (&emitter->function_buffer, rhs_reg, lhs_reg, add->value_type->size);
            else if (inst->type == INSTRUCTION_SUB) sub_reg64_from_reg64 (&emitter->function_buffer, rhs_reg, lhs_reg, add->value_type->size);
            else if (inst->type == INSTRUCTION_MUL) imul_reg64_with_reg64(&emitter->function_buffer, rhs_reg, lhs_reg, add->value_type->size);
            return 0;
        }

        case INSTRUCTION_CALL: {
            auto call = static_cast<Instruction_Call *>(inst);
            auto function_target = static_cast<Function *>(call->call_target);
            auto func_type = static_cast<Function_Type *>(function_target->value_type);

            assert(func_type->type == Type::FUNCTION);

            if (function_target->intrinsic_id) {
                switch (function_target->intrinsic_id) {
                    case Function::NOT_INTRINSIC:
                        assert(false);
                        break;
                    case Function::DEBUG_BREAK:
                        debugbreak(&emitter->function_buffer);
                        break;
                }

                return 0;
            }


            if (object->target.is_win32()) {
                // shadow space for the callee to spill registers...
                emitter->largest_call_stack_adjustment += 32;
            }

            // Spill all argument passing registers to preserve
            // their values in case the callee wishes to use them
            // ...this may not be totally correct, we should be spilling
            // all registers that are not callee-saved. @TODO
            for (auto machine_reg : emitter->parameter_registers) {
                maybe_spill_register(emitter, &emitter->register_usage[machine_reg]);
            }

            for (auto &reg : emitter->xmm_usage) {
                maybe_spill_register(emitter, &reg);
            }

            u8 num_float_params    = 0;
            u8 integer_param_index = 0;
            u8 float_param_index   = 0;

            assert(call->parameters.size() <= emitter->parameter_registers.size()); // @Incomplete
            for (u32 i = 0; i < call->parameters.size(); ++i) {
                auto p = call->parameters[i];
                u8 param_reg = emitter->parameter_registers[integer_param_index];
                u8 int_param_reg = param_reg;

                bool is_float = (p->value_type->type == Type::FLOAT);

                if (is_float) {
                    num_float_params += 1;

                    if (object->target.is_system_v()) {
                        param_reg = float_param_index;
                        float_param_index += 1;
                    } else if (object->target.is_win32()) {
                        emitter->register_usage[param_reg].is_free = false;

                        param_reg = integer_param_index;
                        integer_param_index += 1;
                    }

                    maybe_spill_register(emitter, &emitter->xmm_usage[param_reg]);
                    emitter->xmm_usage[param_reg].is_free = false;
                } else {
                    integer_param_index += 1;
                    emitter->register_usage[param_reg].is_free = false;
                }

                u8 result = emit_load_of_value(emitter, object, code_section, p, int_param_reg, true);

                if (auto str = p->value_type->as<Struct_Type>(); str && str->size <= 16 && object->target.is_system_v()) {
                    // Move the latter half of the struct into next integer register.
                    // TOOD incomplete, we need to move this value onto the stack if we don't have enough integer parameters
                    if (str->size > 8) {
                        u8 next_param = emitter->parameter_registers[integer_param_index];

                        Address_Info info = addr_register_disp(result, 8);
                        move_memory_to_reg(&emitter->function_buffer, next_param, info, str->size - 8);
                        integer_param_index += 1;
                    }

                    Address_Info info = addr_register_disp(result);
                    move_memory_to_reg(&emitter->function_buffer, param_reg, info, str->size > 8 ? 8 : str->size);
                    p->uses--;
                    continue;
                }

                if (result != param_reg) {
                    if (is_float)
                        move_xmm_to_xmm(&emitter->function_buffer, result, param_reg);
                    else
                        move_reg64_to_reg64(&emitter->function_buffer, result, param_reg);
                }

                if (is_float && object->target.is_win32() && func_type->is_varargs && i >= func_type->parameters.size()) {
                    // move float value into corresponding integer register slot
                    movq_xmm_to_reg(&emitter->function_buffer, param_reg, int_param_reg, 8);
                }

                p->uses--;
            }

            // Spill RAX
            maybe_spill_register(emitter, &emitter->register_usage[RAX]);
            if (call->value_type->type != Type::VOID)
                claim_register(emitter, &emitter->register_usage[RAX], inst);

            if (object->target.is_system_v()) {
                // load number of floating point parameters into %al
                move_imm_to_reg_or_clear(&emitter->function_buffer, num_float_params, RAX);
            }

            if (object->use_absolute_addressing) {
                maybe_spill_register(emitter, &emitter->register_usage[RCX]);

                // @Cutnpaste move_imm64_to_reg64
                move_imm64_to_reg64(&emitter->function_buffer, 0, RCX);

                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.offset = emitter->function_buffer.size() - 8;
                reloc.symbol_index = get_symbol_index(object, static_cast<Function *>(call->call_target));
                reloc.size = 8;
                reloc.addend = 0; // @TODO

                code_section->relocations.push_back(reloc);

                emitter->function_buffer.append_byte(0xFF); // callq reg
                emitter->function_buffer.append_byte(ModRM(0b11, 2, RCX));
            } else {
                emitter->function_buffer.append_byte(0xE8); // callq rip-relative
                // emitter->function_buffer.append_byte(ModRM(0b00, 0b000, 0b101));

                Relocation reloc;
                reloc.is_for_rip_call = true;
                reloc.offset = emitter->function_buffer.size();
                reloc.symbol_index = get_symbol_index(object, static_cast<Function *>(call->call_target));
                reloc.size = 4;

                u32 *addr = emitter->function_buffer.allocate_unaligned<u32>();
                *addr = 0;
                reloc.addend = 0; // @TODO

                code_section->relocations.push_back(reloc);
            }

            return RAX;
        }

        case INSTRUCTION_RETURN: {
            Instruction_Return *ret = static_cast<Instruction_Return *>(inst);

            if (ret->return_value) {
                u8 lhs_reg = maybe_get_instruction_register(ret->return_value);

                if (lhs_reg == 0xFF) lhs_reg = emit_load_of_value(emitter, object, code_section, ret->return_value, RAX, true);
                ret->return_value->uses--;

                if (lhs_reg != RAX) move_reg64_to_reg64(&emitter->function_buffer, lhs_reg, RAX);
            }

            if (!emitter->emitting_last_block) { // otherwise fallthrough to epilogue
                emitter->function_buffer.append_byte(0xE9);

                auto offset = emitter->function_buffer.size();
                emitter->epilogue_jump_target_fixups.push_back(offset);

                s32 *value = emitter->function_buffer.allocate_unaligned<s32>();
                emitter->epilogue_jump_target_fixup_pointers.push_back(value);
            }
            break;
        }

        case INSTRUCTION_BRANCH: {
            Instruction_Branch *branch = static_cast<Instruction_Branch *>(inst);

            // Spill all scratch registers at branches in case that we branch
            // to much earlier code that expects all registers to be free.
            // This may not totally be correct, but works for now. -josh 7 August 2020
            for (auto &reg : emitter->register_usage) {
                if (reg.machine_reg == RSP || reg.machine_reg == RBP) continue;

                maybe_spill_register(emitter, &reg);
            }

            if (branch->condition) {
                u8 cond = emit_load_of_value(emitter, object, code_section, branch->condition);
                sub_imm32_from_reg64(&emitter->function_buffer, cond, 0, branch->condition->value_type->size);

                maybe_spill_register(emitter, &emitter->register_usage[RAX]);
                
                emitter->function_buffer.append_byte(0x0F);
                emitter->function_buffer.append_byte(0x85); // jne if cond if true goto true block
                u32 *jne_disp = emitter->function_buffer.allocate_unaligned<u32>();
                
                auto failure_target = branch->failure_target;
                if (failure_target->type == VALUE_BASIC_BLOCK) {
                    *jne_disp = 5; // skip the next jmp instruction

                    Basic_Block *block = static_cast<Basic_Block *>(failure_target);

                    emitter->function_buffer.append_byte(0xE9);

                    // @Cutnpaste from emit_load_of_value
                    auto offset = emitter->function_buffer.size();
                    block->text_locations_needing_addr_fixup.push_back(offset);

                    u32 *value = emitter->function_buffer.allocate_unaligned<u32>();
                    block->text_ptrs_for_fixup.push_back(value);
                } else {
                    *jne_disp = 2; // skip the next jmp instruction

                    u8 fail_target = emit_load_of_value(emitter, object, code_section, branch->failure_target);
                    if (BIT3(fail_target)) {
                        *jne_disp += 1;
                        emitter->function_buffer.append_byte(REX(1, 0, 0, BIT3(fail_target)));
                    }

                    emitter->function_buffer.append_byte(0xFF); // jmp reg
                    emitter->function_buffer.append_byte(ModRM(0b11, 4, LOW3(fail_target)));
                }
            }

            Basic_Block *next_block = nullptr;
            // FIXME blocks should just know their insertion position
            // TODO maybe reoder blocks to optimize this
            for (u64 i = 0; i < function->blocks.size()-1; ++i) {
                if (function->blocks[i] == current_block) {
                    next_block = function->blocks[i+1];
                    break;
                }
            }

            if (branch->true_target != next_block) {
                auto true_target = branch->true_target;
                if (true_target->type == VALUE_BASIC_BLOCK) {
                    Basic_Block *block = static_cast<Basic_Block *>(true_target);

                    emitter->function_buffer.append_byte(0xE9);

                    // @Cutnpaste from emit_load_of_value
                    auto offset = emitter->function_buffer.size();
                    block->text_locations_needing_addr_fixup.push_back(offset);

                    u32 *value = emitter->function_buffer.allocate_unaligned<u32>();
                    block->text_ptrs_for_fixup.push_back(value);
                } else {
                    u8 target = emit_load_of_value(emitter, object, code_section, branch->true_target);

                    if (BIT3(target))
                        emitter->function_buffer.append_byte(REX(1, 0, 0, BIT3(target)));

                    emitter->function_buffer.append_byte(0xFF); // jmp reg
                    emitter->function_buffer.append_byte(ModRM(0b11, 4, LOW3(target)));
                }
            }

            branch->true_target->uses--;
            if (branch->condition) branch->condition->uses--;
            if (branch->failure_target) branch->failure_target->uses--;
            break;
        }

        default: assert(false);
    }

    return 0;
}

Register make_reg(u8 machine_reg, bool is_free = true) {
    Register reg = {};
    reg.machine_reg = machine_reg;
    reg.is_free     = is_free;
    return reg;
}

bool is_callee_saved(const Target &target, u8 reg) {
    if (target.is_win32())
        return (reg >= RBX && reg <= RDI) || (reg >= R12 && reg <= R15);
    else
        return reg == RBX || reg == RSP || reg == RBP || (reg >= R12 && reg <= R15);
}

namespace josh {

void x64_emit_function(X64_Emitter *emitter, Linker_Object *object, Function *function) {
    if (function->intrinsic_id) return;
    if (function->uses == 0 && (function->blocks.size() == 0)) {
        // Function isn't used and is externally defined so we don't need
        // to emit anything, or even add this to the symbol table.
        return;
    }

    Section *code_section = emitter->code_section;

    emitter->function_buffer.clear();
    emitter->register_usage.clear();
    emitter->xmm_usage.clear();
    emitter->parameter_registers.clear();
    emitter->stack_size_fixups.clear();
    emitter->stack_size = 0;
    emitter->largest_call_stack_adjustment = 0;
    emitter->emitting_last_block = false;

    size_t relocations_start = code_section->relocations.size();

    u32 symbol_index = get_symbol_index(object, function);
    Symbol *sym = &object->symbol_table[symbol_index];
    sym->is_function = true;
    sym->is_externally_defined = (function->blocks.size() == 0);
    if (sym->is_externally_defined) return;

    if (!sym->is_externally_defined) sym->section_number = code_section->section_number;
    sym->section_offset = emitter->code_section->data.size();

    if (object->target.is_win32()) {
        emitter->parameter_registers.push_back(RCX);
        emitter->parameter_registers.push_back(RDX);
        emitter->parameter_registers.push_back(R8);
        emitter->parameter_registers.push_back(R9);
    } else {
        emitter->parameter_registers.push_back(RDI);
        emitter->parameter_registers.push_back(RSI);
        emitter->parameter_registers.push_back(RDX);
        emitter->parameter_registers.push_back(RCX);
        emitter->parameter_registers.push_back(R8);
        emitter->parameter_registers.push_back(R9);
    }

    emitter->register_usage.push_back(make_reg(RAX));
    emitter->register_usage.push_back(make_reg(RCX));
    emitter->register_usage.push_back(make_reg(RDX));
    emitter->register_usage.push_back(make_reg(RBX));
    emitter->register_usage.push_back(make_reg(RSP, false));
    emitter->register_usage.push_back(make_reg(RBP, false));
    emitter->register_usage.push_back(make_reg(RSI));
    emitter->register_usage.push_back(make_reg(RDI));
    emitter->register_usage.push_back(make_reg(R8));
    emitter->register_usage.push_back(make_reg(R9));

    for (u8 i = 0; i <= XMM15; ++i) {
        emitter->xmm_usage.push_back(make_reg(i));
    }

    u32 reg_index = 0;
    for (u32 i = 0; i < function->arguments.size(); ++i) {
        Argument *arg = function->arguments[i];

        // TODO this is just a hacky way of handling small structs on System V, it is not totally correct
        if (auto str = arg->value_type->as<Struct_Type>(); str && str->size <= 16 && object->target.is_system_v()) {
            emitter->stack_size += str->size;

            Address_Info info = addr_register_disp(RBP, -emitter->stack_size);
            move_reg_to_memory(&emitter->function_buffer, emitter->parameter_registers[reg_index], info, str->size > 8 ? 8 : str->size);
            reg_index += 1;

            if (str->size > 8) {
                info.disp += 8;
                move_reg_to_memory(&emitter->function_buffer, emitter->parameter_registers[reg_index], info, str->size - 8);
                reg_index += 1;
            }

            arg->copied_to_stack_offset = -emitter->stack_size;
            continue;
        }

        claim_register(emitter, &emitter->register_usage[emitter->parameter_registers[reg_index]], arg);
        reg_index += 1;
    }

    for (auto block : function->blocks) {
        for (auto inst : block->instructions) {
            if (inst->type == INSTRUCTION_ALLOCA) {
                auto _alloca = static_cast<Instruction_Alloca *>(inst);

                emitter->stack_size += (_alloca->alloca_type->size * _alloca->array_size);
                if ((emitter->stack_size % 8)) emitter->stack_size += 8 - (emitter->stack_size % 8);

                _alloca->stack_offset = -emitter->stack_size;
            }
        }
    }

    for (size_t i = 0; i < function->blocks.size(); ++i) {
        auto block = function->blocks[i];

        assert(block->has_terminator());
        block->text_location = emitter->function_buffer.size();

        emitter->emitting_last_block = (i == function->blocks.size()-1);

        for (auto inst : block->instructions) {
            emit_instruction(emitter, object, function, block, code_section, inst);
        }
    }

    // block text locations need to be offset by the end of the function prologue
    // relocation offsets need to be offset by end of the function prologue

    emitter->stack_size += emitter->largest_call_stack_adjustment;
    // Ensure stack is 16-byte aligned.
    emitter->stack_size = ensure_aligned(emitter->stack_size, 16);
    assert((emitter->stack_size & (0xF)) == 0);

    size_t num_push_pops = 0;
    bool pushed_rbp = false;

    if (emitter->stack_size != 0) {
        push(&emitter->code_section->data, RBP);
        num_push_pops += 1;
        pushed_rbp = true;
    }

    for (size_t i = 0; i < emitter->register_usage.size(); ++i) {
        auto reg = &emitter->register_usage[i];
        if (reg->used && is_callee_saved(object->target, reg->machine_reg)) {
            push(&emitter->code_section->data, reg->machine_reg);
            num_push_pops += 1;
        }
    }

    bool offset_push = false;
    if (num_push_pops % 2 == 0) {
        if (emitter->stack_size)
            emitter->stack_size += 8;
        else {
            offset_push = true;
            push(&emitter->code_section->data, RAX); // push a reg so we align stack properly without needing RBP pushed and RSP modified
        }
    }

    if (emitter->stack_size != 0) {
        move_reg64_to_reg64(&emitter->code_section->data, RSP, RBP);
        s32 *stack_size_target = sub_imm32_from_reg64(&emitter->code_section->data, RSP, 0, 8);
        emitter->stack_size_fixups.push_back(stack_size_target);
    }

    // Touch stack pages from top to bottom
    // to release stack pages from the page guard system.
    // TODO we can probably simplify this by removing the loop
    // and emitting one mov instruction per stack page now that
    // we know the stack size post-function-body-generation
    if (object->target.is_win32()) {
        s32 *move_stack_size_to_rax = (s32 *)move_imm64_to_reg64(&emitter->code_section->data, 0, RAX, 4); // 4-byte immediate
        emitter->stack_size_fixups.push_back(move_stack_size_to_rax);

        auto loop_start = emitter->code_section->data.size();
        sub_imm32_from_reg64(&emitter->code_section->data, RAX, 4096, 8);


        // @Cutnpaste from move_reg_to_memory
        auto dataptr = &emitter->code_section->data;
        dataptr->append_byte(REX(1, BIT3(RAX), 0, BIT3(RSP)));

        dataptr->append_byte(0x89);
        dataptr->append_byte(ModRM(MOD_INDIRECT_NO_DISP, LOW3(RAX), LOW3(RSP)));
        dataptr->append_byte(SIB(0, RAX, RSP));

        emitter->code_section->data.append_byte(0x0F);
        emitter->code_section->data.append_byte(0x8C); // jl if RAX < 0 break
        u32 *disp = emitter->code_section->data.allocate_unaligned<u32>();
        *disp = 5; // skip the next jmp instruction

        emitter->code_section->data.append_byte(0xE9); // jmp loop start
        disp = emitter->code_section->data.allocate_unaligned<u32>();
        *disp = (loop_start - emitter->code_section->data.size());
    }

    size_t text_offset = emitter->code_section->data.size();

    for (auto block : function->blocks) {
        for (u64 i = 0; i < block->text_locations_needing_addr_fixup.size(); ++i) {
            u64 location = block->text_locations_needing_addr_fixup[i];
            u32 *addr    = block->text_ptrs_for_fixup[i];

            *addr = static_cast<u32>((block->text_location - (location+4)));
        }
    }

    emitter->code_section->data.append(&emitter->function_buffer);

    if (emitter->stack_size != 0) {
        s32 *stack_size_target = add_imm32_to_reg64(&emitter->code_section->data, RSP, 0, 8);
        emitter->stack_size_fixups.push_back(stack_size_target);
    }

    assert(emitter->stack_size >= 0);
    for (auto fixup : emitter->stack_size_fixups) {
        *fixup = emitter->stack_size;
    }

    size_t epilogue_start_offset = emitter->code_section->data.size();

    for (size_t i = 0; i < emitter->epilogue_jump_target_fixups.size(); ++i) {
        size_t location = text_offset + emitter->epilogue_jump_target_fixups[i];
        s32 *addr = emitter->epilogue_jump_target_fixup_pointers[i];

        *addr = static_cast<s32>(epilogue_start_offset - (location + 4));
    }

    if (offset_push) {
        pop(&emitter->code_section->data, RCX); // pop the stack alignment value into a caller-saved register
    }

    // reverse order pop
    for (size_t i = emitter->register_usage.size(); i > 0; --i) {
        auto reg = &emitter->register_usage[i-1];
        if (reg->used && is_callee_saved(object->target, reg->machine_reg))
             pop(&emitter->code_section->data, reg->machine_reg);
    }

    if (pushed_rbp)
        pop(&emitter->code_section->data, RBP);

    emitter->code_section->data.append_byte(0xC3); // retq

    for (size_t i = relocations_start; i < code_section->relocations.size(); ++i) {
        code_section->relocations[i].offset += text_offset;
    }
}

}