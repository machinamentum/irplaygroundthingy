#include "general.h"
#include "linker_object.h"
#include "ir.h"

#include <stdio.h>

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

template <typename T>
T ensure_aligned(T value, T alignment) {
    T align_bits = alignment - 1;
    if (value & align_bits) value += (alignment - (value & align_bits));
    return value;
}

template <typename T, typename B>
bool fits_into(B b) {
    T a = (T)b;
    return ((B)a) == b;
}

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

    dataptr->append_byte(REX((size == 8) ? 1 : 0, BIT3(operand1), BIT3(index_reg), BIT3(op2_reg)));

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
        *disp = operand2.disp; // @TODO ensure this fits
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

void move_reg64_to_reg64(Data_Buffer *dataptr, u8 src, u8 dst) {
    _two_register_operand_instruction(dataptr, 0x8B, true, dst, addr_register_disp(src), 8);
}

void move_reg_to_memory(Data_Buffer *dataptr, u8 src, Address_Info info, u32 size) {
    _move_bidrectional(dataptr, src, info, size, true);
}

void move_memory_to_reg(Data_Buffer *dataptr, u8 dst, Address_Info info, u32 size) {
    _move_bidrectional(dataptr, dst, info, size, false);
}

u64 *move_imm64_to_reg64(Data_Buffer *dataptr, u64 value, u8 reg, u32 size = 8) {
     u8 op = (size == 1) ? 0xB0 : 0xB8;

    _single_register_operand_instruction(dataptr, op, reg, size);
    
    u8 *operand = (u8 *)dataptr->allocate_bytes_unaligned(size);
    
    if (size == 1) *       operand = static_cast<u8> (value);
    if (size == 2) *(u16 *)operand = static_cast<u16>(value);
    if (size == 4) *(u32 *)operand = static_cast<u32>(value);
    if (size == 8) *(u64 *)operand = value;

    return (u64 *)operand;
}

void move_imm32_sext_to_memory(Data_Buffer *dataptr, u32 value, Address_Info info, u32 size) {
    u8 op = (size == 1) ? 0xC6 : 0xC7;
    _two_register_operand_instruction(dataptr, op, false, RAX, info, size);

    if (size > 4) size = 4;
    u8 *operand = (u8 *)dataptr->allocate_bytes_unaligned(size);
    
    if (size == 1) *       operand = static_cast<u8> (value);
    if (size == 2) *(u16 *)operand = static_cast<u16>(value);
    if (size == 4 || size == 8) *(u32 *)operand = value;
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

// Need to allocate 4 bytes of space after this call
s32 *lea_rip_relative_into_reg64(Data_Buffer *dataptr, u8 reg) {
    return _two_register_operand_instruction(dataptr, 0x8D, false, reg, addr_register_disp(RBP), 8);
}

void lea_into_reg64(Data_Buffer *dataptr, u8 dst, Address_Info source) {
    _two_register_operand_instruction(dataptr, 0x8D, false, dst, source, 8);
}

void pop_reg64(Data_Buffer *dataptr, u8 reg, u8 size = 8) {
    _single_register_operand_instruction(dataptr, 0x58, reg, size);
}

void push_reg64(Data_Buffer *dataptr, u8 reg, u8 size = 8) {
    _single_register_operand_instruction(dataptr, 0x50, reg, size);
}

u32 *_mathop_imm32_reg64(Data_Buffer *dataptr, u8 reg, u32 value, u32 size, u8 op_select) {
    u8 op = (size == 1) ? 0x80 : 0x81;
    _two_register_operand_instruction(dataptr, op, true, op_select, addr_register_disp(reg), size);

    if (size > 4) size = 4;
    u8 *operand = (u8 *)dataptr->allocate_bytes_unaligned(size);
    
    if (size == 1) *       operand = static_cast<u8> (value);
    if (size == 2) *(u16 *)operand = static_cast<u16>(value);
    if (size == 4 || size == 8) *(u32 *)operand = value;

    return (u32 *)operand;
}

const u8 MATH_OP_SELECT_SUB = 5;
const u8 MATH_OP_SELECT_ADD = 0;

u32 *sub_imm32_from_reg64(Data_Buffer *dataptr, u8 reg, u32 value, u32 size) {
    return _mathop_imm32_reg64(dataptr, reg, value, size, MATH_OP_SELECT_SUB);
}

u32 *add_imm32_to_reg64(Data_Buffer *dataptr, u8 reg, u32 value, u32 size) {
    return _mathop_imm32_reg64(dataptr, reg, value, size, MATH_OP_SELECT_ADD);
}

void imul_reg64_with_imm32(Data_Buffer *dataptr, u8 reg, u32 value, u32 size) {
    if (size < 2) size = 2; // @FixMe maybe, imul in this form does not support 1-byte multiplication

    u8 op = 0x69;
    _two_register_operand_instruction(dataptr, op, true, reg, addr_register_disp(reg), size);

    if (size > 4) size = 4;
    u8 *operand = (u8 *)dataptr->allocate_bytes_unaligned(size);

    if (size == 2) *(u16 *)operand = static_cast<u16>(value);
    if (size == 4 || size == 8) *(u32 *)operand = value;
}

void imul_reg64_with_reg64(Data_Buffer *dataptr, u8 src, u8 dst, u32 size) {
    if (size < 2) size = 2; // @FixMe maybe, imul in this form does not support 1-byte multiplication

    u8 op = 0xAF;
    _two_register_operand_instruction(dataptr, op, true, src, addr_register_disp(dst), size, true);
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

void maybe_spill_register(Function *func, Data_Buffer *dataptr, Register *reg) {
    if (reg->currently_holding_result_of_instruction) {
        auto inst = reg->currently_holding_result_of_instruction;
        inst->result_stored_in = nullptr;

        if (inst->result_spilled_onto_stack == 0 && inst->uses) {
            func->stack_size += 8; // @TargetInfo
            inst->result_spilled_onto_stack = -func->stack_size;
            move_reg_to_memory(dataptr, reg->machine_reg, addr_register_disp(RBP, inst->result_spilled_onto_stack), inst->value_type->size);
        }

        reg->currently_holding_result_of_instruction = nullptr;
    }

    reg->is_free = true;
}

Register *get_free_or_suggested_register(Function *function, Data_Buffer *dataptr, u8 suggested_register, bool force_use_suggested, Instruction *inst) {
    Register *reg = nullptr;

    if (!force_use_suggested) {
        reg = function->get_free_register();
        if (!reg) {

            u32 uses = U32_MAX;
            Register *regi;
            for (auto &reg : function->register_usage) {
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

    if (!reg) reg = &function->register_usage[suggested_register];
    
    maybe_spill_register(function, dataptr, reg);
    if (inst) return function->claim_register(dataptr, reg->machine_reg, inst);

    
    return reg;
}

u8 load_instruction_result(Function *function, Data_Buffer *dataptr, Instruction *inst, u8 suggested_register, bool force_use_suggested) {
    if (auto reg = inst->result_stored_in) {
        assert(reg->currently_holding_result_of_instruction == inst);
        return reg->machine_reg;
    } else {
        assert(!inst->result_stored_in);
        reg = get_free_or_suggested_register(function, dataptr, suggested_register, force_use_suggested, inst);

        assert(inst->result_spilled_onto_stack != 0);
        move_memory_to_reg_zero_ext(dataptr, reg->machine_reg, addr_register_disp(RBP, inst->result_spilled_onto_stack), inst->value_type->size);
        return reg->machine_reg;
    }
}


u8 emit_load_of_value(Linker_Object *object, Function *function, Section *code_section, Section *data_section, Value *value, u8 suggested_register = RAX, bool force_use_suggested = false) {
    if (value->type == VALUE_CONSTANT) {
        auto constant = static_cast<Constant *>(value);

        Register *reg = get_free_or_suggested_register(function, &code_section->data, suggested_register, force_use_suggested, nullptr);
        reg->is_free = false;

        if (constant->constant_type == Constant::STRING) {
            auto data_sec_offset = data_section->data.size();

            // copy the string characters into the data section
            assert(constant->string_value.length <= U32_MAX);
            u32 length = static_cast<u32>(constant->string_value.length);
            void *data_target = data_section->data.allocate_bytes_unaligned(length);
            memcpy(data_target, constant->string_value.data, length);
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

                u64 *addr = code_section->data.allocate_unaligned<u64>();
                *addr = data_sec_offset;;
                reloc.addend = data_sec_offset;

                code_section->relocations.add(reloc);
            } else {
                // lea data-section-location(%rip), %reg
                lea_rip_relative_into_reg64(&code_section->data, reg->machine_reg);

                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.is_rip_relative = true;
                reloc.offset = code_section->data.size() - 4;
                reloc.symbol_index = data_section->symbol_index;
                reloc.size = 4;

                code_section->relocations.add(reloc);
            }

            // if (_register != RAX) move_reg64_to_reg64(&code_section->data, RAX, _register);
        } else if (constant->constant_type == Constant::INTEGER) {
            move_imm64_to_reg64(&code_section->data, constant->integer_value, reg->machine_reg, constant->value_type->size);
        }

        return reg->machine_reg;
    } else if (value->type == VALUE_BASIC_BLOCK) {
        Basic_Block *block = static_cast<Basic_Block *>(value);

        Register *reg = get_free_or_suggested_register(function, &code_section->data, suggested_register, force_use_suggested, nullptr);
        reg->is_free = false;

        s32 *value = lea_rip_relative_into_reg64(&code_section->data, reg->machine_reg);

        auto offset = code_section->data.size() - 4;
        block->text_locations_needing_addr_fixup.add(offset);

        block->text_ptrs_for_fixup.add((u32 *)value);
        return reg->machine_reg;
    } else if (value->type == INSTRUCTION_ALLOCA) {
        auto _alloca = static_cast<Instruction_Alloca *>(value);

        if (_alloca->result_stored_in) return _alloca->result_stored_in->machine_reg;
        if (_alloca->result_spilled_onto_stack != 0) return load_instruction_result(function, &code_section->data, _alloca, suggested_register, force_use_suggested);

        Register *reg = get_free_or_suggested_register(function, &code_section->data, suggested_register, force_use_suggested, _alloca);

        assert(_alloca->stack_offset != 0);
        lea_into_reg64(&code_section->data, reg->machine_reg, addr_register_disp(RBP, _alloca->stack_offset));
        return reg->machine_reg;
    } else if (value->type >= INSTRUCTION_FIRST && value->type <= INSTRUCTION_LAST) {
        auto inst = static_cast<Instruction *>(value);
        return load_instruction_result(function, &code_section->data, inst, suggested_register, force_use_suggested);
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

Address_Info get_address_value_of_pointer(Linker_Object *object, Function *function, Section *code_section, Section *data_section, Value *value, u8 suggested_register = RAX) {
    assert(value->value_type->type == Type::POINTER);

    if (value->type == INSTRUCTION_ALLOCA) {
        auto _alloca = static_cast<Instruction_Alloca *>(value);
        return addr_register_disp(RBP, _alloca->stack_offset);
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
            u8 machine_reg = emit_load_of_value(object, function, code_section, data_section, value, suggested_register);
            return addr_register_disp(machine_reg);
        }

        u8 target = maybe_get_instruction_register(gep->index);

        Address_Info source = get_address_value_of_pointer(object, function, code_section, data_section, gep->pointer_value, (target == RAX) ? RCX : RAX);
        // gep->pointer_value->uses--;

        if (target == 0xFF) target = emit_load_of_value(object, function, code_section, data_section, gep->index,         (source.machine_reg == RAX) ? RCX : RAX);
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

    else {
        u8 machine_reg = emit_load_of_value(object, function, code_section, data_section, value, suggested_register);
        return addr_register_disp(machine_reg);
    }
}

u8 emit_instruction(Linker_Object *object, Function *function, Basic_Block *current_block, Section *code_section, Section *data_section, Instruction *inst) {
    inst->emitted = true;
    switch (inst->type) {
        case INSTRUCTION_ALLOCA: {
            auto _alloca = static_cast<Instruction_Alloca *>(inst);

            // Register *reg = get_free_or_suggested_register(function, &code_section->data, RAX, false, inst);

            assert(_alloca->stack_offset != 0);
            // lea_into_reg64(&code_section->data, reg->machine_reg, addr_register_disp(RBP, -_alloca->stack_offset));
            return 0;
        }

        case INSTRUCTION_STORE: {
            auto store = static_cast<Instruction_Store *>(inst);

            Address_Info target_info = get_address_value_of_pointer(object, function, code_section, data_section, store->store_target, RAX);

            Constant *constant = is_constant(store->source_value);
            if (constant && constant->is_integer()) {
                // @TODO ensure this fits
                move_imm32_sext_to_memory(&code_section->data, constant->integer_value, target_info, store->store_target->value_type->pointer_to->size);
            } else {
                u8 source = maybe_get_instruction_register(store->source_value);
                if (source == 0xFF) source = emit_load_of_value(object, function, code_section, data_section, store->source_value, (target_info.machine_reg == RAX) ? RCX : RAX);

                assert(target_info.machine_reg != source);

                move_reg_to_memory(&code_section->data, source, target_info, (u8) store->store_target->value_type->pointer_to->size);
            }

            store->store_target->uses--;
            store->source_value->uses--;
            break;
        }

        case INSTRUCTION_LOAD: {
            auto load = static_cast<Instruction_Load *>(inst);

            Address_Info source = get_address_value_of_pointer(object, function, code_section, data_section, load->pointer_value);

            u8 target_reg = source.machine_reg;
            if (target_reg == RBP || target_reg == RSP) target_reg = RAX;

            load->pointer_value->uses--;
             Register *reg = get_free_or_suggested_register(function, &code_section->data, RAX, false, inst);

            // move_memory_to_reg(&code_section->data, reg->machine_reg, source, 0, load->value_type->size);
            move_memory_to_reg_zero_ext(&code_section->data, reg->machine_reg, source, load->value_type->size);
            return reg->machine_reg;
        }

        case INSTRUCTION_GEP: {
            Instruction_GEP *gep = static_cast<Instruction_GEP *>(inst);

            u8 target = maybe_get_instruction_register(gep->index);

            Address_Info source = get_address_value_of_pointer(object, function, code_section, data_section, gep->pointer_value, (target == RAX) ? RCX : RAX);
            gep->pointer_value->uses--;

            if (target == 0xFF) target = emit_load_of_value(object, function, code_section, data_section, gep->index,         (source.machine_reg == RAX) ? RCX : RAX);
            gep->index->uses--;

            assert(source.machine_reg != target);

            Register *reg = function->claim_register(&code_section->data, target, inst);

            u32 size = gep->pointer_value->value_type->pointer_to->size;

            if (size > 8) {
                imul_reg64_with_imm32(&code_section->data, reg->machine_reg, gep->pointer_value->value_type->pointer_to->size, 8);
                size = 1;
            }

            Address_Info info;
            info.machine_reg = RSP; // SIB
            info.disp        = source.disp;
            info.base_reg    = source.machine_reg;
            info.index_reg   = target;
            info.scale       = (u8) size;

            if (source.disp) lea_into_reg64(&code_section->data, reg->machine_reg, info);
            else             add_reg64_to_reg64(&code_section->data, source.machine_reg, reg->machine_reg, gep->value_type->size);

            return 0;
        }

        case INSTRUCTION_DIV: {
            auto div = static_cast<Instruction_Div *>(inst);

            u8 lhs_reg = maybe_get_instruction_register(div->lhs);
            u8 rhs_reg = maybe_get_instruction_register(div->rhs);

            if (lhs_reg == 0xFF) lhs_reg = emit_load_of_value(object, function, code_section, data_section, div->lhs, RAX, true);
            div->lhs->uses--;

            // Result always in RAX
            Register *reg = function->claim_register(&code_section->data, RAX, inst);

            if (lhs_reg != RAX) {
                move_reg64_to_reg64(&code_section->data, lhs_reg, RAX);
                lhs_reg = RAX;
            }

            if (rhs_reg == 0xFF) rhs_reg = emit_load_of_value(object, function, code_section, data_section, div->rhs, RCX);
            div->rhs->uses--;

            assert(rhs_reg != RAX);
            assert(rhs_reg != RDX);

            if (div->value_type->size >= 2) {
                maybe_spill_register(function, &code_section->data, &function->register_usage[RDX]);
                if   (div->signed_division) cwd_cdq(&code_section->data, div->value_type->size);
                else                        xor_reg64_to_reg64(&code_section->data, RDX, RDX, 8);
            } else {
                // @TODO sign-extend AL value into AH for one-byte division
            }

            div_reg64_with_rax(&code_section->data, rhs_reg, div->value_type->size, div->signed_division);
            return 0;
        }

        case INSTRUCTION_ADD:
        case INSTRUCTION_SUB:
        case INSTRUCTION_MUL: {
            auto add = static_cast<Instruction_Add *>(inst);
            auto div = static_cast<Instruction_Div *>(inst);

            u8 lhs_reg = maybe_get_instruction_register(add->lhs);
            u8 rhs_reg = maybe_get_instruction_register(add->rhs);

            if (lhs_reg == 0xFF) lhs_reg = emit_load_of_value(object, function, code_section, data_section, add->lhs, (rhs_reg == RAX) ? RCX : RAX);
            add->lhs->uses--;

            Constant *constant = is_constant(add->rhs);
            if (constant && constant->is_integer()) {
                Register *reg = function->claim_register(&code_section->data, lhs_reg, inst);
                if      (inst->type == INSTRUCTION_ADD) add_imm32_to_reg64   (&code_section->data, lhs_reg, constant->integer_value, add->value_type->size);
                else if (inst->type == INSTRUCTION_SUB) sub_imm32_from_reg64 (&code_section->data, lhs_reg, constant->integer_value, add->value_type->size);
                else if (inst->type == INSTRUCTION_MUL) imul_reg64_with_imm32(&code_section->data, lhs_reg, constant->integer_value, add->value_type->size);
                return 0;
            }

            if (rhs_reg == 0xFF) rhs_reg = emit_load_of_value(object, function, code_section, data_section, add->rhs, (lhs_reg == RAX) ? RCX : RAX);
            add->rhs->uses--;

            Register *reg = function->claim_register(&code_section->data, lhs_reg, inst);

            if      (inst->type == INSTRUCTION_ADD) add_reg64_to_reg64   (&code_section->data, rhs_reg, lhs_reg, add->value_type->size);
            else if (inst->type == INSTRUCTION_SUB) sub_reg64_from_reg64 (&code_section->data, rhs_reg, lhs_reg, add->value_type->size);
            else if (inst->type == INSTRUCTION_MUL) imul_reg64_with_reg64(&code_section->data, rhs_reg, lhs_reg, add->value_type->size);
            return 0;
        }

        case INSTRUCTION_CALL: {
            auto call = static_cast<Instruction_Call *>(inst);
            auto function_target = static_cast<Function *>(call->call_target);

            if (function_target->intrinsic_id) {
                switch (function_target->intrinsic_id) {
                    case Function::NOT_INTRINSIC:
                        assert(false);
                        break;
                    case Function::DEBUG_BREAK:
                        debugbreak(&code_section->data);
                        break;
                }

                return 0;
            }


            if (object->target.is_win32()) {
                // shadow space for the callee to spill registers...
                function->largest_call_stack_adjustment += 32;
            }

            // Spill all argument passing registers to preserve
            // their values in case the callee wishes to use them
            for (auto machine_reg : function->parameter_registers) {
                maybe_spill_register(function, &code_section->data, &function->register_usage[machine_reg]);
            }

            assert(call->parameters.count <= function->parameter_registers.count); // @Incomplete
            for (u32 i = 0; i < call->parameters.count; ++i) {
                auto p = call->parameters[i];
                u8 param_reg = function->parameter_registers[i];
                function->register_usage[param_reg].is_free = false;

                u8 result = emit_load_of_value(object, function, code_section, data_section, p, param_reg, true);

                if (result != param_reg) move_reg64_to_reg64(&code_section->data, result, param_reg);

                p->uses--;
            }

            // Spill RAX
            maybe_spill_register(function, &code_section->data, &function->register_usage[RAX]);
            if (call->value_type->type != Type::VOID)
                function->claim_register(&code_section->data, RAX, inst);

            if (object->target.is_system_v()) {
                // load number of floating point parameters into %al
                move_imm64_to_reg64(&code_section->data, 0, RAX, 1);
            }

            if (object->use_absolute_addressing) {
                maybe_spill_register(function, &code_section->data, &function->register_usage[RBX]);

                // @Cutnpaste move_imm64_to_reg64
                code_section->data.append_byte(REX(1, 0, 0, 0));
                code_section->data.append_byte(0xB8 + RBX);

                Relocation reloc;
                reloc.is_for_rip_call = false;
                reloc.offset = code_section->data.size();
                reloc.symbol_index = get_symbol_index(object, static_cast<Function *>(call->call_target));
                reloc.size = 8;

                u64 *addr = code_section->data.allocate_unaligned<u64>();
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

                u32 *addr = code_section->data.allocate_unaligned<u32>();
                *addr = 0;
                reloc.addend = 0; // @TODO

                code_section->relocations.add(reloc);
            }

            return RAX;
        }

        case INSTRUCTION_RETURN: {
            u32 *stack_size_target = add_imm32_to_reg64(&code_section->data, RSP, 0, 8);
            function->stack_size_fixups.add(stack_size_target);


            // :WastefulPushPops:
            // pop in reverse order
            if (object->target.is_win32()) {
                pop_reg64(&code_section->data, RSI);
                pop_reg64(&code_section->data, RDI);
            }
            pop_reg64(&code_section->data, RBX);
            pop_reg64(&code_section->data, RBP);

            code_section->data.append_byte(0xC3);
            break;
        }

        case INSTRUCTION_BRANCH: {
            Instruction_Branch *branch = static_cast<Instruction_Branch *>(inst);

            // Spill all scratch registers at branches in case that we branch
            // to much earlier code that expects all registers to be free.
            // This may not totally be correct, but works for now. -josh 7 August 2020
            for (auto &reg : function->register_usage) {
                if (reg.machine_reg == RSP || reg.machine_reg == RBP) continue;

                maybe_spill_register(function, &code_section->data, &reg);
            }

            if (branch->condition) {
                u8 cond = emit_load_of_value(object, function, code_section, data_section, branch->condition);
                sub_imm32_from_reg64(&code_section->data, cond, 0, branch->condition->value_type->size);

                maybe_spill_register(function, &code_section->data, &function->register_usage[RAX]);
                
                code_section->data.append_byte(0x0F);
                code_section->data.append_byte(0x85); // jne if cond if true goto true block
                u32 *jne_disp = code_section->data.allocate_unaligned<u32>();
                
                auto failure_target = branch->failure_target;
                if (failure_target->type == VALUE_BASIC_BLOCK) {
                    *jne_disp = 5; // skip the next jmp instruction

                    Basic_Block *block = static_cast<Basic_Block *>(failure_target);

                    code_section->data.append_byte(0xE9);

                    // @Cutnpaste from emit_load_of_value
                    auto offset = code_section->data.size();
                    block->text_locations_needing_addr_fixup.add(offset);

                    u32 *value = code_section->data.allocate_unaligned<u32>();
                    block->text_ptrs_for_fixup.add(value);
                } else {
                    *jne_disp = 3; // skip the next jmp instruction

                    u8 fail_target = emit_load_of_value(object, function, code_section, data_section, branch->failure_target);
                    code_section->data.append_byte(REX(1, 0, 0, 0));
                    code_section->data.append_byte(0xFF); // jmp reg
                    code_section->data.append_byte(ModRM(0b11, 4, fail_target & 0b0111));
                }
            }

            Basic_Block *next_block = nullptr;
            for (u64 i = 0; i < function->blocks.count-1; ++i) {
                if (function->blocks[i] == current_block) {
                    next_block = function->blocks[i+1];
                    break;
                }
            }

            if (branch->true_target != next_block) {
                auto true_target = branch->true_target;
                if (true_target->type == VALUE_BASIC_BLOCK) {
                    Basic_Block *block = static_cast<Basic_Block *>(true_target);

                    code_section->data.append_byte(0xE9);

                    // @Cutnpaste from emit_load_of_value
                    auto offset = code_section->data.size();
                    block->text_locations_needing_addr_fixup.add(offset);

                    u32 *value = code_section->data.allocate_unaligned<u32>();
                    block->text_ptrs_for_fixup.add(value);
                } else {
                    u8 target = emit_load_of_value(object, function, code_section, data_section, branch->true_target);

                    code_section->data.append_byte(REX(1, 0, 0, 0));
                    code_section->data.append_byte(0xFF); // jmp reg
                    code_section->data.append_byte(ModRM(0b11, 4, target & 0b0111));
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

void emit_function(Linker_Object *object, Section *code_section, Section *data_section, Function *function) {
    u32 symbol_index = get_symbol_index(object, function);
    Symbol *sym = &object->symbol_table[symbol_index];
    sym->is_function = true;
    sym->is_externally_defined = (function->blocks.count == 0);
    if (sym->is_externally_defined) return;

    if (!sym->is_externally_defined) sym->section_number = code_section->section_number;
    sym->section_offset = code_section->data.size();

    if (object->target.is_win32()) {
        function->parameter_registers.add(RCX);
        function->parameter_registers.add(RDX);
        function->parameter_registers.add(R8);
        function->parameter_registers.add(R9);
    } else {
        function->parameter_registers.add(RDI);
        function->parameter_registers.add(RSI);
        function->parameter_registers.add(RDX);
        function->parameter_registers.add(RCX);
        function->parameter_registers.add(R8);
        function->parameter_registers.add(R9);
    }

    function->register_usage.add(make_reg(RAX));
    function->register_usage.add(make_reg(RCX));
    function->register_usage.add(make_reg(RDX));
    function->register_usage.add(make_reg(RBX));
    function->register_usage.add(make_reg(RSP, false));
    function->register_usage.add(make_reg(RBP, false));
    function->register_usage.add(make_reg(RSI));
    function->register_usage.add(make_reg(RDI));
    function->register_usage.add(make_reg(R8));
    function->register_usage.add(make_reg(R9));

    // :WastefulPushPops: @Cleanup pushing all the registers we may need is
    // a bit excessive and wasteful.
    push_reg64(&code_section->data, RBP);
    push_reg64(&code_section->data, RBX);

    if (object->target.is_win32()) {
        push_reg64(&code_section->data, RDI);
        push_reg64(&code_section->data, RSI);
    }

    // R12, R13, R14, and R15 must also be callee-saved in both conventions

    
    move_reg64_to_reg64(&code_section->data, RSP, RBP);

    for (auto block : function->blocks) {
        for (auto inst : block->instructions) {
            if (inst->type == INSTRUCTION_ALLOCA) {
                auto _alloca = static_cast<Instruction_Alloca *>(inst);

                function->stack_size += (_alloca->alloca_type->size * _alloca->array_size);
                if ((function->stack_size % 8)) function->stack_size += 8 - (function->stack_size % 8);

                _alloca->stack_offset = -function->stack_size;
            }
        }
    }


    u32 *stack_size_target = sub_imm32_from_reg64(&code_section->data, RSP, 0, 8);
    function->stack_size_fixups.add(stack_size_target);

    // Touch stack pages from top to bottom
    // to release stack pages from the page guard system.
    if (object->target.is_win32()) {
        u64 *move_stack_size_to_rax = move_imm64_to_reg64(&code_section->data, 0, RAX);
        function->stack_size_fixups.add(reinterpret_cast<u32 *>(move_stack_size_to_rax));

        auto loop_start = code_section->data.size();
        sub_imm32_from_reg64(&code_section->data, RAX, 4096, 8);


        // @Cutnpaste from move_reg_to_memory
        auto dataptr = &code_section->data;
        dataptr->append_byte(REX(1, BIT3(RAX), 0, BIT3(RSP)));

        dataptr->append_byte(0x89);
        dataptr->append_byte(ModRM(MOD_INDIRECT_NO_DISP, LOW3(RAX), LOW3(RSP)));
        dataptr->append_byte(SIB(0, RAX, RSP));

        code_section->data.append_byte(0x0F);
        code_section->data.append_byte(0x8C); // jl if RAX < 0 break
        u32 *disp = code_section->data.allocate_unaligned<u32>();
        *disp = 5; // skip the next jmp instruction

        code_section->data.append_byte(0xE9); // jmp loop start
        disp = code_section->data.allocate_unaligned<u32>();
        *disp = (loop_start - code_section->data.size());
    }


    for (auto block : function->blocks) {
        assert(block->has_terminator());
        block->text_location = code_section->data.size();

        for (auto inst : block->instructions) {
            emit_instruction(object, function, block, code_section, data_section, inst);
        }
    }

    for (auto block : function->blocks) {
        for (u64 i = 0; i < block->text_locations_needing_addr_fixup.count; ++i) {
            u64 location = block->text_locations_needing_addr_fixup[i];
            u32 *addr    = block->text_ptrs_for_fixup[i];

            *addr = static_cast<u32>((block->text_location - (location+4)));
        }
    }

    function->stack_size += function->largest_call_stack_adjustment;
    // Ensure stack is 16-byte aligned.
    function->stack_size = ensure_aligned(function->stack_size, 16);
    assert((function->stack_size & (15)) == 0);
    function->stack_size += 8;


    assert(function->stack_size >= 0);
    for (auto fixup : function->stack_size_fixups) {
        *fixup = static_cast<u32>(function->stack_size);
    }
}