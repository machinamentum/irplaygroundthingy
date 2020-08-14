#include "general.h"
#include "linker_object.h"
#include "ir.h"

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

// This magic is best explained on OSDev https://wiki.osdev.org/X86-64_Instruction_Encoding
// REX prefixes the instruction, ModRM seems to post-fix the instruction but before data operands,
// and both seem to be optional... and the processor magically understands these things and parses
// instructions correctly ????
#define REX(W, R, X, B) ((u8)(0x40 | ((W) << 3) | ((R) << 2) | ((X) << 1) | (B)))
#define ModRM(mod, reg, rm) ((u8)(((mod) << 6) | ((reg) << 3) | (rm)))
#define SIB(scale, index, base) ((u8)(((scale) << 5) | ((index) << 3) | (base)))

#define BIT3(v) ((v & (1 << 3)) >> 3)
#define LOW3(v) (v & 0x7)

void _single_register_operand_instruction(Data_Buffer *dataptr, u8 opcode, u8 reg, u8 size) {
    assert(size >= 2);

    if (size == 2) dataptr->append_byte(0x66);
    if (size >= 2) dataptr->append_byte(REX((size == 8) ? 1 : 0, 0, 0, BIT3(reg)));

    dataptr->append_byte(opcode + LOW3(reg));
}

void _two_register_operand_instruction(Data_Buffer *dataptr, u8 opcode, u8 mod, u8 operand1, u8 operand2, u32 size) {
    if (size == 2) dataptr->append_byte(0x66);
    if (size >= 2) dataptr->append_byte(REX((size == 8) ? 1 : 0, BIT3(operand1), 0, BIT3(operand2)));

    dataptr->append_byte(opcode);
    dataptr->append_byte(ModRM(mod, LOW3(operand1), LOW3(operand2)));
}

void _move_bidrectional(Data_Buffer *dataptr, u8 value_reg, u8 ptrbase, s32 disp, u32 size, bool to_memory) {
    u8 op = (size == 1) ? 0x88 : 0x89;
    if (!to_memory) op += 0x02;

    _two_register_operand_instruction(dataptr, op, 0x2, value_reg, ptrbase, size);
    s32 *value = (s32  *)dataptr->allocate(4);
    *value = disp;
}

void move_reg64_to_reg64(Data_Buffer *dataptr, u8 src, u8 dst) {
    _two_register_operand_instruction(dataptr, 0x8B, 0x3, dst, src, 8);
}

void move_reg_to_memory(Data_Buffer *dataptr, u8 src, u8 dst, s32 disp, u32 size) {
    _move_bidrectional(dataptr, src, dst, disp, size, true);
}

void move_memory_to_reg(Data_Buffer *dataptr, u8 dst, u8 src, s32 disp, u32 size) {
    _move_bidrectional(dataptr, dst, src, disp, size, false);
}

u64 *move_imm64_to_reg64(Data_Buffer *dataptr, u64 imm, u8 reg) {
    _single_register_operand_instruction(dataptr, 0xB8, reg, 8);
    u64 *value = (u64 *)dataptr->allocate(8);
    *value = imm;

    return value;
}

// Need to allocate 4 bytes of space after this call
void lea_rip_relative_into_reg64(Data_Buffer *dataptr, u8 reg) {
    _two_register_operand_instruction(dataptr, 0x8D, 0, reg, 0x5, 8);
}

void lea_into_reg64(Data_Buffer *dataptr, u8 dst, u8 src, s32 disp) {
    _two_register_operand_instruction(dataptr, 0x8D, 0x2, dst, src, 8);

    s32 *value = (s32  *)dataptr->allocate(4);
    *value = disp;
}

void pop_reg64(Data_Buffer *dataptr, u8 reg, u8 size = 8) {
    _single_register_operand_instruction(dataptr, 0x58, reg, size);
}

void push_reg64(Data_Buffer *dataptr, u8 reg, u8 size = 8) {
    _single_register_operand_instruction(dataptr, 0x50, reg, size);
}

u32 *_mathop_imm32_reg64(Data_Buffer *dataptr, u8 reg, u32 value, u32 size, u8 op_select) {
    u8 op = (size == 1) ? 0x80 : 0x81;
    _two_register_operand_instruction(dataptr, op, 0x3, op_select, reg, 8);

    if (size > 4) size = 4;
    u8 *operand = (u8 *)dataptr->allocate(size);
    
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

void add_reg64_to_reg64(Data_Buffer *dataptr, u8 src, u8 dst, u32 size) {
    u8 op = (size == 1) ? 0x00 : 0x01;
    _two_register_operand_instruction(dataptr, op, 0x3, src, dst, size);

}

void sub_reg64_from_reg64(Data_Buffer *dataptr, u8 src, u8 dst, u32 size) {
    u8 op = (size == 1) ? 0x28 : 0x29;
    _two_register_operand_instruction(dataptr, op, 0x3, src, dst, size);
}

void maybe_spill_register(Function *func, Data_Buffer *dataptr, Register *reg) {
    if (reg->currently_holding_result_of_instruction) {
        auto inst = reg->currently_holding_result_of_instruction;
        inst->result_stored_in = nullptr;

        if (inst->result_spilled_onto_stack == 0) {
            func->stack_size += 8; // @TargetInfo
            inst->result_spilled_onto_stack = func->stack_size;
            move_reg_to_memory(dataptr, reg->machine_reg, RBP, -inst->result_spilled_onto_stack, inst->value_type->size);
        }

        reg->currently_holding_result_of_instruction = nullptr;
    }

    reg->is_free = true;
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

u8 load_instruction_result(Function *function, Data_Buffer *dataptr, Instruction *inst) {
    if (auto reg = inst->result_stored_in) {
        return reg->machine_reg;
    } else {
        reg = function->get_free_register();
        if (!reg) {
            reg = &function->register_usage[RAX];
            maybe_spill_register(function, dataptr, reg);
            reg->is_free = false;
        }

        function->claim_register(dataptr, reg->machine_reg, inst);

        assert(inst->result_spilled_onto_stack != 0);
        move_memory_to_reg(dataptr, reg->machine_reg, RBP, -inst->result_spilled_onto_stack, inst->value_type->size);
        return reg->machine_reg;
    }
}


u8 emit_load_of_value(Linker_Object *object, Function *function, Section *code_section, Section *data_section, Value *value) {
    if (value->type == VALUE_CONSTANT) {
        auto constant = static_cast<Constant *>(value);

        Register *reg = function->get_free_register();
        if (!reg) {
            reg = &function->register_usage[RAX];
            maybe_spill_register(function, &code_section->data, reg);
            reg->is_free = false;
        }

        if (constant->constant_type == Constant::STRING) {
            auto data_sec_offset = data_section->data.size();

            // copy the string characters into the data section
            assert(constant->string_value.length <= U32_MAX);
            u32 length = static_cast<u32>(constant->string_value.length);
            void *data_target = data_section->data.allocate(length);
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
    } else if (value->type == VALUE_BASIC_BLOCK) {
        Basic_Block *block = static_cast<Basic_Block *>(value);

        Register *reg = function->get_free_register();
        if (!reg) {
            reg = &function->register_usage[RAX];
            maybe_spill_register(function, &code_section->data, reg);
            reg->is_free = false;
        }

        lea_rip_relative_into_reg64(&code_section->data, reg->machine_reg);

        auto offset = code_section->data.size();
        block->text_locations_needing_addr_fixup.add(offset);

        u32 *value = (u32 *)code_section->data.allocate(4);
        block->text_ptrs_for_fixup.add(value);
        return reg->machine_reg;
    } else if (value->type >= INSTRUCTION_FIRST && value->type <= INSTRUCTION_LAST) {
        auto inst = static_cast<Instruction *>(value);
        return load_instruction_result(function, &code_section->data, inst);
    }

    assert(false);
    return 0;
}

#include <stdio.h>

u8 emit_instruction(Linker_Object *object, Function *function, Basic_Block *current_block, Section *code_section, Section *data_section, Instruction *inst) {
    switch (inst->type) {
        case INSTRUCTION_ALLOCA: {
            auto _alloca = static_cast<Instruction_Alloca *>(inst);

            Register *reg = function->get_free_register();
            if (!reg) {
                reg = &function->register_usage[RAX];
                maybe_spill_register(function, &code_section->data, reg);
                reg->is_free = false;
            }

            function->claim_register(&code_section->data, reg->machine_reg, _alloca);

            assert(_alloca->stack_offset != 0);
            lea_into_reg64(&code_section->data, reg->machine_reg, RBP, -_alloca->stack_offset);
            return reg->machine_reg;
        }

        case INSTRUCTION_STORE: {
            auto store = static_cast<Instruction_Store *>(inst);

            // Ensure two registers are free for the operation
            maybe_spill_register(function, &code_section->data, &function->register_usage[RAX]);
            maybe_spill_register(function, &code_section->data, &function->register_usage[RCX]);

            u8 target = emit_load_of_value(object, function, code_section, data_section, store->store_target);
            u8 source = emit_load_of_value(object, function, code_section, data_section, store->source_value);

            move_reg_to_memory(&code_section->data, source, target, 0, (u8) store->store_target->value_type->pointer_to->size);
            break;
        }

        case INSTRUCTION_LOAD: {
            auto load = static_cast<Instruction_Load *>(inst);

            u8 source = emit_load_of_value(object, function, code_section, data_section, load->pointer_value);

            Register *reg = &function->register_usage[source];
            function->claim_register(&code_section->data, reg->machine_reg, inst);

            move_memory_to_reg(&code_section->data, reg->machine_reg, source, 0, load->value_type->size);
            return reg->machine_reg;
        }

        case INSTRUCTION_GEP: {
            Instruction_GEP *gep = static_cast<Instruction_GEP *>(inst);

            // Ensure two registers are free for the operation
            maybe_spill_register(function, &code_section->data, &function->register_usage[RAX]);
            maybe_spill_register(function, &code_section->data, &function->register_usage[RCX]);

            u8 source = emit_load_of_value(object, function, code_section, data_section, gep->pointer_value);
            u8 target = emit_load_of_value(object, function, code_section, data_section, gep->index);

            Register *reg = &function->register_usage[target];
            maybe_spill_register(function, &code_section->data, reg);
            function->claim_register(&code_section->data, reg->machine_reg, inst);
            reg->is_free = false;

            add_reg64_to_reg64(&code_section->data, source, reg->machine_reg, gep->value_type->size);
            return reg->machine_reg;
        }

        case INSTRUCTION_SUB:
        case INSTRUCTION_ADD: {
            auto add = static_cast<Instruction_Add *>(inst);

            // Ensure two registers are free for the operation
            maybe_spill_register(function, &code_section->data, &function->register_usage[RAX]);
            maybe_spill_register(function, &code_section->data, &function->register_usage[RCX]);

            u8 lhs_reg = emit_load_of_value(object, function, code_section, data_section, add->lhs);
            u8 rhs_reg = emit_load_of_value(object, function, code_section, data_section, add->rhs);

            Register *reg = &function->register_usage[lhs_reg];
            maybe_spill_register(function, &code_section->data, reg);
            reg->is_free = false;

            function->claim_register(&code_section->data, reg->machine_reg, inst);

            if      (inst->type == INSTRUCTION_ADD) add_reg64_to_reg64(&code_section->data, rhs_reg, lhs_reg, add->value_type->size);
            else if (inst->type == INSTRUCTION_SUB) sub_reg64_from_reg64(&code_section->data, rhs_reg, lhs_reg, add->value_type->size);
            return reg->machine_reg;
        }

        case INSTRUCTION_CALL: {
            auto call = static_cast<Instruction_Call *>(inst);

            if (object->target.is_win32()) {
                // shadow space for the callee to spill registers...
                sub_imm32_from_reg64(&code_section->data, RSP, 32, 8);
            }

            u8 index = 0;
            for (auto p : call->parameters) {
                u8 param_reg = 0xFF;

                if (object->target.is_win32()) {
                    param_reg = get_next_win64_abi_register(&index);
                } else {
                    param_reg = get_next_system_v_abi_register(&index);
                }

                assert(param_reg != 0xFF);

                for (auto &reg : function->register_usage) {
                    if (reg.machine_reg == param_reg) {
                        maybe_spill_register(function, &code_section->data, &reg);
                        reg.is_free = false;
                        break;
                    }
                }

                u8 result = emit_load_of_value(object, function, code_section, data_section, p);

                if (result != param_reg) move_reg64_to_reg64(&code_section->data, result, param_reg);
            }

            // Spill RAX
            function->claim_register(&code_section->data, RAX, inst);

            if (object->target.is_system_v()) {
                // load number of floating point parameters into %al
                code_section->data.append_byte(REX(1, 0, 0, 0));
                code_section->data.append_byte(0xB8 + RAX);
                u64 *value = (u64 *)code_section->data.allocate(8);
                *value = 0;
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

            if (object->target.is_win32()) {
                add_imm32_to_reg64(&code_section->data, RSP, 32, 8);
            }

            return RAX;
        }

        case INSTRUCTION_RETURN: {
            u32 *stack_size_target = add_imm32_to_reg64(&code_section->data, RSP, 0, 8);
            function->stack_size_fixups.add(stack_size_target);


            // :WastefulPushPops:
            // pop in reverse order
            for (size_t i = function->register_usage.count; i > 0; --i) {
                auto &reg = function->register_usage[i-1];
                pop_reg64(&code_section->data, reg.machine_reg);
            }
            

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
                maybe_spill_register(function, &code_section->data, &reg);
            }

            if (branch->condition) {
                u8 cond = emit_load_of_value(object, function, code_section, data_section, branch->condition);
                sub_imm32_from_reg64(&code_section->data, cond, 0, branch->condition->value_type->size);

                maybe_spill_register(function, &code_section->data, &function->register_usage[RAX]);
                u8 fail_target = emit_load_of_value(object, function, code_section, data_section, branch->failure_target);

                code_section->data.append_byte(0x0F);
                code_section->data.append_byte(0x85); // jne if cond if true goto true block
                u32 *disp = (u32 *)code_section->data.allocate(4);
                *disp = 3; // skip the next jmp instruction

                code_section->data.append_byte(REX(1, 0, 0, 0));
                code_section->data.append_byte(0xFF); // jmp reg
                code_section->data.append_byte(ModRM(0b11, 4, fail_target & 0b0111));
            }

            Basic_Block *next_block = nullptr;
            for (u64 i = 0; i < function->blocks.count-1; ++i) {
                if (function->blocks[i] == current_block) {
                    next_block = function->blocks[i+1];
                    break;
                }
            }

            if (branch->true_target != next_block) {
                u8 target = emit_load_of_value(object, function, code_section, data_section, branch->true_target);

                code_section->data.append_byte(REX(1, 0, 0, 0));
                code_section->data.append_byte(0xFF); // jmp reg
                code_section->data.append_byte(ModRM(0b11, 4, target & 0b0111));
            }
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
    

    push_reg64(&code_section->data, RBP);

    // :WastefulPushPops: @Cleanup pushing all the registers we may need is
    // a bit excessive and wasteful.
   for (auto reg : function->register_usage) {
        push_reg64(&code_section->data, reg.machine_reg);
   }
    
    move_reg64_to_reg64(&code_section->data, RSP, RBP);

    for (auto block : function->blocks) {
        for (auto inst : block->instructions) {
            if (inst->type == INSTRUCTION_ALLOCA) {
                auto _alloca = static_cast<Instruction_Alloca *>(inst);

                function->stack_size += (_alloca->alloca_type->size * _alloca->array_size);
                if ((function->stack_size % 8)) function->stack_size += 8 - (function->stack_size % 8);

                _alloca->stack_offset = function->stack_size;
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
        dataptr->append_byte(ModRM(0x0, LOW3(RAX), LOW3(RSP)));
        dataptr->append_byte(SIB(0, RAX, RSP));

        code_section->data.append_byte(0x0F);
        code_section->data.append_byte(0x8C); // jl if RAX < 0 break
        u32 *disp = (u32 *)code_section->data.allocate(4);
        *disp = 5; // skip the next jmp instruction

        code_section->data.append_byte(0xE9); // jmp loop start
        disp = (u32 *)code_section->data.allocate(4);
        *disp = (loop_start - code_section->data.size()); // skip the next jmp instruction
    }


    for (auto block : function->blocks) {
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

    // Ensure stack is 16-byte aligned.
    if ((function->stack_size % 16)) function->stack_size += 16 - (function->stack_size % 16);


    assert(function->stack_size >= 0);
    for (auto fixup : function->stack_size_fixups) {
        *fixup = static_cast<u32>(function->stack_size);
    }
}