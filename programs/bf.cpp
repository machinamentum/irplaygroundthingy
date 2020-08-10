// JIT-esque Brainfuck compiler using the IR library.

#include "general.h"
#include "linker_object.h"
#include "ir.h"

#include <stdio.h>

char *bf_program = nullptr;

void gen_bf_instruction(Function *function, Basic_Block *block, Value *data_buffer_alloca, Value *index_alloca, Function *putchar_func, Function *getchar_func) {
    auto load = make_load(index_alloca);
    block->instructions.add(load);

    auto gep = make_gep(data_buffer_alloca, load);
    block->instructions.add(gep);

    while (*bf_program) {
        char c = *bf_program;
        bf_program += 1;

        switch(c) {
            case '+': {
                auto load_value = make_load(gep);
                block->instructions.add(load_value);

                auto add = make_add(load_value, make_integer_constant(1));
                block->instructions.add(add);

                block->instructions.add(make_store(add, gep));
                break;
            }

            case '-': {
                auto load_value = make_load(gep);
                block->instructions.add(load_value);

                auto sub = make_sub(load_value, make_integer_constant(1));
                block->instructions.add(sub);

                block->instructions.add(make_store(sub, gep));
                break;
            }

            case '>': {
                auto load = make_load(index_alloca);
                block->instructions.add(load);

                auto add = make_add(load, make_integer_constant(1));
                block->instructions.add(add);

                gep = make_gep(data_buffer_alloca, add);
                block->instructions.add(gep);

                block->instructions.add(make_store(add, index_alloca));
                break;
            }

            case '<': {
                auto load = make_load(index_alloca);
                block->instructions.add(load);

                auto sub = make_sub(load, make_integer_constant(1));
                block->instructions.add(sub);

                gep = make_gep(data_buffer_alloca, sub);
                block->instructions.add(gep);

                block->instructions.add(make_store(sub, index_alloca));
                break;
            }

            case '.': {
                auto load_value = make_load(gep);
                block->instructions.add(load_value);

                Instruction_Call *call = make_call(putchar_func); // @Temporary int type
                call->parameters.add(load_value);
                block->instructions.add(call);
                break;
            }

            case ',': {
                Instruction_Call *call = make_call(getchar_func);
                block->instructions.add(call);

                block->instructions.add(make_store(call, gep));
                break;
            }

            case '[': {
                Basic_Block *loop_header = new Basic_Block();
                function->blocks.add(loop_header);

                Basic_Block *loop_body = new Basic_Block();
                function->blocks.add(loop_body);

                block->instructions.add(make_branch(nullptr, loop_header));

                auto load = make_load(index_alloca);
                loop_header->instructions.add(load);

                auto gep = make_gep(data_buffer_alloca, load);
                loop_header->instructions.add(gep);

                auto load_value = make_load(gep);
                loop_header->instructions.add(load_value);

                Basic_Block *exit_block = new Basic_Block();
                function->blocks.add(exit_block);

                loop_header->instructions.add(make_branch(load_value, loop_body, exit_block));

                gen_bf_instruction(function, loop_body, data_buffer_alloca, index_alloca, putchar_func, getchar_func);

                loop_body->instructions.add(make_branch(nullptr, loop_header));

                block = exit_block;
                break;
            }

            case ']': {
                return;
            }
        }
    }
}

char *get_file_contents(char *path) {
    FILE *file = fopen(path, "rb");

    fseek(file, 0, SEEK_END);
    auto len = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = (char *)malloc(len+1);
    size_t amount = len;
    do {
        size_t read = fread(buffer, 1, amount, file);
        amount -= read;
    } while (amount);

    buffer[len] = 0;
    return buffer;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: bf <brainfuck source to compile>\n");
        return -1;
    }

    bf_program = get_file_contents(argv[1]);

    Compilation_Unit unit;
    unit.target = get_host_target();

    Function *putchar_func = new Function();
    putchar_func->name = "putchar";
    putchar_func->value_type = make_func_type(make_void_type());

    Function *getchar_func = new Function();
    getchar_func->name = "getchar";
    getchar_func->value_type = make_func_type(make_integer_type(1));

    Function *memset_func = new Function();
    memset_func->name  = "memset";
    memset_func->value_type = make_func_type(make_void_type());

    Function *main_func = new Function();
    main_func->name = "main";

    unit.functions.add(putchar_func);
    unit.functions.add(getchar_func);
    unit.functions.add(memset_func);
    unit.functions.add(main_func);

    Basic_Block *block = new Basic_Block();
    main_func->blocks.add(block);

    Instruction_Alloca *data_buffer_alloca = make_alloca(make_integer_type(1), 30000);
    block->instructions.add(data_buffer_alloca);

    Instruction_Call *call = make_call(memset_func);
    call->parameters.add(data_buffer_alloca);
    call->parameters.add(make_integer_constant(0));
    call->parameters.add(make_integer_constant(30000));
    block->instructions.add(call);

    Instruction_Alloca *index_alloca = make_alloca(make_integer_type(8), 1);
    block->instructions.add(index_alloca);

    block->instructions.add(make_store(make_integer_constant(1000), index_alloca));

    gen_bf_instruction(main_func, block, data_buffer_alloca, index_alloca, putchar_func, getchar_func);

    main_func->blocks[main_func->blocks.count-1]->instructions.add(new Instruction_Return());

    // emit_obj_file(&unit);
    do_jit_and_run_program_main(&unit);
    return 0;
}
