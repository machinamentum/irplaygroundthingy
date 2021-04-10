// JIT-esque Brainfuck compiler using the IR library.

#include "general.h"
#include "linker_object.h"
#include "ir.h"

using namespace josh;

#include <stdio.h>

char *bf_program = nullptr;

struct BF_Gen : IR_Manager {
    Function *putchar_func;
    Function *getchar_func;
    Value *data_buffer_alloca;
    Value *index_alloca;
    char *bf_program = nullptr;

    BF_Gen(IR_Context *context) : IR_Manager(context) {}
};

void gen_bf_instruction(Function *function, BF_Gen *bfg) {
    Instruction *load = bfg->insert_load(bfg->index_alloca);
    auto gep = bfg->insert_gep(bfg->data_buffer_alloca, load);

    Constant *constant_one_index = make_integer_constant(bfg->context, 1, load->value_type);
    Constant *constant_one = make_integer_constant(bfg->context, 1, bfg->data_buffer_alloca->value_type->as<Pointer_Type>()->pointer_to);

    while (*bfg->bf_program) {
        char c = *bfg->bf_program;
        bfg->bf_program += 1;

        switch(c) {
            case '+': {
                auto load_value = bfg->insert_load(gep);
                auto add = bfg->insert_add(load_value, constant_one);
                bfg->insert_store(add, gep);
                break;
            }

            case '-': {
                auto load_value = bfg->insert_load(gep);
                auto sub = bfg->insert_sub(load_value, constant_one);
                bfg->insert_store(sub, gep);
                break;
            }

            case '>': {
                auto add = bfg->insert_add(load, constant_one_index);
                load = add;

                gep = bfg->insert_gep(bfg->data_buffer_alloca, add);
                break;
            }

            case '<': {
                auto sub = bfg->insert_sub(load, constant_one_index);
                load = sub;

                gep = bfg->insert_gep(bfg->data_buffer_alloca, sub);
                break;
            }

            case '.': {
                auto load_value = bfg->insert_load(gep);
                bfg->insert_call(bfg->putchar_func, {load_value});
                break;
            }

            case ',': {
                Instruction_Call *call = bfg->insert_call(bfg->getchar_func);
                bfg->insert_store(call, gep);
                break;
            }

            case '[': {
                // store index value into index_alloca so looping code can load
                // it back out.
                bfg->insert_store(load, bfg->index_alloca);

                Basic_Block *loop_header = new Basic_Block();
                function->insert(loop_header);

                Basic_Block *loop_body = new Basic_Block();
                function->insert(loop_body);

                bfg->insert_branch(loop_header);

                bfg->set_block(loop_header);
                auto gep = bfg->insert_gep(bfg->data_buffer_alloca, load);
                auto load_value = bfg->insert_load(gep);

                Basic_Block *exit_block = new Basic_Block();

                bfg->insert_branch(load_value, loop_body, exit_block);

                bfg->set_block(loop_body);
                gen_bf_instruction(function, bfg);
                bfg->insert_branch(loop_header);

                function->insert(exit_block);
                bfg->set_block(exit_block);
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
    assert(len != -1);
    fseek(file, 0, SEEK_SET);

    size_t amount = static_cast<size_t>(len);
    char *buffer = (char *)malloc(amount+1);

    do {
        size_t read = fread(buffer, 1, amount, file);
        amount -= read;
    } while (amount);

    buffer[len] = 0;
    return buffer;
}

int main(int argc, char **argv) {
    char *filename = nullptr;
    u32 cell_size = 1;

    for (int i = 1; i < argc; ++i) {
        char *option = argv[i];

        if (option[0] == '-') {
            if (strcmp("-cell-size", option) == 0) {
                ++i;
                if (i >= argc) {
                    printf("No size argument given to -cell-size switch.\n");
                    return -1;
                }

                char *size = argv[i];
                cell_size = (u32)strtol(size, nullptr, 10);
                switch (cell_size) {
                    case 1:
                    case 2:
                    case 4:
                        break;
                    default:
                        printf("Invalid cell size provided to -cell-size. Must be 1, 2, or 4.\n");
                        return -1;
                }
                continue;
            } 

            printf("Unknown option: %s\n", option);
            return -1;
        } else {
            filename = option;
        }
    }

    if (!filename) {
        printf("Usage: %s <brainfuck source to compile>\n", argv[0]);
        return -1;
    }

    bf_program = get_file_contents(filename);

    IR_Context context;
    BF_Gen *bfg = new BF_Gen(&context);

    Compilation_Unit unit;
    unit.target = get_host_target();

    Function *putchar_func = bfg->make_function("putchar", make_func_type(make_void_type(), {make_integer_type(cell_size)}));

    Function *getchar_func = bfg->make_function("getchar", make_func_type(make_integer_type(1)));

    Function *memset_func = bfg->make_function("memset", make_func_type(make_void_type(), {make_pointer_type(make_integer_type(cell_size)), make_integer_type(8), make_integer_type(8)}));

    Function *main_func = bfg->make_function("main", make_func_type(make_void_type()));

    unit.functions.push_back(putchar_func);
    unit.functions.push_back(getchar_func);
    unit.functions.push_back(memset_func);
    unit.functions.push_back(main_func);

    Basic_Block *block = new Basic_Block();
    main_func->insert(block);


    bfg->bf_program = bf_program;
    bfg->putchar_func = putchar_func;
    bfg->getchar_func = getchar_func;
    bfg->set_block(block);

    Instruction_Alloca *data_buffer_alloca = bfg->insert_alloca(make_integer_type(cell_size), 30000);
    bfg->data_buffer_alloca = data_buffer_alloca;

    bfg->insert_call(memset_func, {data_buffer_alloca, make_integer_constant(bfg->context, 0), make_integer_constant(bfg->context, 30000 * data_buffer_alloca->value_type->as<Pointer_Type>()->pointer_to->size)});

    Instruction_Alloca *index_alloca = bfg->insert_alloca(make_integer_type(8), 1);
    bfg->index_alloca = index_alloca;

    bfg->insert_store(make_integer_constant(bfg->context, 512, make_integer_type(data_buffer_alloca->value_type->as<Pointer_Type>()->pointer_to->size)), index_alloca);

    gen_bf_instruction(main_func, bfg);

    bfg->insert_return();

    // emit_obj_file(&unit);
    do_jit_and_run_program_main(&context, &unit);
    return 0;
}
