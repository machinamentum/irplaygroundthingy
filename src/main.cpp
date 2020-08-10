#include "general.h"
#include "linker_object.h"
#include "ir.h"

char *bf_program =
"++++[++++>---<]>-.++[----->+<]>+.-.[---->+<]>+++.+[->+++<]>+.+++++++++++."
"[--->+<]>-----.++++[->++<]>+.-[->++++<]>.---[->++++<]>.+[->+++<]>++.++++++++++."
"------.--[--->+<]>-.---[->++++<]>.------------.---.[--->+<]>----.++++[->+++<]>.--"
"[--->+<]>-.[->+++<]>++.[--->+<]>-.-------.+++++.+[---->+<]>+++.---[->++++<]>.-----"
".[--->+<]>-----.-[--->++<]>--.-------.+[--->+<]>.+[->+++<]>.--[--->+<]>-.[->+++<]>+"
".+++++++++++++.----------.-[--->+<]>-.-[--->++<]>--.+++++++.---.--------.-[--->++<]>--"
"-.>++++++++++.++[++++>---<]>.-[->++++<]>.+[->+++<]>+.+++++++++++.>++++++++++.>-[--->+<]>-."
"-[--->+<]>--.--------------.--[--->+<]>--.+.[---->+<]>+++.---[->++++<]>.------------.---.+"
"+++++++.[->+++++<]>-.[-->+++++++<]>.-----------.+++++++++++++.------------.-.--.-[--->+<]>--."
"[---->+<]>+++.---[->++++<]>-.----.[--->+<]>-----.---[->++++<]>.------------.---.++++.+++++++++"
".[-->+++++<]>+++.++[--->++<]>.+++.+++++++.+[->+++<]>.--[--->+<]>-.++[->+++<]>+.+++++++++++.---."
"++++++++.----.+[---->+<]>+++.+++++[->+++<]>.---.--------.-[--->++<]>---.>++++++++++.++[++++>---<]"
">.-[->++++<]>.+[->+++<]>+.+++++++++++.>++++++++++.+[->++++++<]>++.[--->+<]>+++.[--->+<]>-----.+++"
"+[->++<]>+.-[->++++<]>.---[->++++<]>-.++++.[->+++<]>.----.--[--->+<]>---.[-->+++++<]>+++.---[->++++<]"
">.------------.-------.--[--->+<]>-.[---->+<]>+++.++++[->++<]>+.-[->++++<]>.--[->++++<]>-.--------.-."
"[++>---<]>++.[->+++<]>-.[---->+<]>+++.---[->++++<]>-.+++[->+++<]>+.-[--->+<]>----.-------------.[--->+<]"
">.+[---->+<]>++.---[->++++<]>.------------.+.++++++++++.+[---->+<]>+++.---[->++++<]>+.-----.[--->++++<]"
">-.>++++++++++.++[++++>---<]>.-[->++++<]>.+[->+++<]>+.+++++++++++.>++++++++++.+[->++++++<]>++.[--->+<]>++"
"+.[--->+<]>-----.++++[->++<]>+.-[->++++<]>.+[----->+<]>+.---------..-.-[--->+<]>-.---[->++++<]>.--------"
"----.+.++++++++++.+[---->+<]>+++.+[----->+<]>.++.+++.-------------.--[--->+<]>-.---[->++++<]>.------------"
".-------.+++++++++++++.-[->+++++<]>-.[->+++<]>+.+++++++++++++.[--->+<]>-.-----.------------.+.+++++.-------"
".+++[->+++<]>+.>++++++++++.++[++++>---<]>.-[->++++<]>.+[->+++<]>+.+++++++++++.>++++++++++.";

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

                Instruction_Call *call = new Instruction_Call();
                call->call_target = putchar_func;
                call->parameters.add(load_value);
                block->instructions.add(call);
                break;
            }

            case ',': {
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

void old_test();

int main(int argc, char **argv) {
    // old_test();
    // return 0;

    Compilation_Unit unit;
    unit.target = get_host_target();

    Function *putchar_func = new Function();
    putchar_func->name = "putchar";

    Function *getchar_func = new Function();
    getchar_func->name = "getchar";

    Function *memset_func = new Function();
    memset_func->name  = "memset";

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

    Instruction_Call *call = new Instruction_Call();
    call->call_target = memset_func;
    call->parameters.add(data_buffer_alloca);
    call->parameters.add(make_integer_constant(0));
    call->parameters.add(make_integer_constant(30000));
    block->instructions.add(call);

    Instruction_Alloca *index_alloca = make_alloca(make_integer_type(8), 1);
    block->instructions.add(index_alloca);

    block->instructions.add(make_store(make_integer_constant(0), index_alloca));

    gen_bf_instruction(main_func, block, data_buffer_alloca, index_alloca, putchar_func, getchar_func);

    main_func->blocks[main_func->blocks.count-1]->instructions.add(new Instruction_Return());

    // emit_obj_file(&unit);
    do_jit_and_run_program_main(&unit);
    return 0;
}

void old_test() {
    Compilation_Unit unit;
    unit.target = get_host_target();

    Function *printf_func = new Function();
    printf_func->name = "printf";

    Function *main_func = new Function();
    main_func->name = "main";

    Basic_Block *block = new Basic_Block();
    main_func->blocks.add(block);

    Instruction_Alloca *_alloca = make_alloca(make_integer_type(8));
    block->instructions.add(_alloca);
    block->instructions.add(make_store(make_integer_constant(10), _alloca));

    {
        Basic_Block *loop_header = new Basic_Block();
        main_func->blocks.add(loop_header);

        Basic_Block *loop_body = new Basic_Block();
        main_func->blocks.add(loop_body);

        Basic_Block *loop_exit = new Basic_Block();
        main_func->blocks.add(loop_exit);

        block->instructions.add(make_branch(nullptr, loop_header));

        Instruction_Load *load = make_load(_alloca);
        loop_header->instructions.add(load);

        Instruction_Sub *add = make_sub(load, make_integer_constant(1));
        loop_header->instructions.add(add);

        loop_header->instructions.add(make_store(add, _alloca));

        loop_header->instructions.add(make_branch(load, loop_body, loop_exit));

        Instruction_Call *call = new Instruction_Call();
        call->call_target = printf_func;
        call->parameters.add(make_string_constant("Hello World: %d\n"));
        call->parameters.add(load);

        loop_body->instructions.add(call);

        loop_body->instructions.add(make_branch(nullptr, loop_header));

        loop_exit->instructions.add(new Instruction_Return());
    }
    
    unit.functions.add(printf_func);
    unit.functions.add(main_func);


    // emit_obj_file(&unit);
    do_jit_and_run_program_main(&unit);
}