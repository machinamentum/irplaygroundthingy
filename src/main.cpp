#include "general.h"
#include "linker_object.h"
#include "ir.h"

void old_test();

int main(int argc, char **argv) {
    old_test();
    return 0;
}

void old_test() {
    Compilation_Unit unit;
    unit.target = get_host_target();

    Function *printf_func = new Function();
    printf_func->name = "printf";
    printf_func->value_type = make_func_type(make_void_type());

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

        Instruction_Call *call = make_call(printf_func);
        call->parameters.add(make_string_constant("Hello World: %d\n"));
        call->parameters.add(load);

        loop_body->instructions.add(call);

        loop_body->instructions.add(make_branch(nullptr, loop_header));

        loop_exit->instructions.add(new Instruction_Return());
    }
    
    unit.functions.add(printf_func);
    unit.functions.add(main_func);


    emit_obj_file(&unit);
    // do_jit_and_run_program_main(&unit);
}