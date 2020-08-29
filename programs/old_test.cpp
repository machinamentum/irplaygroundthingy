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

    Function *debugbreak = new Function();
    debugbreak->intrinsic_id = Function::DEBUG_BREAK;
    debugbreak->value_type = make_func_type(make_void_type());

    Basic_Block *block = new Basic_Block();
    main_func->insert(block);

    IR_Manager *irm = new IR_Manager();
    irm->set_block(block);

    auto _alloca = irm->insert_alloca(irm->i64);
    irm->insert_store(make_integer_constant(10), _alloca);

    {
        auto load = irm->insert_load(_alloca);
        auto mul = irm->insert_mul(load, make_integer_constant(2, irm->i64));
        irm->insert_store(mul, _alloca);
    }

    {
        Basic_Block *loop_header = new Basic_Block();
        main_func->insert(loop_header);

        Basic_Block *loop_body = new Basic_Block();
        main_func->insert(loop_body);

        Basic_Block *loop_exit = new Basic_Block();
        main_func->insert(loop_exit);

        irm->insert_branch(loop_header);

        irm->set_block(loop_header);

        auto load = irm->insert_load(_alloca);
        auto sub = irm->insert_sub(load, make_integer_constant(1, irm->i64));
        irm->insert_store(sub, _alloca);
        irm->insert_branch(load, loop_body, loop_exit);

        irm->set_block(loop_body);

        irm->insert_call(printf_func, {make_string_constant("Hello World: %d\n"), load});

        // irm->insert_call(debugbreak);

        irm->insert_branch(loop_header);

        irm->set_block(loop_exit);
        irm->insert_return();
    }
    
    unit.functions.add(printf_func);
    unit.functions.add(main_func);


    emit_obj_file(&unit);
    // do_jit_and_run_program_main(&unit);
}