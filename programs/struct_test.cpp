#include "general.h"
#include "linker_object.h"
#include "ir.h"

using namespace josh;

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
    printf_func->value_type = make_func_type(make_void_type(), {}, true);

    Function *main_func = new Function();
    main_func->name = "main";

    Function *debugbreak = new Function();
    debugbreak->intrinsic_id = Function::DEBUG_BREAK;
    debugbreak->value_type = make_func_type(make_void_type());

    Basic_Block *block = new Basic_Block();
    main_func->insert(block);

    IR_Context context;
    IR_Manager *irm = new IR_Manager(&context);
    irm->set_block(block);

    auto _alloca = irm->insert_alloca(irm->i64);
    irm->insert_store(make_integer_constant(&context, 10), _alloca);

    {
        auto load = irm->insert_load(_alloca);
        auto mul = irm->insert_mul(load, make_integer_constant(&context, 2, irm->i64));
        irm->insert_store(mul, _alloca);
    }

    unit.functions.push_back(printf_func);
    unit.functions.push_back(main_func);


    // emit_obj_file(&unit);
    do_jit_and_run_program_main(&unit);
}