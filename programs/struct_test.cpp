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

    auto _alloca = irm->insert_alloca(make_struct_type({irm->i64, irm->i64}));
    auto gep1    = irm->insert_gep(_alloca, {make_integer_constant(&context, 0), make_integer_constant(&context, 0)}); 
    auto gep2    = irm->insert_gep(_alloca, {make_integer_constant(&context, 0), make_integer_constant(&context, 1)});
    irm->insert_store(make_integer_constant(&context, 10), gep2);
    irm->insert_store(make_integer_constant(&context, 13), gep1);

    auto _alloca2 = irm->insert_alloca(make_struct_type({irm->i64, irm->i64}));
    irm->insert_store(irm->insert_load(_alloca), _alloca2);
    auto gep3    = irm->insert_gep(_alloca2, {make_integer_constant(&context, 0), make_integer_constant(&context, 0)});
    auto gep4    = irm->insert_gep(_alloca2, {make_integer_constant(&context, 0), make_integer_constant(&context, 1)});

    irm->insert_store(make_integer_constant(&context, 25), gep2);
    irm->insert_store(make_integer_constant(&context, 22), gep1);

    irm->insert_call(printf_func, {make_string_constant(&context, "GEP1: %d\n"), irm->insert_load(gep1)});
    irm->insert_call(printf_func, {make_string_constant(&context, "GEP2: %d\n"), irm->insert_load(gep2)});

    irm->insert_call(printf_func, {make_string_constant(&context, "GEP3: %d\n"), irm->insert_load(gep3)});
    irm->insert_call(printf_func, {make_string_constant(&context, "GEP4: %d\n"), irm->insert_load(gep4)});

    irm->insert_return();

    unit.functions.push_back(printf_func);
    unit.functions.push_back(main_func);


    emit_obj_file(&unit);
    // do_jit_and_run_program_main(&unit);
}