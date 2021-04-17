#include "general.h"
#include "linker_object.h"
#include "ir.h"

#define UNUSED(x) ((void)(x))

using namespace josh;

void struct_test();

int main(int argc, char **argv) {
    UNUSED(argc);
    UNUSED(argv);
    struct_test();
    return 0;
}

Function *emit_get_field1_func(IR_Manager *irm, Struct_Type *str) {
    Function *get = irm->make_function("get_field", make_func_type(irm->i32, {str}));

    Basic_Block *block = new Basic_Block();
    get->insert(block);
    irm->set_block(block);

    Argument *arg = make_arg(irm->context, str);
    get->arguments.push_back(arg);


    // TODO there is no way to extract a value out of a struct-value-type right now, so store on the stack
    auto _alloca = irm->insert_alloca(str);

    irm->insert_store(arg, _alloca);

    auto gep = irm->insert_gep(_alloca, {make_integer_constant(irm->context, 0), make_integer_constant(irm->context, 1)});
    irm->insert_return(irm->insert_load(gep));

    return get;
}

void struct_test() {
    Compilation_Unit unit;
    unit.target = get_host_target();

    IR_Context context;
    IR_Manager *irm = new IR_Manager(&context);

    Function *printf_func = irm->make_function("printf", make_func_type(make_void_type(), {}, true));

    Function *main_func = irm->make_function("main", make_func_type(make_void_type()));

    Function *debugbreak = new Function();
    debugbreak->intrinsic_id = Function::DEBUG_BREAK;
    debugbreak->value_type = make_func_type(make_void_type());

    Basic_Block *block = new Basic_Block();
    main_func->insert(block);

    

    Struct_Type *str_ty = make_struct_type({irm->i64, irm->i64});
    Function *get_field_func = emit_get_field1_func(irm, str_ty);
    unit.functions.push_back(get_field_func);

    irm->set_block(block);

    auto _alloca = irm->insert_alloca(str_ty);
    auto gep1    = irm->insert_gep(_alloca, {make_integer_constant(&context, 0), make_integer_constant(&context, 0)});
    auto gep2    = irm->insert_gep(_alloca, {make_integer_constant(&context, 0), make_integer_constant(&context, 1)});
    irm->insert_store(make_integer_constant(&context, 10), gep2);
    irm->insert_store(make_integer_constant(&context, 13), gep1);

    // auto _alloca2 = irm->insert_alloca(make_struct_type({irm->i64, irm->i64}));
    // irm->insert_store(irm->insert_load(_alloca), _alloca2);
    // auto gep3    = irm->insert_gep(_alloca2, {make_integer_constant(&context, 0), make_integer_constant(&context, 0)});
    // auto gep4    = irm->insert_gep(_alloca2, {make_integer_constant(&context, 0), make_integer_constant(&context, 1)});

    // irm->insert_store(make_integer_constant(&context, 25), gep2);
    // irm->insert_store(make_integer_constant(&context, 22), gep1);

    irm->insert_call(printf_func, {make_string_constant(&context, "GEP2: %d\n"), irm->insert_call(get_field_func, {irm->insert_load(_alloca)})});
    // irm->insert_call(printf_func, {make_string_constant(&context, "GEP2: %d\n"), irm->insert_load(gep2)});

    // irm->insert_call(printf_func, {make_string_constant(&context, "GEP3: %d\n"), irm->insert_call(get_field_func, {irm->insert_load(_alloca2)})});
    // irm->insert_call(printf_func, {make_string_constant(&context, "GEP4: %d\n"), irm->insert_load(gep4)});

    irm->insert_return();

    unit.functions.push_back(printf_func);
    unit.functions.push_back(main_func);


    // emit_obj_file(&unit);
    do_jit_and_run_program_main(&context, &unit);
}