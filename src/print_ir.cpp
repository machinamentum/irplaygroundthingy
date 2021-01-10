#include "print_ir.h"
#include "ir.h"

namespace josh {

void print(Type *type) {
    switch (type->type) {
        case Type::VOID   : printf("void"); return;
        case Type::INTEGER: printf("i%d", type->size * 8); return;
        case Type::FLOAT  : printf("f%d", type->size * 8); return;
        case Type::POINTER: printf("*"); print(type->pointer_to); return;
        case Type::FUNCTION: {
            print(type->function.result_type);
            printf(" (");

            for (u32 i = 0; i < type->function.parameters.count; ++i) {
                auto param = type->function.parameters[i];
                print(param);

                if (i < type->function.parameters.count-1 || type->function.is_varargs) printf(", ");
            }

            if (type->function.is_varargs) printf("...");

            printf(")");
            return;
        }

        default:
            printf("Unknown type %d\n", type->type);
            assert(false);
            break;
    }
}

} // namespace josh