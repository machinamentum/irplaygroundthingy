#include "print_ir.h"
#include "ir.h"

namespace josh {

void print(Type *type) {
    switch (type->type) {
        case Type::VOID   : printf("void"); return;
        case Type::INTEGER: printf("i%d", type->size * 8); return;
        case Type::FLOAT  : printf("f%d", type->size * 8); return;
        case Type::POINTER: printf("*"); print(static_cast<Pointer_Type *>(type)->pointer_to); return;
        case Type::FUNCTION: {
            auto ftype = static_cast<Function_Type *>(type);
            print(ftype->result_type);
            printf(" (");

            for (u32 i = 0; i < ftype->parameters.size(); ++i) {
                auto param = ftype->parameters[i];
                print(param);

                if (i < ftype->parameters.size()-1 || ftype->is_varargs) printf(", ");
            }

            if (ftype->is_varargs) printf("...");

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