
#ifndef JOSH_OPT_H
#define JOSH_OPT_H

#include "general.h"
#include "ir.h"

namespace josh {

void opt_dead_function_elimination(IR_Context *context, Compilation_Unit *unit);

} // namespace josh

#endif