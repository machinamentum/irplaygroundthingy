#include "opt.h"
#include "internal.h"

namespace josh {

void opt_dead_function_elimination(IR_Context *context, Compilation_Unit *unit) {
    JOSH_UNUSED(context);

    for (size_t i = 0; i < unit->functions.size(); ++i) {
        if (unit->functions[i]->linkage == Function::Linkage::INTERNAL && unit->functions[i]->uses == 0) {
            unit->functions[i] = unit->functions[unit->functions.size()-1];
            unit->functions.pop_back();
            --i;
        }
    }
}

} // namespace josh