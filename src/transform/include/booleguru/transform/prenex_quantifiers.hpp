#pragma once

#include "visitor.hpp"

#include <unordered_map>

namespace booleguru::transform {
struct prenex_quantifier : public visitor<prenex_quantifier> {
    std::unordered_map<uint32_t, uint32_t> bounds_map;

    inline op_ref walk_quant(op_ref o) {
        const auto &old_v = o.get_mgr()[o->quant.v];
        op_ref bound_v = o.get_mgr().get(op(op_type::Var, old_v->var.v, o.get_id()));

        uint32_t outer_bound = bounds_map[old_v->var.v];
        bounds_map[old_v->var.v] = o.get_id();

        auto e = (*this)(o.get_mgr()[o->quant.e]);

        bounds_map[old_v->var.v] = outer_bound;

        return o.get_mgr().get(op(o->type, bound_v.get_id(), e.get_id()));
    }

    inline op_ref walk_exists(op_ref o) {
        return walk_quant(o);
    }

    inline op_ref walk_forall(op_ref o) {
        return walk_quant(o);
    }

    inline op_ref walk_var(op_ref o) {
        auto it = bounds_map.find(o->var.v);
        if(it != bounds_map.end()) {
            return o.get_mgr().get(op(op_type::Var, o->var.v, it->second));
        }
        return o;
    }
};
}
