#include <optional>
#include <type_traits>

#ifdef EMBEDDED_PYTHON_MODULE
#include <pybind11/embed.h>
#endif

#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <booleguru/expression/literals.hpp>

#include <booleguru/py/pyop.hpp>
#include <booleguru/py/python-context.hpp>
#include <booleguru/py/solver.hpp>

#include <booleguru/lua/binding-helpers.hpp>

using namespace booleguru;
using namespace booleguru::expression;
using namespace booleguru::lua;
using namespace booleguru::lua::helpers;
using namespace booleguru::py;

template<typename F>
auto
pyop_un_wrapper(F f) {
  return [f](pyop_ref& l) -> pyop_ref { return pyop_ref(f(l)); };
}

template<typename F>
auto
pyop_un_opt_wrapper(F f) {
  return [f](pyop_ref& l) -> std::optional<pyop_ref> {
    auto o = f(l);
    if(o)
      return pyop_ref(*o);
    else
      return std::nullopt;
  };
}

template<typename F>
auto
pyop_bin_wrapper(F f) {
  return
    [f](pyop_ref& l, pyop_ref& r) -> pyop_ref { return pyop_ref(f(l, r)); };
}

// This is trying to follow
// https://z3prover.github.io/api/html/namespacez3py.html with a custom spin to
// support booleguru's variants.

#ifdef EMBEDDED_PYTHON_MODULE
PYBIND11_EMBEDDED_MODULE(pybooleguru, m) {
#else
PYBIND11_MODULE(pybooleguru, m) {
#endif
  m.doc() = "booleguru: boolean formula multitool";

  pybind11::enum_<solve::result::type>(m, "result")
    .value("SAT", solve::result::SAT)
    .value("UNSAT", solve::result::UNSAT)
    .value("UNKNOWN", solve::result::UNKNOWN);

  pybind11::enum_<transform::prenex_quantifier::kind>(m, "prenex_kind")
    .value("Eup_Aup", transform::prenex_quantifier::Eup_Aup)
    .value("Eup_Adown", transform::prenex_quantifier::Eup_Adown)
    .value("Edown_Aup", transform::prenex_quantifier::Edown_Aup)
    .value("Edown_Adown", transform::prenex_quantifier::Edown_Adown);

  pybind11::enum_<op_type>(m, "op_type")
    .value("none", op_type::None)
    .value("exists", op_type::Exists)
    .value("forall", op_type::Forall)
    .value("equi", op_type::Equi)
    .value("impl", op_type::Impl)
    .value("or_", op_type::Or)
    .value("and_", op_type::And)
    .value("xor_", op_type::Xor)
    .value("not_", op_type::Not)
    .value("var", op_type::Var);

  pybind11::class_<pyop_ref>(m, "op")
    .def_property_readonly("t", helpers::get_op_ref_type)
    .def_property_readonly("id", helpers::get_op_id)
    .def_property_readonly("l", pyop_un_opt_wrapper(helpers::get_op_left))
    .def_property_readonly("r", pyop_un_opt_wrapper(helpers::get_op_right))

    .def("__repr__", &pyop_ref::to_string)

    .def("rename", &helpers::rename)
    .def("rename_map", &helpers::rename_map)

    .def(
      "__and__",
      [](pyop_ref& l, pyop_ref& r) { return l && r; },
      pybind11::is_operator())
    .def(
      "__or__",
      [](pyop_ref& l, pyop_ref& r) { return l || r; },
      pybind11::is_operator())
    .def(
      "__neg__", [](pyop_ref& l) { return !l; }, pybind11::is_operator())
    .def(
      "__xor__",
      [](pyop_ref& l, pyop_ref& r) { return l ^ r; },
      pybind11::is_operator())
    .def(
      "__eq__",
      [](pyop_ref& l, pyop_ref& r) -> pyop_ref { return l == r; },
      pybind11::is_operator())
    .def(
      "__hash__",
      [](const pyop_ref& self) { return self.hash(); },
      pybind11::is_operator())

    .def_property_readonly("v", helpers::get_op_varop_v)
    .def_property_readonly("q", helpers::get_op_varop_q)

    .def_property_readonly("and_inside", helpers::get_op_and_inside)
    .def_property_readonly("is_ors", helpers::get_op_is_ors)
    .def_property_readonly("is_cnf", helpers::get_op_is_cnf);

  pybind11::class_<model>(m, "Model")
    .def("__repr__", &model::to_string)
    .def("__getitem__", &model::operator[])
    .def("__getitem__", [](model& m, pyop_ref& r) {
      return m[static_cast<uint32_t>(r.get_id())];
    });

  pybind11::class_<check_sat_result>(m, "CheckSatResult")
    .def("__repr__", &check_sat_result::to_string)
    .def(pybind11::self == pybind11::self);

  m.attr("sat") = check_sat_result(10);
  m.attr("unsat") = check_sat_result(20);
  m.attr("unknown") = check_sat_result(0);

  pybind11::class_<solver>(m, "Solver")
    .def(pybind11::init<>())
    .def("add",
         [](solver& s, pybind11::args args) {
           for(auto& a : args) {
             auto op = a.cast<pyop_ref>();
             s.add(op);
           }
         })
    .def("check", &solver::check)
    .def_property_readonly("op",
                           [](solver& solver) { return pyop_ref(solver.op()); })
    .def("model", &solver::model);

  m.def("Bool", [](const std::string& name) -> pyop_ref {
    return get_variable_from_global_handle(name);
  });
  m.def("Bools", [](const std::string& names) -> std::vector<pyop_ref> {
    auto vars = get_variables_from_global_handle(names);
    std::vector<pyop_ref> r;
    r.reserve(vars.size());
    std::transform(vars.begin(),
                   vars.end(),
                   std::back_inserter(r),
                   [](op_ref& r) -> pyop_ref { return pyop_ref(r); });
    return r;
  });
  m.def("Exists", pyop_bin_wrapper(helpers::binop<op_type::Exists>));
  m.def("Forall", pyop_bin_wrapper(helpers::binop<op_type::Forall>));
  m.def("Equivalence", pyop_bin_wrapper(helpers::binop<op_type::Equi>));
  m.def("Implies", pyop_bin_wrapper(helpers::binop<op_type::Impl>));
  m.def("Lpmi", pyop_bin_wrapper(helpers::binop<op_type::Lpmi>));
  m.def("Xor", pyop_bin_wrapper(helpers::binop<op_type::Xor>));
  m.def("And", pyop_bin_wrapper(helpers::binop<op_type::And>));
  m.def("Or", pyop_bin_wrapper(helpers::binop<op_type::Or>));
  m.def("Not", pyop_un_wrapper(helpers::unop<op_type::Not>));

  m.def(
    "prenex",
    [](pyop_ref o,
       transform::prenex_quantifier::kind kind,
       const std::string& animation_path) {
      return pyop_ref(helpers::prenex(o, kind, animation_path));
    },
    pybind11::arg("op"),
    pybind11::arg("kind") = transform::prenex_quantifier::Eup_Aup,
    pybind11::arg("animation_path") = "");

  m.def(
    "bsolve",
    [](pyop_ref o,
       std::string solver,
       std::vector<std::string> args) -> std::optional<pyop_ref> {
      auto r = helpers::solve_sat(o, solver, args);
      if(r) {
        return pyop_ref(*r);
      } else {
        return std::nullopt;
      }
    },
    pybind11::arg("op"),
    pybind11::arg("solver") = "kissat",
    pybind11::arg("args") = std::vector<std::string>({ "-q" }));

  m.def(
    "solve",
    [](pyop_ref o, std::string solver, std::vector<std::string> args)
      -> std::optional<std::unordered_map<pyop_ref, bool>> {
      booleguru::expression::op_manager& ops
        = booleguru::expression::literals::handle::global().get_op_manager();
      auto r = helpers::solve_sat_to_resultmap(o, solver, args);
      if(r) {
        std::unordered_map<pyop_ref, bool> m;
        m.reserve(r->size());
        for(auto& e : *r) {
          m[ops[e.first]] = e.second;
        };
        return m;
      } else {
        return std::nullopt;
      }
    },
    pybind11::arg("op"),
    pybind11::arg("solver") = "kissat",
    pybind11::arg("args") = std::vector<std::string>({ "-q" }));
}
