#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <booleguru/py/python-context.hpp>

#include <booleguru/lua/binding-helpers.hpp>

using namespace booleguru;
using namespace booleguru::expression;
using namespace booleguru::lua;
using namespace booleguru::lua::helpers;

PYBIND11_MODULE(pybooleguru, m) {
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

  pybind11::class_<op_ref>(m, "op")
    .def_property_readonly("t", &helpers::get_op_ref_type)
    .def_property_readonly("id", &helpers::get_op_id)
    .def_property_readonly("l", &helpers::get_op_left)
    .def_property_readonly("r", &helpers::get_op_right)

    .def("__repr__", &op_ref::to_string)
    .def(pybind11::self == pybind11::self)
    .def(pybind11::self != pybind11::self)

    .def("rename", &helpers::rename)
    .def("rename_map", &helpers::rename_map)

    .def(
      "__and__",
      [](op_ref& l, op_ref& r) { return l && r; },
      pybind11::is_operator())
    .def(
      "__or__",
      [](op_ref& l, op_ref& r) { return l || r; },
      pybind11::is_operator())
    .def(
      "__neg__", [](op_ref& l) { return !l; }, pybind11::is_operator())
    .def(
      "__xor__",
      [](op_ref& l, op_ref& r) { return l ^ r; },
      pybind11::is_operator())

    .def_property_readonly("v", &helpers::get_op_varop_v)
    .def_property_readonly("q", &helpers::get_op_varop_q)

    .def_property_readonly("and_inside", &helpers::get_op_and_inside)
    .def_property_readonly("is_ors", &helpers::get_op_is_ors)
    .def_property_readonly("is_cnf", &helpers::get_op_is_cnf);

  m.def("v", get_variable_from_global_handle);
  m.def("exists", &helpers::binop<op_type::Exists>);
  m.def("forall", &helpers::binop<op_type::Forall>);
  m.def("equi", &helpers::binop<op_type::Equi>);
  m.def("impl", &helpers::binop<op_type::Impl>);
  m.def("lpmi", &helpers::binop<op_type::Lpmi>);
  m.def("xor", &helpers::binop<op_type::Xor>);
  m.def("b_and", &helpers::binop<op_type::And>);
  m.def("b_or", &helpers::binop<op_type::Or>);
  m.def("b_not", &helpers::unop<op_type::Not>);

  m.def("prenex",
        &helpers::prenex,
        pybind11::arg("op"),
        pybind11::arg("kind") = transform::prenex_quantifier::Eup_Aup,
        pybind11::arg("animation_path") = "");
}

namespace booleguru::py {
void
register_booleguru_types() {}
}
