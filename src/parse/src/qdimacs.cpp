#include <sstream>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/parse/qdimacs.hpp>
#include <booleguru/parse/result.hpp>

#define CHECK_OR_RETURN(LINE, MESSAGE) \
  do {                                 \
    if(!LINE) {                        \
      column_ = LINE.tellg();          \
      return error(MESSAGE);           \
    }                                  \
  } while(false)

namespace booleguru::parse {
bool
qdimacs::read_header(std::istringstream& line) {
  std::string p = "";
  std::string cnf = "";
  line >> p;
  if(!line || p != "p")
    return false;
  line >> cnf;
  if(!line || cnf != "cnf")
    return false;
  line >> varcount_;
  if(!line)
    return false;
  line >> clausecount_;
  if(!line)
    return false;
  return true;
}

result
qdimacs::operator()() {
  using expression::op;
  using expression::op_ref;
  using expression::op_type;
  using expression::variable;

  std::string line_str;
  op_ref expr = op_ref();

  while(std::getline(in_, line_str)) {
    std::istringstream line(line_str);

    switch(phase_) {
      case header:
        if(!read_header(line)) {
          column_ = line.tellg();
          return error("Could not read header!");
        }
        phase_ = prefix_and_matrix;
        break;
      case prefix_and_matrix:
        if(line_str.size() > 0) {
          if(line_str[0] == 'e') {
            char e;
            line >> e;
            std::string var;
            do {
              line >> var;
              CHECK_OR_RETURN(
                line, "could not read existentially quantified variable");
              if(var == "0") {
                break;
              } else {
                quantified_.push_back(std::make_pair(exists, var));
              }
            } while(line && var != "0");
          } else if(line_str[0] == 'a') {
            char a;
            line >> a;
            std::string var;
            do {
              line >> var;
              CHECK_OR_RETURN(line,
                              "could not read universally quantified variable");
              if(var == "0") {
                break;
              } else {
                quantified_.push_back(std::make_pair(forall, var));
              }
            } while(line && var != "0");
          } else {
            std::string var;
            line >> var;
            op_ref ex = ops_->get(
              op(op_type::Var, vars_->get(variable{ var }).get_id(), 0));
            CHECK_OR_RETURN(line, "could not read first variable");
            do {
              line >> var;
              if(var == "0")
                break;
              ex = ex ||
                   ops_->get(
                     op(op_type::Var, vars_->get(variable{ var }).get_id(), 0));
              CHECK_OR_RETURN(line, "could not read variable");
            } while(line && var != "0");
            expr = expr.valid() ? expr && ex : ex;
          }
        }
        break;
    }

    ++line_;
  }

  for(auto it = quantified_.rbegin(); it != quantified_.rend(); ++it) {
    auto& [quant, str] = *it;
    op_type q = quant == exists ? op_type::Exists : op_type::Forall;
    expr =
      ops_->get(op(q, vars_->get(variable{ str }).get_id(), expr.get_id()));
  }

  return generate_result(expr);
}
}
