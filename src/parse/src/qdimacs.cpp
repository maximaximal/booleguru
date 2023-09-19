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
  int highest_var = 0;

  auto var_offset = expression::var_manager::LITERAL_VEC;

  while(std::getline(in_, line_str)) {
    std::istringstream line(line_str);

    switch(phase_) {
      case header:
        if(line.peek() == 'c') {
          char c;
          do {
            line >> c;
          } while(line);
        } else {
          if(!read_header(line)) {
            column_ = line.tellg();
            return error("Could not read header!");
          }
          phase_ = prefix_and_matrix;
        }
        break;
      case prefix_and_matrix:
        if(line_str.size() > 0) {
          if(line_str[0] == 'e') {
            char e;
            line >> e;
            int var;
            do {
              line >> var;
              CHECK_OR_RETURN(
                line, "could not read existentially quantified variable");
              if(var == 0) {
                break;
              } else {
                quantified_.push_back(std::make_pair(exists, var + var_offset));
              }
            } while(line && var != 0);
          } else if(line_str[0] == 'a') {
            char a;
            line >> a;
            int var;
            do {
              line >> var;
              CHECK_OR_RETURN(line,
                              "could not read universally quantified variable");
              if(var == 0) {
                break;
              } else {
                quantified_.push_back(std::make_pair(forall, var + var_offset));
              }
            } while(line && var != 0);
          } else {
            int var;
            line >> var;
            int abs_var = abs(var);
            if(abs_var > highest_var)
              highest_var = abs_var;
            op_ref ex = ops_->get(op(op_type::Var, abs_var + var_offset, 0));
            if(var < 0)
              ex = !ex;
            CHECK_OR_RETURN(line, "could not read first variable");
            do {
              line >> var;
              if(var == 0)
                break;
              abs_var = abs(var);
              if(abs_var > highest_var)
                highest_var = abs_var;
              auto next = ops_->get(op(op_type::Var, abs_var + var_offset, 0));
              if(var < 0)
                next = !next;
              ex = ex || next;
              CHECK_OR_RETURN(line, "could not read variable");
            } while(line && var != 0);
            expr = expr.valid() ? expr && ex : ex;
          }
        }
        break;
    }

    ++line_;
  }

  for(int i = 1; i <= highest_var; ++i) {
    int id = vars_->get_id(variable{ std::to_string(i) })
             - expression::var_manager::LITERAL_VEC;
    assert(id == i);
  }

  for(auto it = quantified_.rbegin(); it != quantified_.rend(); ++it) {
    auto& [quant, v] = *it;
    op_type q = quant == exists ? op_type::Exists : op_type::Forall;
    auto v_op = ops_->get_id(
      op(op_type::Var, v + expression::var_manager::LITERAL_VEC, 0));
    expr = ops_->get(op(q, v_op, expr.get_id()));
  }

  return generate_result(expr);
}
}
