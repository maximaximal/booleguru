#include <ios>
#include <sstream>
#include <stack>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/parse/aiger.hpp>
#include <booleguru/parse/result.hpp>

namespace booleguru::parse {
result
aiger::parse_aag() {
  // The header contains all required meta-information.
  if((in_ >> maximum_variable_index_).fail())
    return error("could not extract maximum variable index", 2);
  if((in_ >> number_of_inputs_).fail())
    return error("could not extract number of inputs", 2);
  if((in_ >> number_of_latches_).fail())
    return error("could not extract number of latches", 2);
  if((in_ >> number_of_outputs_).fail())
    return error("could not extract number of outputs", 2);
  if((in_ >> number_of_and_gates_).fail())
    return error("could not extract number of and gates", 2);

  if(number_of_latches_ > 0) {
    return error("latches not supported", 4);
  }

  // The format itself is line-based.
  std::string line_buf;
  std::string symbol;
  while(std::getline(in_, line_buf) && line_buf.length() > 0) {
    std::istringstream line{ line_buf };
    // Three possibilities: A single literal, an AND gate (three literals) or a
    // symbol (one symbol / name and one literal).
    int l1 = 0;

    if(line_buf[0] == 'i' || line_buf[0] == 'l' || line_buf[0] == 'o') {
      // Must be a symbol!
      char ilo = line.get();
      if((line >> l1).fail())
        return error("symbol ilo not followed by number", 3);
      if((line >> symbol).fail())
        return error("no symbol after ilo and number");

      // Make the literal fit according to ilo.
      switch(ilo) {
        case 'o':
          l1 += number_of_latches_;
          [[fallthrough]];
        case 'l':
          l1 += number_of_inputs_;
          [[fallthrough]];
        case 'i':
          break;
        default:
          return error("invalid value of ilo");
      }

      remember_symbol(l1, symbol);
    } else {
      if((line >> l1).fail()) {
        return error("first lit of line was not a readable number");
      }

      // First was a lit! Now we either have a latch or an and gate or just this
      // one lit.
      int l2, l3;
      if((line >> l2).fail()) {
        // We just have a symbol, i.e. an input or output definition (maybe with
        // a not).
      }
      if((line >> l3).fail()) {
        // We have a latch.
        return error("latches not supported", 4);
      } else {
        // We have an AND gate.
      }
    }
  }

  return error("not fully implemented yet");
}
result
aiger::parse_aig() {
  return error("AIGER files in 'aig' not supported yet.");
}

void
aiger::remember_symbol(unsigned gate, const std::string& symbol) {
  // Remembering symbols is very annoying, as the symbol table is only trailing
  // at the end. This means we have to save up all the symbols we want to swap
  // and finally swap all variables in one go.
  uint32_t var_id = vars_->get_id(expression::variable{ symbol });
  uint32_t varop_id =
    ops_->get_id(expression::op{ expression::op_type::Var, var_id, 0 });
  assert(gates_[gate].is_var());
  unsigned notted = gates_[gate].v.notted();
  gates_[gate].v.var_ = (varop_id << 1) | notted;
}

result
aiger::build() {
  if(number_of_outputs_ == 0) {
    return error("number of outputs may not be 0");
  } else if(number_of_outputs_ == 1) {
    // This is the archetypical formula of one output and many inputs. Build it
    // like a boolean expression by operating on two stacks.
    unsigned root = outputs_offset();
    std::stack<unsigned> s;
    std::stack<expression::op_ref::ref> o;
    s.emplace(root);
    return error("not implemented yet");
    while(!s.empty()) {
    }
  } else {
    return error("number of outputs > 1 not supported yet");
  }
  return error("not implemented yet");
}

result
aiger::operator()() {
  std::string format;
  in_ >> format;
  in_ >> std::skipws;
  if(format == "aig") {
    return parse_aig();
  } else if(format == "aag") {
    return parse_aag();
  }
  return error("Invalid AIGER format! Needs to be 'aig' or 'aag'");
}
}
