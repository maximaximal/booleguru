#include <sstream>

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

  // The format itself is line-based.
  std::string line_buf;
  std::string symbol;
  while(std::getline(in_, line_buf)) {
    std::istringstream line{ line_buf };
    // Three possibilities: A single literal, an AND gate (three literals) or a
    // symbol (one symbol / name and one literal).
    int l1;
    if((line >> l1).fail()) {
      // First must be a symbol if integer parsing failed!
      if((line >> symbol).fail())
        return error("line does start with neither a symbol nor a literal", 3);
      int lit;
      if((line >> lit).fail())
        return error("no literal after symbol");

      remember_symbol(lit, symbol);
    } else {
      // First was a lit! Now we either have a latch or an and gate.
      int l2, l3;
      if((line >> l2).fail())
        return error("first literal was not followed up by a second literal");
      if((line >> l3).fail())
        return error("second literal was not followed up by a third literal");
    }
  }

  return error("not fully implemented yet");
}
result
aiger::parse_aig() {
  return error("AIGER files in 'aig' not supported yet.");
}

int32_t
aiger::remember_symbol(int variable, const std::string& symbol) {
  int32_t id = vars_->get_id(expression::variable{ symbol });
  variable_table_[variable] = id;
  return id;
}

result
aiger::operator()() {
  std::string format;
  in_ >> format;
  if(format == "aig") {
    return parse_aig();
  } else if(format == "aag") {
    return parse_aag();
  }
  return error("Invalid AIGER format! Needs to be 'aig' or 'aag'");
}
}
