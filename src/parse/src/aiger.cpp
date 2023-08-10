#include <ios>
#include <sstream>
#include <stack>

#include <fmt/format.h>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/parse/aiger.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/postorder.hpp>

template<>
struct fmt::formatter<booleguru::parse::aiger::gate> {
  constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }
  template<typename Context>
  constexpr auto format(booleguru::parse::aiger::gate const& g,
                        Context& ctx) const {
    return format_to(ctx.out(), "(and {} {})", g.l, g.r);
  }
};

using fmt::println;

namespace booleguru::parse {

result
aiger::parse_aag(std::istringstream& header) {
  // The header contains all required meta-information.
  if((header >> maximum_variable_index_).fail())
    return error("could not extract maximum variable index", 2);
  if((header >> number_of_inputs_).fail())
    return error("could not extract number of inputs", 2);
  if((header >> number_of_latches_).fail())
    return error("could not extract number of latches", 2);
  if((header >> number_of_outputs_).fail())
    return error("could not extract number of outputs", 2);
  if((header >> number_of_and_gates_).fail())
    return error("could not extract number of and gates", 2);

  if(number_of_latches_ > 0) {
    return error("latches not supported", 4);
  }

  gates.resize(number_of_and_gates_);
  negated_outputs.resize(number_of_outputs_);
  variables.resize(number_of_inputs_ + number_of_outputs_);

  // The format itself is line-based.
  std::string line_buf;
  std::string symbol;
  while(std::getline(in_, line_buf)) {
    std::istringstream line(line_buf);
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
        unsigned var = l1 >> 1u;
        if(var > maximum_variable_index_)
          return error(
            fmt::format("variable {} bigger than max variable index {}",
                        var,
                        maximum_variable_index_));
        if(var <= number_of_inputs_) {// Don't care about input lines, they are
                                      // redundant in this interpretation of
                                      // AIGER.
        } else if(var <= number_of_inputs_ + number_of_latches_) {// latch lines
          return error("latches not supported", 4);
        } else if(var <= number_of_inputs_ + number_of_latches_ +
                           number_of_outputs_) {// output lines
          negated_outputs[var - outputs_offset() - 1] = (l1 & 0b1u) == 0b1u;
        }
      } else if((line >> l3).fail()) {
        // We have a latch.
        return error("latches not supported", 4);
      } else {
        unsigned var = (l1 >> 1u) - gates_offset();
        gates[var].l = l2;
        gates[var].r = l3;
      }
    }
  }

  return build();
}
result
aiger::parse_aig() {
  return error("AIGER files in 'aig' not supported yet.");
}

void
aiger::remember_symbol(unsigned gate, const std::string& symbol) {
  if(gate < number_of_inputs_ + number_of_outputs_) {
    uint32_t var_id = vars_->get_id(expression::variable{ symbol });
    variables[gate] = var_id;
  }
}

result
aiger::build() {
  if(number_of_outputs_ == 0) {
    return error("number of outputs may not be 0");
  } else if(number_of_outputs_ == 1) {
    // This is the archetypical formula of one output and many inputs. Build it
    // like a boolean expression by operating on two stacks.
    unsigned root = outputs_offset() + 1;
    std::stack<uint32_t> ops;

    auto visit = [this, &ops, root](unsigned node) {
      using namespace expression;
      if(node < number_of_inputs_ + 1) {
        // Input node! This is just a variable.
        var_ref::ref var_id = variables[node - 1];
        if(var_id == 0)
          var_id = vars_->get_id(variable{ std::to_string(var_id) });
        op_ref::ref v = ops_->get_id(op(op_type::Var, var_id, 0));
        ops.emplace(v);
      } else if(node <
                number_of_inputs_ + number_of_latches_ + number_of_outputs_) {
        // Output node! This may be directly a variable or a negated variable,
        // depending on what's saved in negated_outputs.
        assert(node == root);
        if(negated_outputs[node - 1]) {
          ops.emplace(ops_->get_id(op(op_type::Not, ops.top(), 0)));
        } else {
          // Nothing needed - the top op stays as it is.
        }
      } else {
        // And gate!
        op_ref::ref r = ops.top();
        ops.pop();
        op_ref::ref l = ops.top();
        ops.pop();

        auto& g = gates[node - gates_offset()];
        if(g.l & 0b1u)
          l = ops_->get_id(op(op_type::Not, l, 0));
        if(g.r & 0b1u)
          r = ops_->get_id(op(op_type::Not, r, 0));

        ops.emplace(ops_->get_id(op(op_type::And, l, r)));
      }
    };

    // The right shift is required because of the encoding of negations.
    auto llink = [this](unsigned n) {
      if(n >= gates_offset())
        return gates[n - gates_offset()].l >> 1u;
      else
        return 0u;
    };
    auto rlink = [this](unsigned n) {
      if(n >= gates_offset())
        return gates[n - gates_offset()].r >> 1u;
      else
        return 0u;
    };
    util::postorder<unsigned, decltype(llink), decltype(rlink)> traverse(llink,
                                                                         rlink);
    traverse(root, visit);

    assert(ops.size());

    return generate_result((*ops_)[ops.top()]);
  } else {
    return error("number of outputs > 1 not supported yet");
  }
  return error("not implemented yet");
}

result
aiger::operator()() {
  std::string header_line;
  if(!std::getline(in_, header_line))
    return error("cannot read first line");
  std::istringstream header(header_line);
  std::string format;
  if((header >> format).fail())
    return error("cannot read format from first line");

  if(format == "aig") {
    return parse_aig();
  } else if(format == "aag") {
    return parse_aag(header);
  }
  return error("Invalid AIGER format! Needs to be 'aig' or 'aag'");
}
}
