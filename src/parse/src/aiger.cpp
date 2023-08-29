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

constexpr static inline unsigned
convert_idx(unsigned i) {
  return (i == 0 || i == 1) ? i + 1 : (i >> 1u) + 2;
}

static_assert(convert_idx(0) == 1);
static_assert(convert_idx(1) == 2);
static_assert(convert_idx(2) == 3);
static_assert(convert_idx(3) == 3);
static_assert(convert_idx(4) == 4);
static_assert(convert_idx(5) == 4);
static_assert(convert_idx(6) == 5);
static_assert(convert_idx(7) == 5);

result
aiger::parse(bool binary) {
  gates.resize(number_of_and_gates_);
  negated_outputs.resize(number_of_outputs_);
  negated_outputs.assign(number_of_outputs_, false);

  // +2 to support constants
  variables.resize(number_of_inputs_ + number_of_outputs_ + 2);

  // FALSE and TRUE
  variables[0] = 2;
  variables[1] = 1;

  // The format itself is line-based.
  unsigned line_num = 0;
  std::string line_buf;
  std::string symbol;
  while(std::getline(in_, line_buf)) {
    std::istringstream line(line_buf);
    // Three possibilities: A single literal, an AND gate (three literals) or a
    // symbol (one symbol / name and one literal).

    // All variables are normally shifted to the left by 1. The rightmost bit
    // says if the variable is notted or not. In order to map this to fixed TRUE
    // and FALSE constants, the index 0 (which is invalid by itself) is shifted
    // to 1 and remembered to be the constant 1 index in internal variables.
    unsigned int l1 = 0;

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

      remember_symbol(l1 + 2, symbol);
    } else {
      if((line >> l1).fail()) {
        return error("first lit of line was not a readable number");
      }

      // First was a lit! Now we either have a latch or an and gate or just this
      // one lit.
      int l2, l3;
      if((line >> l2).fail()) {
        // We just have a symbol, i.e. an input or output definition (maybe with
        // a not). The type depends on the line we are in.
        if(line_num < number_of_inputs_) {
          // Input Line
          if((l1 >> 1u) > maximum_variable_index_)
            return error(
              fmt::format("variable {} bigger than max variable index {}",
                          l1 >> 1u,
                          maximum_variable_index_));
        } else if(number_of_latches_ > 0 && line_num >= number_of_inputs_ &&
                  line_num <= number_of_inputs_ + number_of_latches_) {
          return error("latches not supported", 4);
        } else if(line_num <
                  number_of_outputs_ + number_of_inputs_ + number_of_latches_) {
          // Output line. Only save the last one.
          output_ = convert_idx(l1);
          // Negation only has to be added if this is not just a constant.
          negate_output_ = output_ > 2 && (l1 & 0b1u) == 0b1u;
        }
        ++line_num;
      } else if((line >> l3).fail()) {
        // We have a latch.
        return error("latches not supported", 4);
      } else {
        unsigned var = (l1 >> 1) - gates_offset();

        unsigned l2_ = l2 & 0b1u;
        unsigned l3_ = l3 & 0b1u;

        l2 = (l2 & ~static_cast<unsigned>(0b1)) >> 1u;
        l3 = (l3 & ~static_cast<unsigned>(0b1)) >> 1u;

        l2 += 2;
        l3 += 2;

        l2 <<= 1u;
        l3 <<= 1u;

        l2 |= l2_;
        l3 |= l3_;

        gates[var].l = l2;
        gates[var].r = l3;
      }
    }
  }

  return build();
}

void
aiger::parse_binary() {
  return error("binary not yet implemented");
}

void
aiger::remember_symbol(unsigned gate, const std::string& symbol) {
  if(gate < number_of_inputs_ + number_of_outputs_ + 2) {
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
    std::stack<uint32_t> ops;

    auto visit = [this, &ops](unsigned node) {
      using namespace expression;
      if(node > 0 && node <= 2) {
        // A constant variable! Use booleguru's special constant values.
        op_ref::ref v;
        if(node == 1) {
          // FALSE
          v = ops_->get_id(op(op_type::Var, 2, 0));
        } else {
          // TRUE
          v = ops_->get_id(op(op_type::Var, 1, 0));
        }
        ops.emplace(v);
      } else if(node > 2 && node <= number_of_inputs_ + 2) {
        // Input node! This is just a variable.
        var_ref::ref var_id = variables[node - 1];
        if(var_id == 0)
          var_id = vars_->get_id(variable{ std::to_string(node - 2) });
        op_ref::ref v = ops_->get_id(op(op_type::Var, var_id, 0));
        ops.emplace(v);
      } else if(node >= number_of_inputs_ + 2 + number_of_latches_ +
                          number_of_outputs_ &&
                number_of_and_gates_ > 0) {
        // And gate!
        assert(!ops.empty());
        op_ref::ref r = ops.top();
        ops.pop();
        assert(!ops.empty());
        op_ref::ref l = ops.top();
        ops.pop();

        auto& g = gates[node - gates_offset() - 2];
        if(g.l & 0b1u)
          l = ops_->get_id(op(op_type::Not, l, 0));
        if(g.r & 0b1u)
          r = ops_->get_id(op(op_type::Not, r, 0));

        ops.emplace(ops_->get_id(op(op_type::And, l, r)));
      }

      if(node >= number_of_inputs_ + 2 && node < number_of_inputs_ +
                                                   number_of_latches_ +
                                                   number_of_outputs_ + 2) {
        // Output node! This may be directly a variable or a negated variable,
        // depending on what's saved in negated_outputs.
        if(ops.empty()) {
          var_ref::ref var_id = variables[node - 1];
          if(var_id == 0)
            var_id = vars_->get_id(variable{ std::to_string(node) });
          op_ref::ref v = ops_->get_id(op(op_type::Var, var_id, 0));
          ops.emplace(v);
        }
        if(negated_outputs[node - 1]) {
          auto top = ops.top();
          ops.pop();
          ops.emplace(ops_->get_id(op(op_type::Not, top, 0)));
        }
      }
    };

    // The right shift is required because of the encoding of negations.
    auto llink = [this](unsigned n) {
      if(n >= gates_offset() + 2 && (n - gates_offset() - 2) < gates.size())
        return gates[n - gates_offset() - 2].l >> 1u;
      else
        return 0u;
    };
    auto rlink = [this](unsigned n) {
      if(n >= gates_offset() + 2 && (n - gates_offset() - 2) < gates.size())
        return gates[n - gates_offset() - 2].r >> 1u;
      else
        return 0u;
    };
    util::postorder<unsigned, decltype(llink), decltype(rlink)> traverse(llink,
                                                                         rlink);

    traverse(output_, visit);

    assert(ops.size() > 0);

    auto top = ops.top();
    if(negate_output_) {
      top = ops_->get_id(expression::op(expression::op_type::Not, top, 0));
    }

    return generate_result((*ops_)[top]);
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
  if(number_of_outputs_ != 1) {
    return error("number of outputs may only be 1", 5);
  }

  if(format == "aig") {
    return parse_binary();
  } else if(format == "aag") {
    return parse_ascii();
  }
  return error("Invalid AIGER format! Needs to be 'aig' or 'aag'");
}
}
