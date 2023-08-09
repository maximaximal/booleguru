#include <ios>
#include <sstream>
#include <stack>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/parse/aiger.hpp>
#include <booleguru/parse/result.hpp>

template<>
struct fmt::formatter<booleguru::parse::aiger::gate> {
  constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }
  template<typename Context>
  constexpr auto format(booleguru::parse::aiger::gate const& g,
                        Context& ctx) const {
    char sign = ' ';
    if(g.inverted)
      sign = '-';

    if(g.is_and())
      return format_to(ctx.out(), "{}(and {} {})", sign, g.l, g.r);
    else
      return format_to(ctx.out(), "{}(var)", sign);
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

  gates_.resize(maximum_variable_index_ + 1);

  // The format itself is line-based.
  std::string line_buf;
  std::string symbol;
  while(std::getline(in_, line_buf)) {
    println("Line: {}", line_buf);
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
        if(var <= number_of_inputs_) {// Input line
          gates_[var].inverted = (l1 & 0b1u) == 0b1u;
        } else if(var <= number_of_inputs_ + number_of_latches_) {// latch lines
          return error("latches not supported", 4);
        } else if(var <= number_of_inputs_ + number_of_latches_ +
                           number_of_outputs_) {// output lines
          gates_[var].inverted = (l1 & 0b1u) == 0b1u;
        }
      } else if((line >> l3).fail()) {
        // We have a latch.
        return error("latches not supported", 4);
      } else {
        unsigned var = l1 >> 1u;
        gates_[var].l = l2;
        gates_[var].r = l3;
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
  // Remembering symbols is very annoying, as the symbol table is only trailing
  // at the end. This means we have to save up all the symbols we want to swap
  // and finally swap all variables in one go.
  uint32_t var_id = vars_->get_id(expression::variable{ symbol });
  gates_[gate].var_id = var_id;
}

result
aiger::build() {
  if(number_of_outputs_ == 0) {
    return error("number of outputs may not be 0");
  } else if(number_of_outputs_ == 1) {
    // This is the archetypical formula of one output and many inputs. Build it
    // like a boolean expression by operating on two stacks.
    unsigned root = outputs_offset() + 1;
    std::stack<unsigned> s;
    s.emplace(root);
    std::cout << "Root: " << root << ", Gate size: " << gates_.size()
              << std::endl;

    println("Gates: {}", fmt::join(gates_, ", "));

    unsigned prev = 0;
    while(!s.empty()) {
      unsigned current = s.top();
      s.pop();
      println("Current: {}", current);
      if(!prev || left(prev) == current || right(prev) == current) {
        if(auto l = left(current))
          s.emplace(l >> 1u);
        else if(auto r = right(current))
          s.emplace(r >> 1u);
        else {
          s.pop();

          // visit current
          std::cout << current << std::endl;
        }
      } else if(left(current) == prev) {
        if(auto r = right(current)) {
          s.emplace(r >> 1u);
        } else {
          s.pop();

          // visit current
          std::cout << current << std::endl;
        }
      } else if(right(current) == prev) {
        s.pop();

        std::cout << current << std::endl;
      }
      prev = current;
    }
    return error("not implemented yet");
  } else {
    return error("number of outputs > 1 not supported yet");
  }
  return error("not implemented yet");
}

unsigned
aiger::left(unsigned var) const {
  assert(var < gates_.size());
  if(gates_[var].is_and())
    return gates_[var].l;
  else
    return 0;
}

unsigned
aiger::right(unsigned var) const {
  assert(var < gates_.size());
  if(gates_[var].is_and())
    return gates_[var].r;
  else
    return 0;
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
