#pragma once

#include <string>
#include <string_view>
#include <vector>

#include <booleguru/util/type.hpp>

#include "id.hpp"
#include "op_manager.hpp"
#include "var_manager.hpp"

namespace booleguru::expression {
// Abstract tool to build expression graphs. The base class has no effects and
// returns 0 for all cases.
class expression_graph {
  public:
  expression_graph() = default;

  using ref = op_id;

  virtual ref not_(ref a) {
    (void)a;
    return 0;
  };
  virtual ref and_(ref a, ref b) {
    (void)a;
    (void)b;
    return 0;
  };
  virtual ref or_(ref a, ref b) {
    (void)a;
    (void)b;
    return 0;
  };
  virtual ref xor_(ref a, ref b) {
    (void)a;
    (void)b;
    return 0;
  };
  virtual ref impl_(ref a, ref b) {
    (void)a;
    (void)b;
    return 0;
  };
  virtual ref lpmi_(ref a, ref b) {
    (void)a;
    (void)b;
    return 0;
  };
  virtual ref equi_(ref a, ref b) {
    (void)a;
    (void)b;
    return 0;
  };
  virtual ref forall_(ref a, ref b) {
    (void)a;
    (void)b;
    return 0;
  };
  virtual ref exists_(ref a, ref b) {
    (void)a;
    (void)b;
    return 0;
  };
  virtual ref var_(var_id var, uint16_t q, uint16_t v) {
    (void)var;
    (void)q;
    (void)v;
    return 0;
  };
  virtual ref bottom_() { return 0; }
  virtual ref top_() { return 0; }
  virtual ref fennel_(std::string_view code, ref last_op = 0) {
    (void)code;
    (void)last_op;
    return 0;
  }
  virtual ref fennel_binop_(std::string_view code, ref l = 0, ref r = 0) {
    (void)code;
    (void)l;
    (void)r;
    return 0;
  }
  virtual ref file_(std::string_view path, util::type type) {
    (void)type;
    (void)path;
    return 0;
  }

  virtual var_id variable(std::string name) {
    (void)name;
    return 0;
  }
};

class op_graph : public expression_graph {
  template<op_type t>
  inline ref unop(ref a) {
    return ops.get_id(op(t, a, 0));
  }
  template<op_type t>
  inline ref binop(ref a, ref b) {
    return ops.get_id(op(t, a, b));
  }

  protected:
  op_manager& ops;

  public:
  op_graph(op_manager& ops)
    : ops(ops) {}

  virtual ref not_(ref a) final override { return unop<op_type::Not>(a); };
  virtual ref and_(ref a, ref b) final override {
    return binop<op_type::And>(a, b);
  };
  virtual ref or_(ref a, ref b) final override {
    return binop<op_type::Or>(a, b);
  };
  virtual ref xor_(ref a, ref b) final override {
    return binop<op_type::Xor>(a, b);
  };
  virtual ref impl_(ref a, ref b) final override {
    return binop<op_type::Impl>(a, b);
  };
  virtual ref lpmi_(ref a, ref b) final override {
    return binop<op_type::Lpmi>(a, b);
  };
  virtual ref equi_(ref a, ref b) final override {
    return binop<op_type::Equi>(a, b);
  };
  virtual ref forall_(ref a, ref b) final override {
    return binop<op_type::Forall>(a, b);
  };
  virtual ref exists_(ref a, ref b) final override {
    return binop<op_type::Exists>(a, b);
  };
  virtual ref var_(var_id var, uint16_t q, uint16_t v) final override {
    return ops.get_id(op(op_type::Var, var, q, v));
  };
  virtual ref bottom_() final override { return ops.bottom().get_id(); }
  virtual ref top_() final override { return ops.top().get_id(); }
  virtual var_id variable(std::string name) final override {
    return ops.vars().get_id(expression::variable{ std::move(name) });
  }
};

class fuzz_file_graph : public expression_graph {
  size_t number_of_fuzz_files_ = 0;

  public:
  fuzz_file_graph() {}

  size_t number_of_fuzz_files() const { return number_of_fuzz_files_; }

  virtual ref file_(std::string_view path, util::type type) final override {
    if(type == util::type::fuzz || path.ends_with(".fuzz") || path == "fuzz") {
      ++number_of_fuzz_files_;
    }
    return 0;
  }
};
}
