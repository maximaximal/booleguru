#include <cstdlib>
#include <stdexcept>
#include <unistd.h>

#include <sys/types.h>
#include <sys/wait.h>

#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>

#include <booleguru/transform/minimize_failing.hpp>

namespace booleguru::transform {
minimize_failing::minimize_failing() {}
minimize_failing::~minimize_failing() {}

expression::op_ref
minimize_failing::operator()(expression::op_ref o) {
  expression::op_id id = o.get_id();

  int exitstatus = 0;
  bool first_run = true;

  for(expression::op_id test_id = id; test_id > 0; --test_id) {
    pid_t p = fork();
    if(p < 0) {
      throw std::runtime_error("Could not fork!");
    }

    if(p == 0) {
      std::cout.setstate(std::ios_base::failbit);
      return o.get_mgr()[test_id];
    } else {
      int status = 0;
      waitpid(p, &status, 0);
      if(first_run) {
        first_run = false;
        exitstatus = WEXITSTATUS(status);
      } else {
        if(WEXITSTATUS(status) != exitstatus) {
          std::cerr << o.get_mgr()[test_id + 1] << std::endl;
          exit(0);
        } else {
        }
      }
    }
  }
  return expression::op_ref();
}
}
