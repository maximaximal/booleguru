#include "booleguru/transform/tseitin.hpp"
#include <booleguru/solve/sat.hpp>
#include <booleguru/transform/output_to_qdimacs.hpp>
#include <booleguru/util/file.hpp>

#include <sstream>
#include <unordered_set>

using namespace booleguru::util;

#if __has_include(<ext/stdio_filebuf.h>)
#include <ext/stdio_filebuf.h>
#define USE_STDIO_FILEBUF
#endif

extern "C" {
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef WIN32
#include <io.h>
#define F_OK 0
#define access _access
#endif

#if __has_include(<sys/wait.h>)
#include <sys/wait.h>
#endif
}

extern char** environ;
#define BUF_SIZE 4096

static inline bool
xcc_sign(int32_t value) {
  uint32_t temp = value;
  temp >>= 31;
  bool sign = !temp;
  return sign;
}

// Taken from the XCC SAT solver implementation and adapted to fit C++.
typedef struct xcc_sat_solver {
  process p;
  unsigned int variables, clauses;
  char* assignments;
} xcc_sat_solver;

static void
xcc_sat_solver_init(xcc_sat_solver* solver,
                    unsigned int variables,
                    unsigned int clauses,
                    char* binary,
                    char* const argv[],
                    char* envp[]) {
  assert(solver);

  pipe(solver->p.infd);
  pipe(solver->p.outfd);
  solver->variables = variables;
  solver->clauses = clauses;

  solver->p.pid = fork();
  if(solver->p.pid) {
    // Parent
    solver->p.infd_handle = fdopen(solver->p.infd[1], "w");
    assert(solver->p.infd_handle);
    solver->p.outfd_handle = fdopen(solver->p.outfd[0], "r");
    assert(solver->p.outfd_handle);

    if(variables > 0) {
      solver->assignments
        = (char*)realloc(solver->assignments, sizeof(char) * (variables + 1));
    } else {
      solver->assignments = nullptr;
    }
  } else {
    // Child
    dup2(solver->p.infd[0], STDIN_FILENO);
    close(solver->p.infd[0]);
    close(solver->p.infd[1]);

    dup2(solver->p.outfd[1], STDOUT_FILENO);
    close(solver->p.outfd[0]);
    close(solver->p.outfd[1]);

    char* argv_null[1] = { NULL };
    if(argv == NULL)
      argv = argv_null;

    char* envp_null[1] = { NULL };
    if(argv == NULL)
      argv = argv_null;

    if(envp == NULL) {
      envp = environ;
    }

    size_t arg_count = 0;
    for(arg_count = 0; argv[arg_count] != NULL; ++arg_count) {
    }

    char* real_argv[arg_count + 2];

    real_argv[0] = binary;
    for(size_t i = 0; i < arg_count; ++i) {
      real_argv[i + 1] = argv[i];
    }
    real_argv[arg_count + 1] = NULL;

    int status = execve(binary, real_argv, envp);
    exit(-1);
  }
}

static void
xcc_sat_solver_destroy(xcc_sat_solver* solver) {
  assert(solver);
  if(solver->assignments) {
    free(solver->assignments);
    solver->assignments = NULL;
  }
}

static void
parse_solver_output(xcc_sat_solver* solver) {
  assert(solver);
  assert(solver->assignments);
  char stack_buf[BUF_SIZE];
  char* buf = stack_buf;
  size_t buf_size = BUF_SIZE;
  size_t len;
  while((len = getline(&buf, &buf_size, solver->p.outfd_handle)) != -1) {
    if(len > 0 && (buf[0] == 'c' || buf[0] == 'r')) {
      continue;// Comment line or result line
    }
    if(len > 0 && (buf[0] == 'v')) {
      char* numbers = buf + 2;
      int ret = 0;
      do {
        int v = 0;
        int pos;
        ret = sscanf(numbers, "%d%n", &v, &pos);
        numbers += pos + 1;
        if(v == 0)
          break;

        uint32_t v_ = abs(v);

        assert(v_ < solver->variables + 1);
        solver->assignments[v_] = xcc_sign(v);

        if(v_ == solver->variables)
          return;
      } while(ret == 1);
    }
  }
}

static void
xcc_sat_solver_add(xcc_sat_solver* solver, int l) {
  assert(solver);
  assert(solver->p.infd_handle);
  if(l == 0) {
    fprintf(solver->p.infd_handle, "0\n");
  } else {
    fprintf(solver->p.infd_handle, "%d ", l);
  }
}

static int
xcc_sat_solver_solve(xcc_sat_solver* solver) {
  fclose(solver->p.infd_handle);
  solver->p.infd_handle = NULL;
  close(solver->p.infd[1]);

  // Wait for pid to finish.

  int status;
  waitpid(solver->p.pid, &status, 0);

  if(WIFEXITED(status)) {
    int exit_code = WEXITSTATUS(status);
    switch(exit_code) {
      case 10:
        parse_solver_output(solver);
        fclose(solver->p.outfd_handle);
        return 10;
      case 20:
        fclose(solver->p.outfd_handle);
        return 20;
      default:
        fclose(solver->p.outfd_handle);
        return exit_code;
    }
  } else {
    throw std::runtime_error("Child SAT solver process had unexpected exit!");
  }
  return 0;
}

namespace booleguru::solve {
sat::sat(std::string solver, std::vector<std::string> args)
  : solver_(solver)
  , solver_args_(args) {
  if(!find_executable(solver.c_str(), solver_path_)) {
    throw std::runtime_error("Could not find SAT solver " + solver + "!");
  }
}
sat::~sat() {
  free_command_and_args();
}

void
sat::free_command_and_args() {
  if(command_)
    free(command_);
  if(!args_.empty()) {
    for(size_t i = 0; i < args_.size(); ++i) {
      free(args_[i]);
    }
    args_.clear();
  }
}

int
sat::solve_with_solver(expression::op_ref& o, xcc_sat_solver& solver) {
  free_command_and_args();

  command_ = strdup(solver_path_.c_str());
  args_.resize(solver_args_.size() + 1);
  args_[solver_args_.size()] = NULL;
  for(size_t i = 0; i < solver_args_.size(); ++i) {
    args_[i] = strdup(solver_args_[i].c_str());
  }

  memset(&solver, 0, sizeof(solver));
  xcc_sat_solver_init(&solver, 0, 0, command_, args_.data(), environ);

  if(o->is_cnf) {
    std::stringstream dummy;
    transform::output_to_qdimacs out(dummy);

    out.serialize_cnf_op(
      o,
      [&solver](int32_t variables, int32_t clauses) {
        solver.variables = variables;
        solver.clauses = clauses;
        solver.assignments = (char*)malloc(variables * sizeof(char) + 1);
        fprintf(solver.p.infd_handle, "p cnf %d %d\n", variables, clauses);
      },
      [&solver](int32_t l) { xcc_sat_solver_add(&solver, l); },
      false /* don't produce mappings */);
  } else {
    // Re-use tseitin and the output stream provided here. Using non-standard
    // APIs that make it more convenient.
    //
    // Inspired from https://stackoverflow.com/a/5253726
#ifdef USE_STDIO_FILEBUF
    int handle = fileno(solver.p.infd_handle);
    __gnu_cxx::stdio_filebuf<char> filebuf(handle, std::ios::out);
    std::ostream os(&filebuf);
    transform::tseitin<transform::output_to_qdimacs> tseitin(os);
    tseitin.mapping_comments(false);
    tseitin(o);

    solver.assignments = (char*)malloc(tseitin.variables() * sizeof(char) + 1);
    solver.variables = tseitin.variables();
    solver.clauses = tseitin.clauses();
#else
    throw(std::runtime_error("Cannot write non-cnf using tseitin transform and "
                             "unsupported platform!"));
#endif
  }

  int res = xcc_sat_solver_solve(&solver);
  return res;
}

std::optional<expression::op_ref>
sat::solve(expression::op_ref o) {
  if(o->is_quant()) {
    throw std::runtime_error("Cannot solve a QBF with a pure SAT solver!");
  }

  xcc_sat_solver solver;
  int res = solve_with_solver(o, solver);

  std::unordered_set<uint32_t> varops;

  expression::op_manager& ops = o.get_mgr();
  if(res == 10) {
    using enum expression::op_type;
    std::stack<uint32_t> s;
    s.emplace(o.get_id());

    int32_t tseitin_id = ops.vars().LITERAL_TSEITIN;

    while(!s.empty()) {
      uint32_t i = s.top();
      s.pop();
      const expression::op& op = ops.getobj(i);
      switch(op.type) {
        // The more complex binops are handled by a tseitin transformation, but
        // to assign results, they have to be traversed.
        case Equi:
          [[fallthrough]];
        case Impl:
          [[fallthrough]];
        case Lpmi:
          [[fallthrough]];
        case Xor:
          [[fallthrough]];
        case Or:
          [[fallthrough]];
        case And:
          s.emplace(op.bin.l);
          s.emplace(op.bin.r);
          break;
        case Forall:
          [[fallthrough]];
        case Exists:
          s.emplace(op.quant.e);
          break;
        case Not:
          s.emplace(op.un.c);
          break;
        case Var:
          op.mark = solver.assignments[op.user_int32];
          if(op.var.v != tseitin_id
             && op.var.v != expression::var_manager::LITERAL_TOP
             && op.var.v != expression::var_manager::LITERAL_BOTTOM) {
            if(op.mark) {
              varops.emplace(i);
            } else {
              varops.emplace((!ops[i]).get_id());
            }
          }
          break;
        case None:
          assert(false);
      }
    }
  }

  xcc_sat_solver_destroy(&solver);

  switch(res) {
    case 10: {
      auto it = varops.begin();
      // In order for the formula to be SAT there has to be SOMETHING in this
      // system.
      assert(it != varops.end());
      expression::op_ref o = ops[*it];
      for(++it; it != varops.end(); ++it) {
        o = o && ops[*it];
      }
      return o;
    }
    case 20:
      return {};
    default:
      return {};
  }
}

std::optional<std::unordered_map<uint32_t, bool>>
sat::solve_resultmap(expression::op_ref o) {
  if(o->is_quant()) {
    throw std::runtime_error("Cannot solve a QBF with a pure SAT solver!");
  }

  xcc_sat_solver solver;
  int res = solve_with_solver(o, solver);

  // TODO(Marcel): 'Implicitly deleted default constructor' Can we use
  //               templating to make a `var_id` map?
  std::unordered_map<uint32_t, bool> assignments;

  expression::op_manager& ops = o.get_mgr();
  if(res == 10) {
    using enum expression::op_type;
    std::stack<uint32_t> s;
    s.emplace(o.get_id());

    int32_t tseitin_id = ops.vars().LITERAL_TSEITIN;

    while(!s.empty()) {
      uint32_t i = s.top();
      s.pop();
      const expression::op& op = ops.getobj(i);
      switch(op.type) {
        case Or:
          [[fallthrough]];
        case And:
          s.emplace(op.bin.l);
          s.emplace(op.bin.r);
          break;
        case Forall:
          [[fallthrough]];
        case Exists:
          s.emplace(op.quant.e);
          break;
        case Not:
          s.emplace(op.un.c);
          break;
        case Var:
          assignments[static_cast<uint32_t>(ops[i].get_id())]
            = solver.assignments[op.user_int32];
          break;
        default:
          assert(false);
          break;
      }
    }
  }

  xcc_sat_solver_destroy(&solver);

  switch(res) {
    case 10:
      return assignments;
    case 20:
      return {};
    default:
      return {};
  }
}
}
