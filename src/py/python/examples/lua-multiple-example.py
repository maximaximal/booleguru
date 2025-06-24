from pybooleguru import *

lua_ctx = make_lua_context()

def run_lua(code):
    mgr = make_op_manager()
    return dimacs_from_lua(lua_ctx, mgr, code)

lua_code = 'return b_and(b_var("a"), b_var("b"))'

# First invocation takes a bit because of creating the Lua context
print(run_lua(lua_code))

# Later invocations are more immediate
print(run_lua(lua_code))

print(run_lua(lua_code))
