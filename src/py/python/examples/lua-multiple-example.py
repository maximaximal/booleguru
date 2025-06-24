from pybooleguru import *

lua_ctx = make_lua_context()

def run_lua(code):
    mgr = make_op_manager()
    return dimacs_from_lua(lua_ctx, mgr, code)
