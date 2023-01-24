#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

/** @file

    This file describes the API boundary between common lisp and C. The
    functions are defined in this C file, so that everything may be loaded
    from ECL using C-inline wrapper functions, in turn making the library also
    available from outside the wrapped version.
*/

uint32_t
booleguru_cl_varop(const char* name);

uint32_t
booleguru_cl_vars_prefix(uint32_t opid, const char* prefix);

uint32_t
booleguru_cl_vars_postfix(uint32_t opid, const char* postfix);

uint32_t
booleguru_cl_vars_wrap(uint32_t opid, const char* prefix, const char* postfix);

uint32_t
booleguru_cl_var_rename(uint32_t opid,
                        const char* oldname,
                        const char* newname);

uint32_t
booleguru_cl_optype(uint32_t opid);

uint32_t
booleguru_cl_optype(uint32_t opid);

uint32_t
booleguru_cl_and(uint32_t l, uint32_t r);

uint32_t
booleguru_cl_or(uint32_t l, uint32_t r);

uint32_t
booleguru_cl_equi(uint32_t l, uint32_t r);

uint32_t
booleguru_cl_impl(uint32_t l, uint32_t r);

uint32_t
booleguru_cl_lpmi(uint32_t l, uint32_t r);

uint32_t
booleguru_cl_not(uint32_t l);

uint32_t
booleguru_cl_distribute_to_cnf(uint32_t l);

uint32_t
booleguru_cl_distribute_or(uint32_t l);

uint32_t
booleguru_cl_distribute_not(uint32_t l);

uint32_t
booleguru_cl_distribute_implication(uint32_t l);

uint32_t
booleguru_cl_distribute_equivalence(uint32_t l);

uint32_t
booleguru_cl_prenex_quantifier(uint32_t l);

void
booleguru_cl_print(uint32_t op);

#ifdef __cplusplus
}
#endif
