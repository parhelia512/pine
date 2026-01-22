#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "include/exprs.h"
#include "include/lexer.h"
#include "include/types.h"
#include "include/stb_ds.h"

Expr expr_none(void) {
    return (Expr){.kind = EkNone};
}

Expr expr_true(size_t index) {
    return (Expr){
        .kind = EkTrue,
        .cursors_idx = index,
        .type = type_bool(TYPEVAR, index),
    };
}

Expr expr_false(size_t index) {
    return (Expr){
        .kind = EkFalse,
        .cursors_idx = index,
        .type = type_bool(TYPEVAR, index),
    };
}

Expr expr_null(Type t, size_t index) {
    return (Expr){
        .kind = EkNull,
        .cursors_idx = index,
        .type = t,
    };
}

Expr expr_type(Type v, size_t index) {
    return (Expr){
        .kind = EkType,
        .cursors_idx = index,
        .type = (Type){
            .kind = TkTypeId,
        },
        .type_expr = v,
    };
}

Expr expr_intlit(const char *s, Type t, size_t index) {
    return (Expr){
        .kind = EkIntLit,
        .cursors_idx = index,
        .type = t,
        .lit = s,
    };
}

Expr expr_floatlit(const char *s, Type t, size_t index) {
    return (Expr){
        .kind = EkFloatLit,
        .cursors_idx = index,
        .type = t,
        .lit = s,
    };
}

Expr expr_charlit(const char *s, size_t index) {
    return (Expr){
        .kind = EkCharLit,
        .cursors_idx = index,
        .type = type_char(TYPEVAR, index),
        .lit = s,
    };
}

Expr expr_strlit(const char *s, size_t index) {
    return (Expr){
        .kind = EkStrLit,
        .cursors_idx = index,
        .type = type_string(TkUntypedString, TYPEVAR, index),
        .lit = s,
    };
}

Expr expr_ident(const char *v, Type t, size_t index) {
    return (Expr){
        .kind = EkIdent,
        .cursors_idx = index,
        .type = t,
        .ident = v,
    };
}

Expr expr_literal(Literal v, Type t, size_t index) {
    return (Expr){
        .kind = EkLiteral,
        .cursors_idx = index,
        .type = t,
        .literal = v,
    };
}

Expr expr_fncall(FnCall v, Type t, size_t index) {
    return (Expr){
        .kind = EkFnCall,
        .cursors_idx = index,
        .type = t,
        .fncall = v,
    };
}

Expr expr_binop(Binop v, Type t, size_t index) {
    return (Expr){
        .kind = EkBinop,
        .cursors_idx = index,
        .type = t,
        .binop = v,
    };
}

Expr expr_unop(Unop v, Type t, size_t index) {
    return (Expr){
        .kind = EkUnop,
        .cursors_idx = index,
        .type = t,
        .unop = v,
    };
}

Expr expr_group(Arr(Expr) v, Type t, size_t index) {
    return (Expr){
        .kind = EkGrouping,
        .cursors_idx = index,
        .type = t,
        .group = v,
    };
}

Expr expr_range(RangeLit v, Type t, size_t index) {
    return (Expr){
        .kind = EkRangeLit,
        .cursors_idx = index,
        .type = t,
        .rangelit = v,
    };
}

Expr expr_fieldaccess(FieldAccess v, Type t, size_t index) {
    return (Expr){
        .kind = EkFieldAccess,
        .cursors_idx = index,
        .type = t,
        .fieldacc = v,
    };
}

Expr expr_arrayindex(ArrayIndex v, Type t, size_t index) {
    return (Expr){
        .kind = EkArrayIndex,
        .cursors_idx = index,
        .type = t,
        .arrayidx = v,
    };
}

Expr expr_arrayslice(ArraySlice v, Type t, size_t index) {
    return (Expr){
        .kind = EkArraySlice,
        .cursors_idx = index,
        .type = t,
        .arrayslice = v,
    };
}
