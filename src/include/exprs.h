#ifndef EXPRS_H
#define EXPRS_H

#include <stddef.h>
#include <stdint.h>
#include "lexer.h"
#include "types.h"

typedef struct Stmnt Stmnt;

typedef enum ExprKind {
    EkNone,

    // types are also exprs
    EkType,

    EkIntLit,
    EkFloatLit,
    EkCharLit,
    EkStrLit,
    EkCstrLit,
    EkRangeLit,
    EkLiteral,

    EkIdent,
    EkFnCall,

    EkBinop,
    EkUnop,

    EkTrue,
    EkFalse,
    EkGrouping,

    EkFieldAccess,
    EkArrayIndex,
    EkArraySlice,

    EkNull,
} ExprKind;

typedef enum LitKind {
    LitkNone,
    LitkExprs,
    LitkVars,
} LitKind;

typedef struct Literal {
    LitKind kind;

    union {
        Arr(Expr) exprs;
        Arr(Stmnt) vars; // VarReassign
    };
} Literal;

typedef struct RangeLit {
    Expr *start;
    Expr *end;
    bool inclusive;
} RangeLit;

typedef struct FieldAccess {
    Expr *accessing;
    Expr *field;
    bool deref;
} FieldAccess;

typedef struct ArrayIndex {
    Expr *accessing;
    Expr *index;
} ArrayIndex;

typedef struct ArraySlice {
    Expr *accessing;
    Expr *slice;
} ArraySlice;

typedef struct FnCall {
    Expr *name;
    LitKind arg_kind;

    union {
        Arr(Expr) exprs;
        Arr(Stmnt) vars; // VarReassign
    } args;
} FnCall;

typedef enum BinopKind {
    BkPlus,
    BkMinus,
    BkDivide,
    BkMultiply,
    BkMod,

    BkLess,
    BkLessEqual,
    BkGreater,
    BkGreaterEqual,
    BkEquals,
    BkInequals,

    BkBitOr,
    BkBitAnd,
    BkBitXor,
    BkLeftShift,
    BkRightShift,

    BkAnd,
    BkOr,
} BinopKind;

// Binary Operation
typedef struct Binop {
    BinopKind kind;

    Expr *left;
    Expr *right;
} Binop;

typedef enum UnopKind {
    UkBitNot,
    UkNot,
    UkNegate,
    UkAddress,
    UkCast,
    UkSizeof,
} UnopKind;

// Unary Operation
typedef struct Unop {
    UnopKind kind;
    Expr *val;
} Unop;

typedef struct Expr {
    ExprKind kind;
    size_t cursors_idx;
    Type type;

    union {
        Type type_expr;

        const char *lit;
        const char *ident;

        Literal literal;
        FnCall fncall;

        Binop binop;
        Unop unop;

        Expr *group;
        RangeLit rangelit;
        FieldAccess fieldacc;
        ArrayIndex arrayidx;
        ArraySlice arrayslice;
    };
} Expr;

Expr expr_none(void);
Expr expr_true(size_t index);
Expr expr_false(size_t index);
Expr expr_null(Type t, size_t index);
Expr expr_type(Type v, size_t index);

Expr expr_intlit(const char *s, Type t, size_t index);
Expr expr_floatlit(const char *s, Type t, size_t index);
Expr expr_charlit(const char *s, size_t index);
Expr expr_strlit(const char *s, size_t index);
Expr expr_cstrlit(const char *s, size_t index);
Expr expr_ident(const char *s, Type t, size_t index);

Expr expr_literal(Literal v, Type t, size_t index);
Expr expr_fncall(FnCall v, Type t, size_t index);
Expr expr_binop(Binop v, Type t, size_t index);
Expr expr_unop(Unop v, Type t, size_t index);
Expr expr_group(Arr(Expr) v, Type t, size_t index);
Expr expr_range(RangeLit v, Type t, size_t index);
Expr expr_fieldaccess(FieldAccess v, Type t, size_t index);
Expr expr_arrayindex(ArrayIndex v, Type t, size_t index);
Expr expr_arrayslice(ArraySlice v, Type t, size_t index);

#endif // EXPRS_H
