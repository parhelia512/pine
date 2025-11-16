#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include "include/eval.h"
#include "include/exprs.h"
#include "include/sema.h"
#include "include/types.h"
#include "include/utils.h"

uint64_t eval_binop(Sema *sema, Expr *expr) {
    assert(expr->kind == EkBinop);

    uint64_t lhs = eval_expr(sema, expr->binop.left);
    uint64_t rhs = eval_expr(sema, expr->binop.right);

    switch (expr->binop.kind) {
        case BkPlus:
            return lhs + rhs;
        case BkMinus:
            return lhs - rhs;
        case BkMultiply:
            return lhs * rhs;
        case BkDivide:
            return lhs / rhs;
        case BkMod:
            return lhs % rhs;
        case BkLess:
            return lhs < rhs;
        case BkLessEqual:
            return lhs <= rhs;
        case BkGreater:
            return lhs > rhs;
        case BkGreaterEqual:
            return lhs >= rhs;
        case BkEquals:
            return lhs == rhs;
        case BkInequals:
            return lhs != rhs;
        case BkLeftShift:
            return lhs << rhs;
        case BkRightShift:
            return lhs >> rhs;
        case BkBitAnd:
            return lhs & rhs;
        case BkBitOr:
            return lhs | rhs;
        case BkBitXor:
            return lhs ^ rhs;
        case BkAnd:
            return lhs && rhs;
        case BkOr:
            return lhs || rhs;
    }

    assert(false);
}

uint64_t eval_unop(Sema *sema, Expr *expr) {
    assert(expr->kind == EkUnop);

    uint64_t val = eval_expr(sema, expr->unop.val);

    // TODO: add cast and sizeof to eval
    switch (expr->unop.kind) {
        case UkBitNot:
            return ~val;
        case UkNot:
            return !val;
        case UkNegate:
            return -val;
        case UkAddress:
            comp_elog("cannot take address at compile time");
            return 0;
    }

    assert(false);
}

uint64_t eval_expr(Sema *sema, Expr *expr) {
    sema_expr(sema, expr);

    switch (expr->kind) {
        case EkIntLit: {
            uint64_t n = 0;
            if (parse_u64(expr->lit, &n)) {
                return n;
            } else {
                assert(false && "couldn't parse intlit to u64");
            }
        } break;
        case EkIdent:
            // TODO: find out if ident is compile time known
            return 0;
        case EkBinop:
            return eval_binop(sema, expr);
        case EkUnop:
            return eval_unop(sema, expr);
        default:
            debug("not implemented in eval_expr");
            exit(1);
    }

    assert(false);
}
