#include <assert.h>
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

uint64_t eval_sizeof_typedef(Sema *sema, Type type) {
    assert(type.kind == TkTypeDef);

    int64_t size = shget(sema->typedef_sizes, type.typedeff);
    assert(size != -1 && "sizeof typedef not calculated...");
    return (uint64_t)size;
}

uint64_t eval_sizeof(Sema *sema, Type type) {
    switch (type.kind) {
        case TkTypeId:
            // NOTE: not sure what to return here
        case TkNone:
        case TkVoid:
        case TkPoison:
            return 0;

        case TkBool:
        case TkI8:
        case TkU8:
            return BITS_TO_BYTES(8);

        case TkI16:
        case TkU16:
            return BITS_TO_BYTES(16);

        case TkChar:
        case TkI32:
        case TkU32:
        case TkF32:
            return BITS_TO_BYTES(32);

        case TkI64:
        case TkU64:
        case TkF64:
            return BITS_TO_BYTES(64);

        case TkIsize:
        case TkUsize:
            // TODO: this is platform dependent
            return BITS_TO_BYTES(64);

        case TkUntypedInt:
        case TkUntypedUint:
        case TkUntypedFloat:
            // NOTE: not sure if this is the best way to handle this
            return BITS_TO_BYTES(64);

        case TkUntypedString:
            // NOTE: evals to sizeof(string), might need to change in the future
        case TkString:
            // TODO: this is platform dependent, (ptr 32 / 64, len 32 / 64)
            return BITS_TO_BYTES(128);

        case TkCstring:
            // TODO: this is platform dependent, (ptr 32 / 64)
            return BITS_TO_BYTES(64);

        case TkRange:
            return eval_sizeof(sema, *type.range.subtype) * 2;
        case TkSlice:
            // TODO: this is platform dependent, (ptr 32 / 64, len 32 / 64)
            return BITS_TO_BYTES(128);

        case TkPtr:
            // TODO: this is platform dependent (32 / 64)
            return BITS_TO_BYTES(64);

        case TkOption:
            return eval_sizeof(sema, *type.option.subtype) + BITS_TO_BYTES(8);
        case TkArray:
            return eval_expr(sema, type.array.len) * eval_sizeof(sema, *type.array.of);
        case TkTypeDef:
            return eval_sizeof_typedef(sema, type);
    }

    return 0;
}

uint64_t eval_unop(Sema *sema, Expr *expr) {
    assert(expr->kind == EkUnop);

    uint64_t val = eval_expr(sema, expr->unop.val);

    // TODO: add cast to eval
    switch (expr->unop.kind) {
        case UkSizeof:
            return eval_sizeof(sema, expr->type);
            break;
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
            comp_elog("not implemented in eval_expr");
    }

    assert(false);
}
