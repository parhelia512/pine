#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include "include/typecheck.h"
#include "include/eval.h"
#include "include/exprs.h"
#include "include/sema.h"
#include "include/stmnts.h"
#include "include/strb.h"
#include "include/types.h"
#include "include/utils.h"

static const double F32_MIN = -3.40282347E+38;
static const double F32_MAX = 3.40282347E+38;
// static const double F64_MIN = -1.7976931348623157E+308;
// static const double F64_MAX = 1.7976931348623157E+308;

static const int64_t I8_MIN = INT8_MIN;
static const int64_t I8_MAX = INT8_MAX;
static const int64_t I16_MIN = INT16_MIN;
static const int64_t I16_MAX = INT16_MAX;
static const int64_t I32_MIN = INT32_MIN;
static const int64_t I32_MAX = INT32_MAX;
// static const int64_t I64_MIN = INT64_MIN;
// static const int64_t I64_MAX = INT64_MAX;

static const uint64_t U8_MAX = UINT8_MAX;
static const uint64_t U16_MAX = UINT16_MAX;
static const uint64_t U32_MAX = UINT32_MAX;
// static const uint64_t U64_MAX = UINT64_MAX;

static void elog(Sema *sema, size_t i, const char *msg, ...) {
    eprintf("%s:%lu:%lu " TERM_RED "error" TERM_END ": ", sema->filename, sema->cursors[i].row, sema->cursors[i].col);

    va_list args;
    va_start(args, msg);

    veprintfln(msg, args);

    va_end(args);
    exit(1);
}

bool tc_ptr_equals(Sema *sema, Type lhs, Type *rhs) {
    if (lhs.kind == TkPtr && rhs->kind != TkPtr) {
        return false;
    }

    if (lhs.kind == TkPtr && rhs->kind == TkPtr) {
        if (!lhs.constant && rhs->constant) return false;

        // hardcoded since usually can't typecheck between void and another type
        // unless it's a *void and *void, or **void and **void so on
        if (lhs.ptr_to->kind == TkVoid && rhs->ptr_to->kind == TkVoid) {
            return true;
        }

        if (!lhs.constant && !rhs->constant) return tc_ptr_equals(sema, *lhs.ptr_to, rhs->ptr_to);
        if (lhs.constant) return tc_ptr_equals(sema, *lhs.ptr_to, rhs->ptr_to);
    } else {
        return tc_equals(sema, lhs, rhs);
    }

    return false;
}

bool tc_range_equals(Sema *sema, Type lhs, Type *rhs) {
    if (lhs.kind == TkPoison || rhs->kind == TkPoison) {
        return false;
    }

    if (lhs.kind == TkRange && rhs->kind == TkRange) {
        return tc_equals(sema, *lhs.range.subtype, rhs->range.subtype);
    }

    return false;
}

bool tc_slice_equals(Sema *sema, Type lhs, Type *rhs) {
    if (lhs.kind == TkPoison || rhs->kind == TkPoison) {
        return false;
    }

    if (lhs.kind == TkSlice && rhs->kind == TkSlice) {
        return tc_equals(sema, *lhs.slice.of, rhs->slice.of);
    }

    return false;
}

bool tc_array_equals(Sema *sema, Type lhs, Type *rhs) {
    if (lhs.kind == TkPoison || rhs->kind == TkPoison) {
        return false;
    }

    if (lhs.kind == TkArray && rhs->kind == TkArray) {
        if (lhs.array.len->kind != EkNone) {
            uint64_t l_len = eval_expr(sema, lhs.array.len);

            if (rhs->array.len->kind == EkNone) elog(sema, rhs->cursors_idx, "cannot infer array length");
            uint64_t r_len = eval_expr(sema, rhs->array.len);

            if (l_len != r_len) return false;
            return tc_equals(sema, *lhs.array.of, rhs->array.of);
        } else {
            if (rhs->array.len->kind == EkNone) elog(sema, rhs->cursors_idx, "cannot infer array length");
            *lhs.array.len = *rhs->array.len;
            return tc_equals(sema, *lhs.array.of, rhs->array.of);
        }
    }

    return false;
}

// <ident>: <lhs> = <rhs>
// rhs is a pointer because it might be correct if wrapped in an option
bool tc_equals(Sema *sema, Type lhs, Type *rhs) {
    switch (lhs.kind) {
        case TkVoid:
            // NOTE: not sure about this
            return false;
        case TkNone:
            return false;
        case TkPoison:
            return false;
        case TkBool:
            return rhs->kind == TkBool;
        case TkChar:
            return rhs->kind == TkChar;
        case TkUntypedString:
            if (rhs->kind == TkString || rhs->kind == TkCstring) {
                return true;
            }
            return false;
        case TkString:
            if (rhs->kind == TkUntypedString) {
                rhs->kind = TkString;
            }
            return rhs->kind == TkString;
        case TkCstring:
            if (rhs->kind == TkUntypedString) {
                rhs->kind = TkCstring;
            }
            return rhs->kind == TkCstring;
        case TkTypeId:
            return rhs->kind == TkTypeId;
        case TkTypeDef:
            symtab_find(sema, lhs.typedeff, lhs.cursors_idx);
            return rhs->kind == TkTypeDef && streq(lhs.typedeff, rhs->typedeff);
        case TkOption:
            if (rhs->kind == TkOption) {
                if (lhs.option.subtype->kind == TkVoid) {
                    elog(sema, lhs.cursors_idx, "cannot use ?void. maybe use bool instead?");
                }

                if (rhs->option.is_null) {
                    *rhs->option.subtype = *lhs.option.subtype;
                    rhs->option.gen_option = true;
                    return true;
                }
                return tc_equals(sema, *lhs.option.subtype, rhs->option.subtype);
            } else if (tc_equals(sema, *lhs.option.subtype, rhs)) {
                Type *subtype = ealloc(sizeof(Type)); *subtype = *rhs;
                *rhs = type_option((Option){
                    .subtype = subtype,
                    .is_null = false,
                    .gen_option = true,
                }, TYPEVAR, 0);
                return true;
            }
            return false;
        case TkPtr:
            return tc_ptr_equals(sema, lhs, rhs);
        case TkArray:
            return tc_array_equals(sema, lhs, rhs);
        case TkSlice:
            return tc_slice_equals(sema, lhs, rhs);
        case TkRange:
            return tc_range_equals(sema, lhs, rhs);
        case TkUntypedInt:
            switch (rhs->kind) {
                case TkUntypedInt:
                case TkUntypedUint:
                case TkI8:
                case TkI16:
                case TkI32:
                case TkI64:
                case TkIsize:
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                case TkUsize:
                    return true;
                default:
                    return false;
            }
        case TkUntypedUint: {
            switch (rhs->kind) {
                case TkUntypedUint:
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                case TkUsize:
                    return true;
                default:
                    return false;
            }
        }
        case TkI8:
            switch (rhs->kind) {
                case TkUntypedInt:
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkI8:
                    return true;
                default:
                    return false;
            }
        case TkI16:
            switch (rhs->kind) {
                case TkUntypedInt:
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkI8:
                case TkI16:
                    return true;
                default:
                    return false;
            }
        case TkI32:
            switch (rhs->kind) {
                case TkUntypedInt:
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkI8:
                case TkI16:
                case TkI32:
                    return true;
                default:
                    return false;
            }
        case TkI64:
            switch (rhs->kind) {
                case TkUntypedInt:
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkI8:
                case TkI16:
                case TkI32:
                case TkI64:
                    return true;
                default:
                    return false;
            }
        case TkIsize:
            switch (rhs->kind) {
                case TkUntypedInt:
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkI8:
                case TkI16:
                case TkI32:
                case TkI64:
                case TkIsize:
                    return true;
                default:
                    return false;
            }
        case TkU8:
            switch (rhs->kind) {
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkU8:
                    return true;
                default:
                    return false;
            }
        case TkU16:
            switch (rhs->kind) {
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkU8:
                case TkU16:
                    return true;
                default:
                    return false;
            }
        case TkU32:
            switch (rhs->kind) {
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkU8:
                case TkU16:
                case TkU32:
                    return true;
                default:
                    return false;
            }
        case TkU64:
            switch (rhs->kind) {
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                    return true;
                default:
                    return false;
            }
        case TkUsize:
            switch (rhs->kind) {
                case TkUntypedUint:
                    *rhs = lhs;
                    return true;
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                case TkUsize:
                    return true;
                default:
                    return false;
            }
        case TkUntypedFloat:
            switch (rhs->kind) {
                case TkUntypedFloat:
                case TkUntypedInt:
                case TkF32:
                case TkF64:
                    return true;
                default:
                    return false;
            }
        case TkF32:
            switch (rhs->kind) {
                case TkUntypedFloat:
                case TkUntypedInt:
                    *rhs = lhs;
                    return true;
                case TkF32:
                    return true;
                default:
                    return false;
            }
        case TkF64:
            switch (rhs->kind) {
                case TkUntypedFloat:
                case TkUntypedInt:
                    *rhs = lhs;
                    return true;
                case TkF32:
                case TkF64:
                    return true;
                default:
                    return false;
            }
    }

    if (lhs.kind == rhs->kind) return true;
    return false;
}

void tc_return(Sema *sema, Stmnt *stmnt) {
    assert(stmnt->kind == SkReturn);
    FnDecl fndecl = sema->envinfo.fn.fndecl;
    Return *ret = &stmnt->returnf;

    if (ret->type.kind == TkNone) {
        ret->type = fndecl.type;
    }

    if (ret->type.kind == TkPoison || fndecl.type.kind == TkPoison) {
        return;
    }
    if (ret->value.type.kind == TkPoison) {
        return;
    }

    if (ret->value.kind == EkNone) {
        if (fndecl.type.kind == TkVoid) return;

        strb t1 = string_from_type(fndecl.type);
        strb t2 = string_from_type(ret->type);
        elog(sema, stmnt->cursors_idx, "mismatch types, %s vs %s", t1, t2);
        strbfree(t1); strbfree(t2);
        return;
    }

    if (!tc_equals(sema, ret->type, &ret->value.type)) {
        strb t1 = string_from_type(ret->type);
        strb t2 = string_from_type(ret->value.type);
        elog(sema, stmnt->cursors_idx, "mismatch types, expected return type %s, got %s", t1, t2);
        strbfree(t1); strbfree(t2);
        return;
    }

    if (!tc_equals(sema, fndecl.type, &ret->type)) {
        strb t1 = string_from_type(fndecl.type);
        strb t2 = string_from_type(ret->type);
        elog(sema, stmnt->cursors_idx, "mismatch types, funciton type %s, got %s", t1, t2);
        strbfree(t1); strbfree(t2);
        return;
    }
}

// returns TkNone if no default
Type tc_default_untyped_type(Type type) {
    if (type.kind == TkUntypedInt) {
        return type_number(TkI64, TYPEVAR, type.cursors_idx);
    } else if (type.kind == TkUntypedUint) {
        return type_number(TkU64, TYPEVAR, type.cursors_idx);
    } else if (type.kind == TkUntypedFloat) {
        return type_number(TkF64, TYPEVAR, type.cursors_idx);
    } else if (type.kind == TkUntypedString) {
        return type_string(TkString, TYPEVAR, type.cursors_idx);
    }

    return type_none();
}

void tc_infer(Sema *sema, Type *lhs, Expr *expr) {
    Type *exprtype = resolve_expr_type(sema, expr);
    Type default_type = tc_default_untyped_type(*exprtype);

    if (exprtype->kind == TkTypeDef) {
        symtab_find(sema, exprtype->typedeff, expr->cursors_idx);
    }

    if (default_type.kind != TkNone) {
        *lhs = default_type;
    } else {
        *lhs = *exprtype;
    }
}

void tc_var_decl(Sema *sema, Stmnt *stmnt) {
    assert(stmnt->kind == SkVarDecl);
    VarDecl *vardecl = &stmnt->vardecl;

    if (vardecl->value.kind == EkNone) {
        // <ident>: <type>;
        if (vardecl->type.kind == TkVoid) {
            // <ident>: void; error
            elog(sema, stmnt->cursors_idx, "variable cannot be of type void");
        }
    } else if (vardecl->type.kind == TkNone) {
        tc_infer(sema, &vardecl->type, &vardecl->value);
    } else {
        Type *exprtype = resolve_expr_type(sema, &vardecl->value);
        if (exprtype->kind == TkPoison || vardecl->type.kind == TkPoison) {
            return;
        }

        if (!tc_equals(sema, vardecl->type, exprtype)) {
            strb t1 = string_from_type(vardecl->type);
            strb t2 = string_from_type(*exprtype);
            elog(sema, stmnt->cursors_idx, "mismatch types, variable \"%s\" type %s, expression type %s", vardecl->name.ident, t1, t2);
            strbfree(t1); strbfree(t2);
            return;
        }
    }

    if (vardecl->type.kind == TkArray && vardecl->type.array.len->kind == EkNone) {
        elog(sema, stmnt->cursors_idx, "cannot infer array length for \"%s\" without compound literal", vardecl->name.ident);
    }

    tc_number_within_bounds(sema, vardecl->type, vardecl->value);
}

void tc_make_constant(Type *type) {
    switch (type->kind) {
        case TkPoison:
            break;
        case TkI8:
        case TkI16:
        case TkI32:
        case TkI64:
        case TkIsize:
        case TkU8:
        case TkU16:
        case TkU32:
        case TkU64:
        case TkUsize:
        case TkF32:
        case TkF64:
        case TkBool:
        case TkChar:
        case TkString:
        case TkCstring:
        case TkTypeDef:
        case TkTypeId:
            type->constant = true;
            return;
        case TkRange:
            tc_make_constant(type->range.subtype);
            type->constant = true;
            return;
        case TkSlice:
            tc_make_constant(type->slice.of);
            type->constant = true;
            return;
        case TkArray:
            tc_make_constant(type->array.of);
            type->constant = true;
            return;
        case TkOption:
            tc_make_constant(type->option.subtype);
            type->constant = true;
            return;
        case TkPtr:
            // don't make the underlying type constant
            type->constant = true;
            return;
        case TkVoid:
        case TkUntypedInt:
        case TkUntypedUint:
        case TkUntypedFloat:
        case TkUntypedString:
        case TkNone:
            assert(false && "cannot make void, untyped_int, untyped_uint, untyped_float, untyped_string, or None constant");
    }
}

void tc_const_decl(Sema *sema, Stmnt *stmnt) {
    assert(stmnt->kind == SkConstDecl);
    ConstDecl *constdecl = &stmnt->constdecl;
    Type *valtype = resolve_expr_type(sema, &constdecl->value);

    if (constdecl->type.kind == TkPoison || valtype->kind == TkPoison) {
        return;
    }

    if (constdecl->type.kind == TkNone) {
        tc_infer(sema, &constdecl->type, &constdecl->value);
    } else if (!tc_equals(sema, constdecl->type, valtype)) {
        strb t1 = string_from_type(constdecl->type);
        strb t2 = string_from_type(*valtype);
        elog(sema, stmnt->cursors_idx, "mismatch types, variable \"%s\" type %s, expression type %s", constdecl->name.ident, t1, t2);
        strbfree(t1); strbfree(t2);
    }

    tc_make_constant(&constdecl->type);
    tc_number_within_bounds(sema, constdecl->type, constdecl->value);
}

void tc_number_within_bounds(Sema *sema, Type type, Expr expr) {
    const char *lit = "";
    bool is_signed = false;

    if (expr.kind == EkUnop && expr.unop.kind == UkNegate && (expr.unop.val->kind == EkIntLit || expr.unop.val->kind == EkFloatLit)) {
        lit = expr.unop.val->lit;
        is_signed = true;
    } else if (expr.kind == EkIntLit || expr.kind == EkFloatLit) {
        lit = expr.lit;
    }

    if (streq(lit, "")) {
        return;
    }

    switch (type.kind) {
        case TkF32: {
            double value = 0.0;
            assert(parse_f64(lit, &value));
            if (value > F32_MAX || value < F32_MIN) {
                elog(sema, expr.cursors_idx, "literal \"%f\" cannot be represented in f32", value);
            }
        } break;
        case TkF64: {
            // no way to properly check here
        } break;
        case TkU8: {
            uint64_t value = 0;
            assert(parse_u64(lit, &value));
            if (is_signed || value > U8_MAX) {
                elog(sema, expr.cursors_idx, "literal \"%s%zu\" cannot be represented in u8", is_signed ? "-" : "", value);
            }
        } break;
        case TkU16: {
            uint64_t value = 0;
            assert(parse_u64(lit, &value));
            if (is_signed || value > U16_MAX) {
                elog(sema, expr.cursors_idx, "literal \"%s%zu\" cannot be represented in u16", is_signed ? "-" : "", value);
            }
        } break;
        case TkU32: {
            uint64_t value = 0;
            assert(parse_u64(lit, &value));
            if (is_signed || value > U32_MAX) {
                elog(sema, expr.cursors_idx, "literal \"%s%zu\" cannot be represented in u32", is_signed ? "-" : "", value);
            }
        } break;
        case TkU64:
            // no way to properly check here
            break;
        case TkUsize:
            // NOTE: if os is 32bit, this *can* be handled
            break;
        case TkI8: {
            int64_t value = 0;
            assert(parse_i64(lit, &value));
            if (value > I8_MAX || value < I8_MIN) {
                elog(sema, expr.cursors_idx, "literal \"%s%zu\" cannot be represented in i8", is_signed ? "-" : "", value);
            }
        } break;
        case TkI16: {
            int64_t value = 0;
            assert(parse_i64(lit, &value));
            if (value > I16_MAX || value < I16_MIN) {
                elog(sema, expr.cursors_idx, "literal \"%s%zu\" cannot be represented in i16", is_signed ? "-" : "", value);
            }
        } break;
        case TkI32: {
            int64_t value = 0;
            assert(parse_i64(lit, &value));
            if (value > I32_MAX || value < I32_MIN) {
                elog(sema, expr.cursors_idx, "literal \"%s%zu\" cannot be represented in i32", is_signed ? "-" : "", value);
            }
        } break;
        case TkI64:
            // no way to properly check here
            break;
        case TkIsize:
            // NOTE: if os is 32bit, this *can* be handled
            break;
        default:
            break;
    }
}

bool tc_is_unsigned(Sema *sema, Expr expr) {
    Type *type = resolve_expr_type(sema, &expr);

    switch (type->kind) {
        case TkU8:
        case TkU16:
        case TkU32:
        case TkU64:
        case TkUsize:
        case TkUntypedUint:
            return true;
        case TkI8:
        case TkI16:
        case TkI32:
        case TkI64:
        case TkIsize:
        case TkF32:
        case TkF64:
        case TkUntypedFloat:
        case TkUntypedInt:
            return false;
        case TkPoison:
            return false;
        default: {
            strb t = string_from_type(*type);
            elog(sema, expr.cursors_idx, "expected an integer type, got %s", t);
            strbfree(t);
            return false;
        }
    }
}

bool tc_can_arithmetic(Type lhs, Type rhs, bool ints_only) {
    switch (lhs.kind) {
        case TkI8:
        case TkI16:
        case TkI32:
        case TkI64:
        case TkIsize:
            switch (rhs.kind) {
                case TkI8:
                case TkI16:
                case TkI32:
                case TkI64:
                case TkIsize:
                case TkUntypedInt:
                    return true;
                default:
                    return false;
            }
        case TkU8:
        case TkU16:
        case TkU32:
        case TkU64:
        case TkUsize:
            switch (rhs.kind) {
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                case TkUsize:
                case TkUntypedInt:
                    return true;
                default:
                    return false;
            }
        case TkUntypedInt:
            switch (rhs.kind) {
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                case TkUsize:
                case TkI8:
                case TkI16:
                case TkI32:
                case TkI64:
                case TkIsize:
                case TkUntypedInt:
                    return true;
                default:
                    return false;
            }
        case TkF32:
        case TkF64:
        case TkUntypedFloat:
            if (ints_only) return false;
            switch (rhs.kind) {
                case TkF32:
                case TkF64:
                case TkUntypedFloat:
                    return true;
                default:
                    return false;
            }
        default:
            return false;
    }
}

bool tc_can_compare_equality(Sema *sema, Type lhs, Type rhs) {
    switch (lhs.kind) {
        case TkBool:
            if (rhs.kind == TkBool) {
                return true;
            }
            return false;
        case TkI8:
        case TkI16:
        case TkI32:
        case TkI64:
        case TkIsize:
            switch (rhs.kind) {
                case TkI8:
                case TkI16:
                case TkI32:
                case TkI64:
                case TkIsize:
                case TkUntypedInt:
                    return true;
                default:
                    return false;
            }
        case TkU8:
        case TkU16:
        case TkU32:
        case TkU64:
        case TkUsize:
            switch (rhs.kind) {
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                case TkUsize:
                case TkUntypedInt:
                    return true;
                default:
                    return false;
            }
        case TkUntypedInt:
            switch (rhs.kind) {
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                case TkUsize:
                case TkI8:
                case TkI16:
                case TkI32:
                case TkI64:
                case TkIsize:
                case TkUntypedInt:
                    return true;
                default:
                    return false;
            }
        case TkF32:
        case TkF64:
        case TkUntypedFloat:
            switch (rhs.kind) {
                case TkF32:
                case TkF64:
                case TkUntypedFloat:
                    return true;
                default:
                    return false;
            }
        case TkTypeDef: {
            if (rhs.kind != TkTypeDef) {
                return false;
            }

            Stmnt lhs_stmnt = symtab_find(sema, lhs.typedeff, lhs.cursors_idx);
            Stmnt rhs_stmnt = symtab_find(sema, rhs.typedeff, rhs.cursors_idx);
            if (!(lhs_stmnt.kind == SkEnumDecl && rhs_stmnt.kind == SkEnumDecl)) {
                return false;
            }

            assert(lhs_stmnt.enumdecl.name.kind == EkIdent && ".name is still expected to be Ident");
            assert(rhs_stmnt.enumdecl.name.kind == EkIdent && ".name is still expected to be Ident");
            if (!streq(lhs_stmnt.enumdecl.name.ident, rhs_stmnt.enumdecl.name.ident)) {
                return false;
            }

            return true;
        }
        default:
            return false;
    }
}

bool tc_can_compare_order(Type lhs, Type rhs) {
    switch (lhs.kind) {
        case TkI8:
        case TkI16:
        case TkI32:
        case TkI64:
        case TkIsize:
            switch (rhs.kind) {
                case TkI8:
                case TkI16:
                case TkI32:
                case TkI64:
                case TkIsize:
                    return true;
                default:
                    return false;
            }
        case TkU8:
        case TkU16:
        case TkU32:
        case TkU64:
        case TkUsize:
            switch (rhs.kind) {
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                case TkUsize:
                    return true;
                default:
                    return false;
            }
        case TkUntypedInt:
            switch (rhs.kind) {
                case TkI8:
                case TkI16:
                case TkI32:
                case TkI64:
                case TkIsize:
                case TkU8:
                case TkU16:
                case TkU32:
                case TkU64:
                case TkUsize:
                case TkUntypedInt:
                    return true;
                default:
                    return false;
            }
        case TkF32:
        case TkF64:
        case TkUntypedFloat:
            switch (rhs.kind) {
                case TkF32:
                case TkF64:
                case TkUntypedFloat:
                    return true;
                default:
                    return false;
            }
        default:
            return false;
    }
}

bool tc_can_bitwise(Type lhs, Type rhs) {
    switch (lhs.kind) {
        case TkI8:
        case TkU8:
        case TkI16:
        case TkU16:
        case TkI32:
        case TkU32:
        case TkI64:
        case TkU64:
        case TkIsize:
        case TkUsize:
        case TkUntypedInt:
            switch (rhs.kind) {
                case TkI8:
                case TkU8:
                case TkI16:
                case TkU16:
                case TkI32:
                case TkU32:
                case TkI64:
                case TkU64:
                case TkIsize:
                case TkUsize:
                case TkUntypedInt:
                    return true;
                default:
                    return false;
            }
        default:
            return false;
    }
}

bool tc_can_cast_ptr(Type *from, Type to) {
    if (to.kind == TkPtr && from->kind == TkPtr) {
        if (from->constant && !to.constant) return false;
        
        if (to.ptr_to->kind == TkVoid && from->ptr_to->kind != TkPtr) {
            return true;
        } else if (from->ptr_to->kind == TkVoid && from->ptr_to->kind != TkPtr) {
            return true;
        }

        if (!from->constant && !to.constant) return tc_can_cast_ptr(from->ptr_to, *to.ptr_to);
        if (to.constant) return tc_can_cast_ptr(from->ptr_to, *to.ptr_to);
    }
    if (to.kind == TkPtr && to.ptr_to->kind == TkVoid) {
        return true;
    }

    return false;
}

bool tc_can_cast(Sema *sema, Type *from, Type to) {
    if (tc_equals(sema, to, from)) {
        return true;
    }

    switch (to.kind) {
        case TkPtr:
            return tc_can_cast_ptr(from, to);
        case TkI8:
        case TkU8:
        case TkI16:
        case TkU16:
        case TkI32:
        case TkU32:
        case TkI64:
        case TkU64:
        case TkF32:
        case TkF64:
        case TkIsize:
        case TkUsize:
        case TkUntypedInt:
        case TkUntypedFloat:
        case TkChar:
            switch (from->kind) {
                case TkI8:
                case TkU8:
                case TkI16:
                case TkU16:
                case TkI32:
                case TkU32:
                case TkI64:
                case TkU64:
                case TkF32:
                case TkF64:
                case TkIsize:
                case TkUsize:
                case TkUntypedInt:
                case TkUntypedFloat:
                case TkChar:
                    return true;
                default:
                    return false;
            }
        default:
            return false;
    }
}
