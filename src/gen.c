#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "include/gen.h"
#include "include/exprs.h"
#include "include/lexer.h"
#include "include/sema.h"
#include "include/stb_ds.h"
#include "include/stmnts.h"
#include "include/strb.h"
#include "include/types.h"
#include "include/utils.h"

extern unsigned char builtin_defs[];
extern unsigned int builtin_defs_len;

void mastrfree(MaybeAllocStr s) {
    if (s.alloced) strbfree(s.str);
}

Gen gen_init(Arr(Stmnt) ast, Dgraph dgraph, const char *filename, Arr(Cursor) cursors) {
    return (Gen){
        .ast = ast,
        .code = NULL,
        .defs = NULL,

        .indent = 0,
        .defers = NULL,
        
        .in_defs = false,
        .dgraph = dgraph,
        .def_loc = 0,

        .code_loc = 0,
        .generated_typedefs = NULL,

        .compile_flags = {
            .links = NULL,
            .optimisation = OlDebug,
            .output = "",
        },
        .cursors = cursors,
        .filename = filename,
    };
}

void gen_push_defer(Gen *gen, Stmnt *stmnt) {
    Defer defer = {
        .stmnt = stmnt,
        .indent = gen->indent,
    };
    arrpush(gen->defers, defer);
}

void gen_pop_defers(Gen *gen) {
    for (ptrdiff_t i = arrlen(gen->defers) - 1; i >= 0; i--) {
        if (gen->defers[i].indent == gen->indent) {
            arrdel(gen->defers, i);
        }
    }
}

void gen_directive(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkDirective);
    switch (stmnt.directive.kind) {
        case DkLink:
            arrpush(gen->compile_flags.links, stmnt.directive.str);
            break;
        case DkSyslink: {
            strb l = NULL;
            strbprintf(&l, "-l%s", stmnt.directive.str);
            arrpush(gen->compile_flags.links, l);
        } break;
        case DkOutput:
            gen->compile_flags.output = stmnt.directive.str;
            break;
        case DkO0:
            gen->compile_flags.optimisation = OlZero;
            break;
        case DkO1:
            gen->compile_flags.optimisation = OlOne;
            break;
        case DkO2:
            gen->compile_flags.optimisation = OlTwo;
            break;
        case DkO3:
            gen->compile_flags.optimisation = OlThree;
            break;
        case DkOdebug:
            gen->compile_flags.optimisation = OlDebug;
            break;
        case DkOfast:
            gen->compile_flags.optimisation = OlFast;
            break;
        case DkOsmall:
            gen->compile_flags.optimisation = OlSmall;
            break;
        default:
            break;
    }
}

void gen_write(Gen *gen, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    if (gen->in_defs) {
        vstrbprintf(&gen->defs, fmt, args);
    } else {
        vstrbprintf(&gen->code, fmt, args);
    }
    
    va_end(args);
}

void gen_writeln(Gen *gen, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    if (gen->in_defs) {
        vstrbprintf(&gen->defs, fmt, args);
        strbpush(&gen->defs, '\n');
    } else {
        vstrbprintf(&gen->code, fmt, args);
        strbpush(&gen->code, '\n');
    }

    va_end(args);
}

void gen_indent(Gen *gen) {
    for (uint8_t i = 0; i < gen->indent; i++) {
        gen_write(gen, "    ");
    }
}

bool gen_find_generated_typedef(Gen *gen, const char *needle) {
    for (size_t i = 0; i < arrlenu(gen->generated_typedefs); i++) {
        if (streq(needle, gen->generated_typedefs[i])) {
            return true;
        }
    }

    return false;
}

strb gen_array_type(Gen *gen, Type type, const char *name) {
    strb subtype = NULL;
    strb lens = NULL;

    Type st = type;
    while (true) {
        if (st.kind == TkArray) {
            MaybeAllocStr len = gen_expr(gen, *st.array.len);
            strbprintf(&lens, "[%s]", len.str);
            mastrfree(len);

            st = *st.array.of;
        } else {
            MaybeAllocStr t = gen_type(gen, st);
            strbprintf(&subtype, "%s", t);
            mastrfree(t);
            break;
        }
    }

    strb ret = NULL;
    strbprintf(&ret, "%s %s%s", subtype, name, lens);
    return ret;
}

strb gen_ptr_type(Gen *gen, Type type) {
    strb stars = NULL;
    Type st = type;

    while (true) {
        if (st.kind == TkPtr) {
            strbprintf(&stars, "*");
            st = *st.ptr_to;
        } else {
            break;
        }
    }

    strb ret = NULL;

    MaybeAllocStr t = gen_type(gen, st);
    strbprintf(&ret, "%s%s", t.str, stars);
    mastrfree(t);

    strbfree(stars);
    return ret;
}

MaybeAllocStr gen_type(Gen *gen, Type type) {
    assert(type.kind != TkUntypedUint && type.kind != TkUntypedInt && type.kind != TkUntypedFloat && "should not be codegening untyped types");
    gen_decl_generic(gen, type);

    switch (type.kind) {
        case TkSlice: {
            strb ret = NULL;
            gen_typename(gen, &type, 1, &ret);

            return (MaybeAllocStr){
                .str = ret,
                .alloced = true,
            };
        }
        case TkArray: {
            strb ret = NULL;
            gen_typename(gen, &type, 1, &ret);

            return (MaybeAllocStr){
                .str = ret,
                .alloced = true,
            };
        }
        case TkOption: {
            strb ret = NULL;
            gen_typename(gen, &type, 1, &ret);

            return (MaybeAllocStr){
                .str = ret,
                .alloced = true,
            };
        }
        case TkPtr:
            return (MaybeAllocStr){
                .str = gen_ptr_type(gen, type),
                .alloced = true,
            };
        case TkCstring:
            return (MaybeAllocStr){
                .str = "const char*",
                .alloced = false,
            };
        case TkString:
            return (MaybeAllocStr){
                .str = "PineString",
                .alloced = false,
            };
        case TkChar:
            // TODO: make this be a type that supports utf8
            return (MaybeAllocStr){
                .str = "u8",
                .alloced = false,
            };
        default:
            return (MaybeAllocStr){
                .str = string_from_type(type),
                .alloced = true,
            };
    }
}

strb gen_typename_array(Gen *gen, Type type) {
    strb arr = NULL;
    strbprintf(&arr, "PineArray");

    int dim = 0;
    Type st = type;
    strb lengths = NULL;
    while (true) {
        if (st.kind == TkArray) {
            dim++;

            MaybeAllocStr len = gen_expr_for_identifier(gen, *st.array.len);
            strbprintf(&lengths, "%s", len.str);
            mastrfree(len);

            st = *st.array.of;
        } else {
            break;
        }
    }

    strb typename = NULL;
    gen_typename(gen, &st, 1, &typename);
    strbprintf(&arr, "%d_%s%s", dim, typename, lengths);

    strbfree(typename);
    strbfree(lengths);

    return arr;
}

strb gen_typename_slice(Gen *gen, Type type) {
    strb slice = NULL;
    strbprintf(&slice, "PineSlice");

    int dim = 0;
    Type st = type;
    while (true) {
        if (st.kind == TkSlice) {
            dim++;
            st = *st.slice.of;
        } else {
            break;
        }
    }

    strb typename = NULL;
    gen_typename(gen, &st, 1, &typename);
    strbprintf(&slice, "%dd_%s", dim, typename);

    strbfree(typename);
    return slice;
}

// remember to free typename after
void gen_typename(Gen *gen, Type *types, size_t types_len, strb *typename) {
    for (size_t i = 0; i < types_len; i++) {
        Type type = types[i];

        switch (type.kind) {
            case TkUntypedInt:
            case TkUntypedUint:
            case TkUntypedFloat:
                assert(false && "unexpected untyped type in gen_typename");
                break;
            case TkCstring:
                strbprintf(typename, "constcharptr");
                break;
            case TkPtr:
                gen_typename(gen, type.ptr_to, 1, typename);
                strbprintf(typename, "ptr");
                break;
            case TkSlice: {
                strb slice = gen_typename_slice(gen, type);
                strbprintf(typename, "%s", slice);
                strbfree(slice);
            } break;
            case TkArray: {
                strb arr = gen_typename_array(gen, type);
                strbprintf(typename, "%s", arr);
                strbfree(arr);
            } break;
            case TkOption: {
                strb option = NULL;
                gen_typename(gen, type.option.subtype, 1, &option);
                strbprintf(typename, "PineOption_%s", option);
                strbfree(option);
            } break;
            default: {
                MaybeAllocStr ty = gen_type(gen, type);
                if (ty.alloced) {
                    char *tn = strtrim(ty.str);
                    strbprintf(typename, "%s", tn);
                    mastrfree(ty);
                } else {
                    strbprintf(typename, "%s", ty);
                }
            } break;
        }
    }
}

// .alloced will always be true if it generated, if it's false, it wasn't an option expr
MaybeAllocStr gen_option_expr(Gen *gen, Expr expr) {
    if (expr.type.kind == TkOption && expr.type.option.gen_option) {
        strb typename = NULL;
        gen_typename(gen, expr.type.option.subtype, 1, &typename);
        expr.type = *expr.type.option.subtype;
        MaybeAllocStr value = gen_expr(gen, expr);

        strb option = NULL;
        strbprintf(&option, "pineoption_%s(%s)", typename, value.str);

        mastrfree(value);
        strbfree(typename);

        return (MaybeAllocStr){
            .str = option,
            .alloced = true,
        };
    }

    return (MaybeAllocStr){
        .str = "",
        .alloced = false,
    };
}

strb gen_numlit_expr(Expr expr, bool for_identifier) {
    assert(expr.kind == EkIntLit || expr.kind == EkFloatLit);
    strb s = NULL;

    if (for_identifier) {
        strbprintf(&s, "%s", expr.lit);
        return s;
    }

    switch (expr.type.kind) {
        case TkF32:
            strbprintf(&s, "%sf", expr.lit);
            break;
        case TkF64:
        case TkUntypedFloat:
            strbprintf(&s, "%s", expr.lit);
            break;
        case TkU8:
            strbprintf(&s, "UINT8_C(%s)", expr.lit);
            break;
        case TkU16:
            strbprintf(&s, "UINT16_C(%s)", expr.lit);
            break;
        case TkU32:
            strbprintf(&s, "UINT32_C(%s)", expr.lit);
            break;
        case TkU64:
        case TkUntypedUint:
            strbprintf(&s, "UINT64_C(%s)", expr.lit);
            break;
        case TkUsize:
            strbprintf(&s, "UINT64_C(%s)", expr.lit);
            break;
        case TkI8:
            strbprintf(&s, "INT8_C(%s)", expr.lit);
            break;
        case TkI16:
            strbprintf(&s, "INT16_C(%s)", expr.lit);
            break;
        case TkI32:
            strbprintf(&s, "INT32_C(%s)", expr.lit);
            break;
        case TkI64:
        case TkUntypedInt:
            strbprintf(&s, "INT64_C(%s)", expr.lit);
            break;
        case TkIsize:
            strbprintf(&s, "INT64_C(%s)", expr.lit);
            break;
        default:
            break;
    }

    return s;
}

MaybeAllocStr gen_unop_expr(Gen *gen, Expr expr) {
    assert(expr.kind == EkUnop);

    strb ret = NULL;
    MaybeAllocStr value = gen_expr(gen, *expr.unop.val);

    switch (expr.unop.kind) {
        case UkAddress:
            strbprintf(&ret, "&%s", value.str);
            break;
        case UkNegate:
            strbprintf(&ret, "-%s", value.str);
            break;
        case UkNot:
            strbprintf(&ret, "!%s", value.str);
            break;
        case UkBitNot:
            strbprintf(&ret, "~%s", value.str);
            break;
        case UkCast: {
            MaybeAllocStr type = gen_type(gen, expr.type);
            strbprintf(&ret, "(%s)%s", type.str, value.str);
            mastrfree(type);
            break;
        case UkSizeof:
            strbprintf(&ret, "%s", value.str);
            break;
        }
    }

    mastrfree(value);
    return (MaybeAllocStr){
        .str = ret,
        .alloced = true,
    };
}

MaybeAllocStr gen_fn_call(Gen *gen, Expr expr) {
    assert(expr.kind == EkFnCall);

    strb call = NULL;
    strbprintf(&call, "%s(", expr.fncall.name->ident);

    for (size_t i = 0; i < arrlenu(expr.fncall.args.exprs); i++) {
        MaybeAllocStr arg = gen_expr(gen, expr.fncall.args.exprs[i]);

        if (i == 0) {
            strbprintf(&call, "%s", arg.str);
        } else {
            strbprintf(&call, ", %s", arg.str);
        }

        mastrfree(arg);
    }
    strbpush(&call, ')');

    return (MaybeAllocStr){
        .str = call,
        .alloced = true,
    };
}

MaybeAllocStr gen_slice_literal_expr(Gen *gen, Expr expr) {
    assert(expr.type.kind == TkSlice);
    strb lit = NULL;
    Type slice = expr.type;

    strb typename = NULL;
    gen_typename(gen, &slice, 1, &typename);

    char *type = strtok(typename, "_");
    for (size_t i = 0; i < strlen(type); i++) {
        type[i] = tolower(type[i]);
    }
    typename[strlen(type)] = '_';

    strbprintf(&lit, "%s(", typename);

    Type *slice_of = slice.slice.of;
    MaybeAllocStr subtype = gen_type(gen, *slice_of);

    strbprintf(&lit, "(%s[]){", subtype.str);
    mastrfree(subtype);

    for (size_t i = 0; i < arrlenu(expr.literal.exprs); i++) {
        MaybeAllocStr val = gen_expr(gen, expr.literal.exprs[i]);

        if (i == 0) {
            strbprintf(&lit, "%s", val.str);
        } else {
            strbprintf(&lit, ", %s", val.str);
        }

        mastrfree(val);
    }
    strbprintf(&lit, "}, %d)", arrlenu(expr.literal.exprs));

    strbfree(typename);
    return (MaybeAllocStr){
        .str = lit,
        .alloced = true,
    };
}

MaybeAllocStr gen_literal_expr(Gen *gen, Expr expr) {
    assert(expr.kind == EkLiteral);
    strb lit = NULL;

    if (expr.type.kind == TkSlice) {
        return gen_slice_literal_expr(gen, expr);
    }

    if (expr.type.kind == TkArray) {
        strbpush(&lit, '{');
    } else {
        MaybeAllocStr exprtype = gen_type(gen, expr.type);
        strbprintf(&lit, "(%s){", exprtype.str);
        mastrfree(exprtype);
    }

    if (expr.literal.kind == LitkExprs) {
        for (size_t i = 0; i < arrlenu(expr.literal.exprs); i++) {
            MaybeAllocStr val = gen_expr(gen, expr.literal.exprs[i]);

            if (i == 0) {
                strbprintf(&lit, "%s", val.str);
            } else {
                strbprintf(&lit, ", %s", val.str);
            }

            mastrfree(val);
        }
    } else {
        for (size_t i = 0; i < arrlenu(expr.literal.vars); i++) {
            MaybeAllocStr reassigned = gen_expr(gen, expr.literal.vars[i].varreassign.name);
            MaybeAllocStr value = gen_expr(gen, expr.literal.vars[i].varreassign.value);

            if (i == 0) {
                strbprintf(&lit, ".%s = %s", reassigned.str, value.str);
            } else {
                strbprintf(&lit, ", .%s = %s", reassigned.str, value.str);
            }

            mastrfree(reassigned);
            mastrfree(value);
        }
    }
    strbpush(&lit, '}');

    return (MaybeAllocStr){
        .str = lit,
        .alloced = true,
    };
}

MaybeAllocStr gen_binop_expr(Gen *gen, Expr expr) {
    assert(expr.kind == EkBinop);

    MaybeAllocStr lhs = gen_expr(gen, *expr.binop.left);
    MaybeAllocStr rhs = gen_expr(gen, *expr.binop.right);
    strb ret = NULL;

    switch (expr.binop.kind) {
        case BkPlus:
            strbprintf(&ret, "%s + %s", lhs.str, rhs.str);
            break;
        case BkMinus:
            strbprintf(&ret, "%s - %s", lhs.str, rhs.str);
            break;
        case BkMultiply:
            strbprintf(&ret, "%s * %s", lhs.str, rhs.str);
            break;
        case BkDivide:
            strbprintf(&ret, "%s / %s", lhs.str, rhs.str);
            break;
        case BkMod:
            strbprintf(&ret, "%s %% %s", lhs.str, rhs.str);
            break;
        case BkLess:
            strbprintf(&ret, "%s < %s", lhs.str, rhs.str);
            break;
        case BkLessEqual:
            strbprintf(&ret, "%s <= %s", lhs.str, rhs.str);
            break;
        case BkGreater:
            strbprintf(&ret, "%s > %s", lhs.str, rhs.str);
            break;
        case BkGreaterEqual:
            strbprintf(&ret, "%s >= %s", lhs.str, rhs.str);
            break;
        case BkEquals:
            strbprintf(&ret, "%s == %s", lhs.str, rhs.str);
            break;
        case BkInequals:
            strbprintf(&ret, "%s != %s", lhs.str, rhs.str);
            break;
        case BkLeftShift:
            strbprintf(&ret, "%s << %s", lhs.str, rhs.str);
            break;
        case BkRightShift:
            strbprintf(&ret, "%s >> %s", lhs.str, rhs.str);
            break;
        case BkBitAnd:
            strbprintf(&ret, "%s & %s", lhs.str, rhs.str);
            break;
        case BkBitOr:
            strbprintf(&ret, "%s | %s", lhs.str, rhs.str);
            break;
        case BkBitXor:
            strbprintf(&ret, "%s ^ %s", lhs.str, rhs.str);
            break;
        case BkAnd:
            strbprintf(&ret, "%s && %s", lhs.str, rhs.str);
            break;
        case BkOr:
            strbprintf(&ret, "%s || %s", lhs.str, rhs.str);
            break;
    }
    
    mastrfree(lhs);
    mastrfree(rhs);

    return (MaybeAllocStr){
        .str = ret,
        .alloced = true,
    };
}

MaybeAllocStr _gen_expr(Gen *gen, Expr expr, bool for_identifier) {
    if (expr.kind == EkType) {
        return gen_type(gen, expr.type_expr);
    }

    if (expr.kind != EkNull) {
        MaybeAllocStr opt = gen_option_expr(gen, expr);
        if (opt.alloced) return opt;
    }

    switch (expr.kind) {
        case EkIdent: {
            strb ident = NULL;
            strbprintf(&ident, "%s", expr.ident);
            return (MaybeAllocStr){
                .str = ident,
                .alloced = true,
            };
        }
        case EkIntLit:
        case EkFloatLit:
            return (MaybeAllocStr){
                .str = gen_numlit_expr(expr, for_identifier),
                .alloced = true,
            };
        case EkCharLit: {
            strb lit = NULL;
            strbprintf(&lit, "'%s'", expr.lit);
            return (MaybeAllocStr){
                .str = lit,
                .alloced = true,
            };
        }
        case EkStrLit: {
            strb lit = NULL;
            if (expr.type.kind == TkCstring) {
                strbprintf(&lit, "\"%s\"", expr.lit);
            } else {
                strbprintf(&lit, "(PineString){.ptr = \"%s\", .len = %zu}", expr.lit, strlen(expr.lit));
            }

            return (MaybeAllocStr){
                .str = lit,
                .alloced = true,
            };
        }
        case EkTrue:
            return (MaybeAllocStr){
                .str = "true",
                .alloced = false,
            };
        case EkFalse:
            return (MaybeAllocStr){
                .str = "false",
                .alloced = false,
            };
        case EkNull: {
            strb typename = NULL;
            gen_typename(gen, expr.type.option.subtype, 1, &typename);

            strb option = NULL;
            strbprintf(&option, "pineoptionnull_%s()", typename);

            strbfree(typename);
            return (MaybeAllocStr){
                .str = option,
                .alloced = true,
            };
        } break;
        case EkFieldAccess: {
            MaybeAllocStr subexpr = gen_expr(gen, *expr.fieldacc.accessing);
            if (expr.fieldacc.deref) {
                strb ret = NULL; strbprintf(&ret, "*%s", subexpr.str);
                mastrfree(subexpr);
                return (MaybeAllocStr){
                    .str = ret,
                    .alloced = true,
                };
            }

            MaybeAllocStr field = gen_expr(gen, *expr.fieldacc.field);
            strb ret = NULL;

            // NOTE: temporary fix for constant folding `array.len`
            // TODO: maybe have an optimisation step, do constant folding, etc
            if (
                ((expr.fieldacc.accessing->type.kind == TkPtr &&
                expr.fieldacc.accessing->type.ptr_to->kind == TkArray) ||
                expr.fieldacc.accessing->type.kind == TkArray) &&
                streq(field.str, "len")
            ) {
                MaybeAllocStr len;
                if (expr.fieldacc.accessing->type.kind == TkPtr) {
                    len = gen_expr(gen, *expr.fieldacc.accessing->type.ptr_to->array.len);
                } else {
                    len = gen_expr(gen, *expr.fieldacc.accessing->type.array.len);
                }
                strbprintf(&ret, "%s", len.str);
                mastrfree(len);
            }

            else if (expr.fieldacc.accessing->type.kind == TkPtr) {
                strbprintf(&ret, "%s->%s", subexpr.str, field.str);
            } else if (expr.fieldacc.accessing->type.kind == TkTypeDef) {
                Stmnt stmnt = ast_find_decl(gen->ast, expr.fieldacc.accessing->type.typedeff);

                if (stmnt.kind == SkStructDecl) {
                    strbprintf(&ret, "%s.%s", subexpr.str, field.str);
                } else if (stmnt.kind == SkEnumDecl) {
                    strbprintf(&ret, "%s_%s", subexpr.str, field.str);
                }
            } else {
                strbprintf(&ret, "%s.%s", subexpr.str, field.str);
            }

            mastrfree(subexpr);
            mastrfree(field);

            return (MaybeAllocStr){
                .str = ret,
                .alloced = true,
            };
        }
        case EkArrayIndex: {
            MaybeAllocStr access = gen_expr(gen, *expr.arrayidx.accessing);
            MaybeAllocStr index = gen_expr(gen, *expr.arrayidx.index);
            strb ret = NULL;

            if (expr.fieldacc.accessing->type.kind == TkPtr) {
                strbprintf(&ret, "(*%s)[%s]", access.str, index.str);
            } else if (expr.fieldacc.accessing->type.kind == TkSlice) {
                strbprintf(&ret, "pinesliceat(\"%s\", \"%lu\", %s, %s)", gen->filename, gen->cursors[expr.cursors_idx].row, access.str, index.str);
            } else {
                MaybeAllocStr len = gen_expr(gen, *expr.arrayidx.accessing->type.array.len);
                strbprintf(&ret, "pinearrat(\"%s\", \"%lu\", %s, %s, %s)", gen->filename, gen->cursors[expr.cursors_idx].row, access.str, index.str, len.str);
                mastrfree(len);
            }

            mastrfree(access);
            mastrfree(index);

            return (MaybeAllocStr){
                .str = ret,
                .alloced = true,
            };
        }
        case EkArraySlice: {
            strb typename = NULL;
            gen_typename(gen, expr.type.slice.of, 1, &typename);

            MaybeAllocStr access = gen_expr(gen, *expr.arrayslice.accessing);

            MaybeAllocStr start;
            if (expr.arrayslice.slice->rangelit.start->kind == EkNone) {
                start = (MaybeAllocStr){
                    .str = "0",
                    .alloced = false,
                };
            } else {
                start = gen_expr(gen, *expr.arrayslice.slice->rangelit.start);
            }

            MaybeAllocStr end;
            if (expr.arrayslice.slice->rangelit.end->kind == EkNone) {
                if (expr.arrayslice.accessing->type.kind == TkArray) {
                    MaybeAllocStr len = gen_expr(gen, *expr.arrayslice.accessing->type.array.len);
                    strb end_s = NULL; strbprintf(&end_s, "%s - 1", len.str);
                    mastrfree(len);
                    end = (MaybeAllocStr){
                        .str = end_s,
                        .alloced = true,
                    };
                } else {
                    strb end_s = NULL; strbprintf(&end_s, "%s.len - 1", access.str);
                    end = (MaybeAllocStr){
                        .str = end_s,
                        .alloced = true,
                    };
                }
            } else {
                end = gen_expr(gen, *expr.arrayslice.slice->rangelit.end);
            }
            strb ret = NULL;
            strbprintf(&ret,
                "pineslice1d_range_%s(%s.ptr, %s, %s%s)",
                typename,
                access.str,
                start.str,
                end.str,
                expr.arrayslice.slice->rangelit.inclusive ? " + 1" : ""
            );

            mastrfree(end);
            mastrfree(start);
            mastrfree(access);
            strbfree(typename);

            return (MaybeAllocStr){
                .str = ret,
                .alloced = true,
            };
        } break;
        case EkGrouping: {
            MaybeAllocStr value = gen_expr(gen, *expr.group);
            strb ret = NULL; strbprintf(&ret, "(%s)", value.str);

            mastrfree(value);
            return (MaybeAllocStr){
                .str = ret,
                .alloced = true,
            };
        } break;
        case EkFnCall:
            return gen_fn_call(gen, expr);
        case EkLiteral:
            return gen_literal_expr(gen, expr);
        case EkUnop:
            return gen_unop_expr(gen, expr);
        case EkBinop:
            return gen_binop_expr(gen, expr);
        case EkNone:
        case EkType:
        default:
            return (MaybeAllocStr){
                .str = "",
                .alloced = false,
            };
    }
}

// returns true if decl needs to be inserted
bool gen_decl_generic_array(Gen *gen, Type type, strb *decl) {
    strb typename = NULL;
    gen_typename(gen, &type, 1, &typename);

    Type st = type;
    strb lengths = NULL;
    while (true) {
        if (st.kind == TkArray) {
            gen_decl_generic(gen, *st.array.of);

            MaybeAllocStr len = gen_expr(gen, *st.array.len);
            strbprintf(&lengths, "[%s]", len.str);
            mastrfree(len);

            st = *st.array.of;
        } else {
            break;
        }
    }

    MaybeAllocStr subtype = gen_type(gen, st);
    strbprintfln(decl, "typedef %s %s%s;", subtype.str, typename, lengths);
    mastrfree(subtype);
    strbfree(lengths);
    strbfree(typename);

    return !gen_find_generated_typedef(gen, *decl);
}

// returns true if decl needs to be inserted
bool gen_decl_generic_slice(Gen *gen, Type type, strb *decl) {
    strb typename = NULL;
    gen_typename(gen, &type, 1, &typename);
    char *t = strtok(typename, "_");

    // PineSlice<N>dDef(
    strbprintf(decl, "%sDef(", t);
    size_t underscore_idx = strlen(t);
    typename[underscore_idx] = '_';

    Type st = type;
    while (true) {
        if (st.kind == TkSlice) {
            gen_decl_generic(gen, *st.slice.of);
            st = *st.slice.of;
        } else {
            break;
        }
    }

    MaybeAllocStr subtype = gen_type(gen, st);
    strbprintfln(decl, "%s, %s);", subtype.str, &typename[underscore_idx + 1]);
    mastrfree(subtype);
    strbfree(typename);

    return !gen_find_generated_typedef(gen, *decl);
}

void gen_decl_generic(Gen *gen, Type type) {
    strb def = NULL;

    switch (type.kind) {
        case TkSlice: {
            bool add = gen_decl_generic_slice(gen, type, &def);
            if (!add) {
                strbfree(def);
                return;
            }
        } break;
        case TkArray: {
            bool add = gen_decl_generic_array(gen, type, &def);
            if (!add) {
                strbfree(def);
                return;
            }

            gen->defs = strbinsert(gen->defs, def, gen->def_loc);
            gen->def_loc += strlen(def);
            arrpush(gen->generated_typedefs, def);
            return;
        } break;
        case TkOption: {
            MaybeAllocStr typestr = gen_type(gen, *type.option.subtype);

            strb typename = NULL;
            gen_typename(gen, type.option.subtype, 1, &typename);

            strbprintfln(&def, "PineOptionDef(%s, %s);", typestr.str, typename);

            strbfree(typename);
            mastrfree(typestr);

            if (gen_find_generated_typedef(gen, def)) {
                strbfree(def);
                return;
            }
        } break;
        case TkTypeDef: {
            // NOTE: no generics right now, just for forward declaration
            // TODO: add typedefs generics

            strb typedeff = NULL;

            Stmnt stmnt = ast_find_decl(gen->ast, type.typedeff);
            if (stmnt.kind == SkStructDecl) {
                strbprintfln(&typedeff, "typedef struct %s %s;", type.typedeff, type.typedeff);
            } else if (stmnt.kind == SkEnumDecl) {
                strbprintfln(&typedeff, "typedef enum %s %s;", type.typedeff, type.typedeff);
            }

            if (gen_find_generated_typedef(gen, typedeff)) {
                strbfree(typedeff);
                return;
            }

            arrpush(gen->generated_typedefs, typedeff);

            gen->defs = strbinsert(gen->defs, typedeff, gen->def_loc);
            gen->def_loc += strlen(typedeff);
            return;
        } break;
        default:
            return;
    }

    gen->defs = strbinsert(gen->defs, def, gen->def_loc);
    gen->def_loc += strlen(def);
    arrpush(gen->generated_typedefs, def);

    bool replaced = strreplace(def, "Def", "Imp");
    assert(replaced);
    gen->code = strbinsert(gen->code, def, gen->code_loc);
    gen->code_loc += strlen(def);

    replaced = strreplace(def, "Imp", "Def");
    assert(replaced);
}

strb gen_decl_proto(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkVarDecl || stmnt.kind == SkConstDecl || stmnt.kind == SkFnDecl);
    const char *name = "";
    Type type = type_none();

    switch (stmnt.kind) {
        case SkVarDecl:
            name = stmnt.vardecl.name.ident;
            type = stmnt.vardecl.type;
            break;
        case SkConstDecl:
            name = stmnt.constdecl.name.ident;
            type = stmnt.constdecl.type;
            break;
        case SkFnDecl:
            assert(stmnt.fndecl.name.kind == EkIdent && ".name is still expected to be Ident");
            name = stmnt.fndecl.name.ident;
            type = stmnt.fndecl.type;
            break;
        default:
            break;
    }

    strb ret = NULL;
    gen_indent(gen);

    MaybeAllocStr vartype = gen_type(gen, type);
    strbprintf(&ret, "%s %s", vartype.str, name);

    mastrfree(vartype);
    return ret;
}

void gen_var_decl(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkVarDecl);
    VarDecl vardecl = stmnt.vardecl;

    strb proto = gen_decl_proto(gen, stmnt);
    gen_write(gen, "%s", proto);
    strbfree(proto);

    if (vardecl.value.kind == EkNone) {
        if (vardecl.type.kind == TkSlice) {
            gen_write(gen, " = ");

            MaybeAllocStr value = gen_literal_expr(gen, expr_literal(
                (Literal){
                    .kind = LitkExprs,
                    .exprs = NULL,
                },
                vardecl.type,
                stmnt.cursors_idx
            ));
            gen_writeln(gen, "%s;", value.str);

            mastrfree(value);
            return;
        }

        gen_writeln(gen, ";");
    } else {
        gen_write(gen, " = ");

        MaybeAllocStr value = gen_expr(gen, vardecl.value);
        gen_writeln(gen, "%s;", value.str);

        mastrfree(value);
    }
}

void gen_const_decl(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkConstDecl);
    ConstDecl constdecl = stmnt.constdecl;

    strb proto = gen_decl_proto(gen, stmnt);
    gen_write(gen, "%s = ", proto);
    strbfree(proto);

    MaybeAllocStr value = gen_expr(gen, constdecl.value);
    gen_writeln(gen, "%s;", value.str);

    mastrfree(value);
}

void gen_var_reassign(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkVarReassign);
    VarReassign varre = stmnt.varreassign;

    gen_indent(gen);

    MaybeAllocStr reassign = gen_expr(gen, varre.name);
    MaybeAllocStr value = gen_expr(gen, varre.value);
    gen_writeln(gen, "%s = %s;", reassign.str, value.str);

    mastrfree(value);
    mastrfree(reassign);
}

void gen_all_defers(Gen *gen) {
    for (ptrdiff_t i = arrlen(gen->defers) - 1; i >= 0; i--) {
        gen_stmnt(gen, gen->defers[i].stmnt);
    }
}
void gen_defers(Gen *gen) {
    for (ptrdiff_t i = arrlen(gen->defers) - 1; i >= 0; i--) {
        if (gen->defers[i].indent == gen->indent) {
            gen_stmnt(gen, gen->defers[i].stmnt);
        }
    }
}

void gen_return(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkReturn);
    gen_all_defers(gen);

    Return ret = stmnt.returnf;

    gen_indent(gen);
    if (ret.value.kind == EkNone) {
        gen_writeln(gen, "return;");
        return;
    }

    MaybeAllocStr value = gen_expr(gen, ret.value);
    gen_writeln(gen, "return %s;", value.str);

    mastrfree(value);
}

void gen_continue(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkContinue);
    gen_defers(gen);
    gen_indent(gen);
    gen_writeln(gen, "continue;");
}

void gen_break(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkBreak);
    gen_defers(gen);
    gen_indent(gen);
    gen_writeln(gen, "break;");
}

void gen_fn_call_stmnt(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkFnCall);
    Expr expr = expr_fncall(stmnt.fncall, type_none(), stmnt.cursors_idx);

    MaybeAllocStr call = gen_fn_call(gen, expr);
    gen_indent(gen);
    gen_writeln(gen, "%s;", call.str);

    mastrfree(call);
}

void gen_if(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkIf);
    If iff = stmnt.iff;

    gen_indent(gen);

    MaybeAllocStr cond = gen_expr(gen, iff.condition);

    if (iff.capturekind != CkNone) {
        gen_writeln(gen, "if (%s.ok) {", cond.str);

        gen->indent++;
        strb proto = gen_decl_proto(gen, *iff.capture.constdecl);
        gen_writeln(gen, "%s = %s.some;", proto, cond.str);
        gen_indent(gen);

        strbfree(proto);
    } else {
        gen_write(gen, "if (%s) ", cond.str);
    }

    gen_block(gen, iff.body);

    if (iff.capturekind != CkNone) {
        gen->indent--;
        gen_indent(gen);
        gen_writeln(gen, "}");
    }

    gen_indent(gen);
    gen_write(gen, "else ");
    gen_block(gen, iff.els);

    mastrfree(cond);
}

void gen_switch(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkSwitch);
    Switch switchf = stmnt.switchf;

    gen_indent(gen);

    MaybeAllocStr switch_value = gen_expr(gen, switchf.value);
    gen_writeln(gen, "switch (%s) {", switch_value.str);
    mastrfree(switch_value);

    for (size_t i = 0; i < arrlenu(switchf.cases); i++) {
        Case casef = switchf.cases[i].casef;

        if (casef.value.kind == EkNone) {
            gen_indent(gen);
            gen_write(gen, "default: ");
            gen_block(gen, casef.body);
            gen_indent(gen);
            gen_writeln(gen, "break;");
            break;
        }

        MaybeAllocStr case_value = gen_expr(gen, casef.value);
        gen_indent(gen);
        gen_write(gen, "case %s: ", case_value.str);
        mastrfree(case_value);

        gen_block(gen, casef.body);
        if (!casef.fall) {
            gen_indent(gen);
            gen_writeln(gen, "break;");
        }
    }

    gen_indent(gen);
    gen_writeln(gen, "}");
}

void gen_for(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkFor);
    For forf = stmnt.forf;

    gen_indent(gen);

    gen_writeln(gen, "{");

    gen_var_decl(gen, *forf.decl);

    MaybeAllocStr cond = gen_expr(gen, forf.condition);
    MaybeAllocStr reassign = gen_expr(gen, forf.reassign->varreassign.name);
    MaybeAllocStr value = gen_expr(gen, forf.reassign->varreassign.value);

    gen_indent(gen);
    gen_write(gen, "for (; %s; %s = %s) ", cond.str, reassign.str, value.str);
    gen_block(gen, forf.body);

    gen_indent(gen);
    gen_writeln(gen, "}");

    mastrfree(value);
    mastrfree(reassign);
    mastrfree(cond);
}

void gen_stmnt(Gen *gen, Stmnt *stmnt) {
    switch (stmnt->kind) {
        case SkNone:
            break;
        case SkDirective:
            gen_directive(gen, *stmnt);
            break;
        case SkExtern:
            gen_extern(gen, *stmnt);
            break;
        case SkDefer:
            gen_push_defer(gen, stmnt->defer);
            break;
        case SkBlock:
            gen_indent(gen);
            gen_block(gen, stmnt->block);
            gen_writeln(gen, "");
            break;
        case SkFnDecl:
            gen_fn_decl(gen, *stmnt, false);
            break;
        case SkStructDecl:
        case SkEnumDecl:
            // do nothing, defs will be resolved later
            break;
        case SkVarDecl:
            gen_var_decl(gen, *stmnt);
            break;
        case SkConstDecl:
            gen_const_decl(gen, *stmnt);
            break;
        case SkVarReassign:
            gen_var_reassign(gen, *stmnt);
            break;
        case SkReturn:
            gen_return(gen, *stmnt);
            break;
        case SkContinue:
            gen_continue(gen, *stmnt);
            break;
        case SkBreak:
            gen_break(gen, *stmnt);
            break;
        case SkFnCall:
            gen_fn_call_stmnt(gen, *stmnt);
            break;
        case SkIf:
            gen_if(gen, *stmnt);
            break;
        case SkSwitch:
            gen_switch(gen, *stmnt);
            break;
        case SkCase:
            // do nothing, case statements are not allowed without switch
            break;
        case SkFall:
            // do nothing, fall statements are not allowed without switch
            break;
        case SkFor:
            gen_for(gen, *stmnt);
            break;
    }
}

void gen_block(Gen *gen, Arr(Stmnt) block) {
    gen_writeln(gen, "{");
    gen->indent++;

    for (size_t i = 0; i < arrlenu(block); i++) {
        gen_stmnt(gen, &block[i]);
    }

    gen_defers(gen);
    gen_pop_defers(gen);
    gen->indent--;
    gen_indent(gen);
    gen_writeln(gen, "}");
}

void gen_main_fn_decl(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkFnDecl);
    FnDecl fndecl = stmnt.fndecl;

    gen_writeln(gen, "int main(int argc, const char **argv) {");
    gen->indent++;

    if (arrlenu(fndecl.args) == 1) {
        Stmnt arg = fndecl.args[0];
        assert(arg.kind == SkConstDecl);

        assert(arg.constdecl.type.kind == TkSlice);
        gen_decl_generic(gen, arg.constdecl.type);

        char *builtin_args =
            "    PineString _PINE_ARGS_[argc];\n"
            "    for (int i = 0; i < argc; i++) {\n"
            "        _PINE_ARGS_[i] = (PineString){.ptr = argv[i], .len = strlen(argv[i])};\n"
            "    }\n"
            // "    PineSlice1d_PineString args = pineslice1d_PineString(_PINE_ARGS_, argc);\n"
        ;
        gen_write(gen, builtin_args);
        gen_writeln(gen, "    PineSlice1d_PineString %s = pineslice1d_PineString(_PINE_ARGS_, argc);", arg.constdecl.name.ident);
    }

    gen_indent(gen);
    gen_block(gen, fndecl.body);
    gen->indent--;
    gen_indent(gen);
    gen_writeln(gen, "}");
}

void gen_fn_decl(Gen *gen, Stmnt stmnt, bool is_extern) {
    assert(stmnt.kind == SkFnDecl);
    FnDecl fndecl = stmnt.fndecl;

    gen->code_loc = strlen(gen->code);
    gen->def_loc = strlen(gen->defs);
    gen_indent(gen);

    assert(stmnt.fndecl.name.kind == EkIdent && ".name is still expected to be Ident");
    if (fndecl.name.kind == EkIdent && streq("main", fndecl.name.ident)) {
        gen_main_fn_decl(gen, stmnt);
        return;
    }

    strb proto = gen_decl_proto(gen, stmnt);
    strb code = NULL;
    strbprintf(&code, "%s(", proto);
    strbfree(proto);

    for (size_t i = 0; i < arrlenu(fndecl.args); i++) {
        Stmnt arg = fndecl.args[i];
        assert(arg.kind == SkConstDecl || arg.kind == SkVarDecl);

        strb arg_proto = gen_decl_proto(gen, arg);
        if (i == 0) {
            strbprintf(&code, "%s", arg_proto);
        } else {
            strbprintf(&code, ", %s", arg_proto);
        }

        strbfree(arg_proto);
    }
    strbprintf(&code, ")");

    gen->in_defs = true;
    gen_writeln(gen, "%s;", code);
    gen->in_defs = false;

    if (fndecl.has_body) {
        gen_write(gen, "%s ", code);
        gen_block(gen, fndecl.body);
    } else if (!is_extern) {
        gen_writeln(gen, "%v;", code);
    }
}

void gen_extern(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkExtern);
    Stmnt *externf = stmnt.externf;

    switch (externf->kind) {
        case SkFnDecl:
            gen_fn_decl(gen, *stmnt.externf, true);
            break;
        case SkVarDecl:
            gen_var_decl(gen, *stmnt.externf);
            break;
        case SkConstDecl:
            gen_const_decl(gen, *stmnt.externf);
            break;
        case SkVarReassign:
            gen_var_reassign(gen, *stmnt.externf);
            break;
        default:
            break;
    }
}

void gen_struct_decl(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkStructDecl);
    StructDecl structd = stmnt.structdecl;

    strb struct_def = NULL;
    assert(stmnt.structdecl.name.kind == EkIdent && ".name is still expected to be Ident");
    strbprintf(&struct_def, "struct %s", structd.name.ident);

    if (gen_find_generated_typedef(gen, struct_def)) {
        strbfree(struct_def);
        return;
    }
    arrpush(gen->generated_typedefs, struct_def);

    gen->def_loc = strlen(gen->defs);
    gen_indent(gen);

    gen->in_defs = true;
    gen_write(gen, "%s", struct_def);
    gen_block(gen, structd.fields);
    gen_writeln(gen, ";");
    gen->in_defs = false;
}

void gen_enum_decl(Gen *gen, Stmnt stmnt) {
    assert(stmnt.kind == SkEnumDecl);
    EnumDecl enumd = stmnt.enumdecl;

    strb enum_def = NULL;
    assert(stmnt.enumdecl.name.kind == EkIdent && ".name is still expected to be Ident");
    strbprintf(&enum_def, "enum %s", enumd.name.ident);

    if (gen_find_generated_typedef(gen, enum_def)) {
        strbfree(enum_def);
        return;
    }

    arrpush(gen->generated_typedefs, enum_def);
    gen->def_loc = strlen(gen->defs);
    gen_indent(gen);

    gen->in_defs = true;

    gen_writeln(gen, "%s {", enum_def);
    gen->indent++;
    for (size_t i = 0; i < arrlenu(enumd.fields); i++) {
        assert(enumd.fields[i].kind == SkConstDecl);
        Stmnt f = enumd.fields[i];

        MaybeAllocStr expr = gen_expr(gen, f.constdecl.value);
        gen_indent(gen);
        gen_writeln(gen, "%s_%s = %s,", enumd.name.ident, f.constdecl.name.ident, expr.str);

        mastrfree(expr);
    }
    gen->indent--;
    gen_writeln(gen, "};");

    gen->in_defs = false;
}

void gen_resolve_def(Gen *gen, Dnode node) {
    for (size_t i = 0; i < arrlenu(node.children); i++) {
        size_t index = 0;
        for (; index < arrlenu(gen->dgraph.names); index++) {
            if (streq(node.children[i], gen->dgraph.names[index])) {
                break;
            }
        }

        gen_resolve_def(gen, gen->dgraph.children[index]);
    }

    Stmnt stmnt = node.us;
    if (stmnt.kind == SkStructDecl) {
        gen_struct_decl(gen, stmnt);
    } else if (stmnt.kind == SkEnumDecl) {
        gen_enum_decl(gen, stmnt);
    }
}

void gen_resolve_defs(Gen *gen) {
    for (size_t i = 0; i < arrlenu(gen->dgraph.children); i++) {
        gen_resolve_def(gen, gen->dgraph.children[i]);
    }
}

void gen_generate(Gen *gen) {
    strbprintf(&gen->defs, "%.*s", builtin_defs_len, builtin_defs);
    strbprintf(&gen->code, "#include \"output.h\"\n");

    for (size_t i = 0; i < arrlenu(gen->ast); i++) {
        Stmnt stmnt = gen->ast[i];
        switch (stmnt.kind) {
            case SkDirective:
                gen_directive(gen, stmnt);
                break;
            case SkExtern:
                gen_extern(gen, stmnt);
                break;
            case SkFnDecl:
                gen_fn_decl(gen, stmnt, false);
                break;
            case SkStructDecl:
            case SkEnumDecl:
                // do nothing, defs will be resolved later
                break;
            case SkVarDecl:
                gen_var_decl(gen, stmnt);
                break;
            case SkConstDecl:
                gen_const_decl(gen, stmnt);
                break;
            case SkVarReassign:
                gen_var_reassign(gen, stmnt);
                break;
            default:
                break;
        }
    }

    gen_resolve_defs(gen);

    strbprintf(&gen->defs, "\n#endif // PINE_DEFS_H");
}
