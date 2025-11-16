#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "include/exprs.h"
#include "include/keywords.h"
#include "include/parser.h"
#include "include/lexer.h"
#include "include/stmnts.h"
#include "include/strb.h"
#include "include/types.h"
#include "include/utils.h"
#include "include/stb_ds.h"

#define ERRORS_MAX 5

static void warn(Parser *parser, size_t i, const char *msg, ...) {
    eprintf("%s:%lu:%lu " TERM_YELLOW "warning" TERM_END ": ", parser->filename, parser->cursors[i].row, parser->cursors[i].col);

    va_list args;
    va_start(args, msg);

    veprintfln(msg, args);

    va_end(args);
}

static void elog(Parser *parser, size_t i, const char *msg, ...) {
    parser->error_count++;
    eprintf("%s:%lu:%lu " TERM_RED "error" TERM_END ": ", parser->filename, parser->cursors[i].row, parser->cursors[i].col);

    va_list args;
    va_start(args, msg);

    veprintfln(msg, args);

    va_end(args);

    if (parser->error_count > ERRORS_MAX) {
        exit(1);
    }
}

Expr expr_from_keyword(Parser *parser, Keyword kw) {
    switch (kw) {
        case KwTrue:
            return expr_true((size_t)parser->cursors_idx);
        case KwFalse:
            return expr_false((size_t)parser->cursors_idx);
        case KwNull: {
            Type *subtype = ealloc(sizeof(Type)); subtype->kind = TkNone;
            return expr_null(
                type_option(
                    (Option){
                        .is_null = true,
                        .gen_option = false,
                        .subtype = subtype,
                    },
                    TYPECONST,
                    (size_t)parser->cursors_idx
                ),
                (size_t)parser->cursors_idx
            );
        }
        default:
            elog(parser, parser->cursors_idx, "expected an expression, got keyword %s", keyword_stringify(kw));
            return expr_none();
    }
}

static Directive directive_map(const char *str) {
    if (streq(str, "link")) {
        return (Directive){ .kind = DkLink };
    } else if (streq(str, "syslink")) {
        return (Directive){ .kind = DkSyslink };
    } else if (streq(str, "output")) {
        return (Directive){ .kind = DkOutput };
    } else if (streq(str, "O0")) {
        return (Directive){ .kind = DkO0 };
    } else if (streq(str, "O1")) {
        return (Directive){ .kind = DkO1 };
    } else if (streq(str, "O2")) {
        return (Directive){ .kind = DkO2 };
    } else if (streq(str, "O3")) {
        return (Directive){ .kind = DkO3 };
    } else if (streq(str, "Odebug")) {
        return (Directive){ .kind = DkOdebug };
    } else if (streq(str, "Ofast")) {
        return (Directive){ .kind = DkOfast };
    } else if (streq(str, "Osmall")) {
        return (Directive){ .kind = DkOsmall };
    }

    return (Directive){ .kind = DkNone };
}

Directive parser_get_directive(Parser *parser, const char *word) {
    Directive d = directive_map(word);
    if (d.kind == DkNone) {
        elog(parser, parser->cursors_idx, "\"#%s\" is not a directive", word);
    }
    return d;
}

Parser parser_init(Lexer lex, const char *filename) {
    return (Parser){
        .tokens = lex.tokens,
        .in_func_decl_args = false,
        .in_enum_decl = false,

        .filename = filename,
        .cursors = lex.cursors,
        .cursors_idx = -1,
        .error_count = 0,
    };
}

Token peek(Parser *parser) {
    if (arrlen(parser->tokens) == 0) {
        return token_none();
    }

    return parser->tokens[0];
}

Token peek_after(Parser *parser) {
    if (arrlen(parser->tokens) == 0) {
        return token_none();
    }

    return parser->tokens[1];
}

Token next(Parser *parser) {
    if (arrlen(parser->tokens) == 0) {
        return token_none();
    }

    parser->cursors_idx += 1;
    Token tok = parser->tokens[0];
    arrdel(parser->tokens, 0);
    return tok;
}

Token expect(Parser *parser, TokenKind expected) {
    Token tok = next(parser);
    if (tok.kind == TokNone) {
        elog(parser, parser->cursors_idx, "expected token %s when no more tokens left", tokenkind_stringify(expected));
    }

    if (tok.kind != expected) {
        elog(parser, parser->cursors_idx, "expected token %s, got %s", tokenkind_stringify(expected), tokenkind_stringify(tok.kind));
    }

    return tok;
}

typedef enum IdentifersKind {
    IkType,
    IkKeyword,
    IkIdent,
} IdentifersKind;

const char *identifierskind_stringify(IdentifersKind i) {
    switch (i) {
        case IkType: return "Type";
        case IkKeyword: return "Keyword";
        case IkIdent: return "Ident";
    }

    return "";
}

typedef struct Identifiers {
    IdentifersKind kind;

    union {
        Type type;
        Keyword keyword;
        Expr expr;
    };
} Identifiers;

static Identifiers convert_ident(Parser *parser, Token tok) {
    if (tok.kind != TokIdent) {
        return (Identifiers){
            .kind = IkIdent,
            .expr = expr_ident("", type_none(), parser->cursors_idx),
        };
    }

    Keyword k = keyword_map(tok.string);
    if (k != KwNone) {
        return (Identifiers){
            .kind = IkKeyword,
            .keyword = k,
        };
    }

    Type t = type_from_string(tok.string);
    if (t.kind != TkNone) {
        return (Identifiers){
            .kind = IkType,
            .type = t,
        };
    }

    return (Identifiers){
        .kind = IkIdent,
        .expr = expr_ident(tok.string, type_none(), (size_t)parser->cursors_idx),
    };
}

Stmnt parse_next_stmnt(Parser *parser) {
    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        if (tok.kind == TokLeftCurl) {
            return parser_parse(parser);
        }

        next(parser);
        if (tok.kind == TokSemiColon) {
            return stmnt_none();
        } else if (tok.kind == TokComma) {
            // next(parser);
            assert(false && "parsing to next statement failed at comma");
        } else if (tok.kind == TokRightCurl) {
            return stmnt_none();
        }
    }

    return stmnt_none();
}

// expects = already nexted
// <ident> = 
Stmnt parse_var_reassign(Parser *parser, Expr ident, bool expect_semicolon) {
    Expr expr = parse_expr(parser);
    if (expect_semicolon) expect(parser, TokSemiColon);

    return stmnt_varreassign((VarReassign){
        .type = type_none(),
        .name = ident,
        .value = expr,
    }, (size_t)parser->cursors_idx);
}

Expr parse_end_literal(Parser *parser, Type type) {
    Expr lit = expr_literal((Literal){
        .kind = LitkNone,
    }, type, (size_t)parser->cursors_idx);

    bool is_stmnts = false;
    Arr(Expr) exprs = NULL;
    Arr(Stmnt) stmnts = NULL;

    bool first = true;
    while (peek(parser).kind != TokRightCurl) {
        if (first) {
            if (peek(parser).kind == TokDot) {
                // expecting var reassign
                // { .
                next(parser);
                Token tok = expect(parser, TokIdent);
                Identifiers convert = convert_ident(parser, tok);
                is_stmnts = true;
                if (convert.kind == IkIdent) {
                    expect(parser, TokEqual);
                    Stmnt stmnt = parse_var_reassign(parser, convert.expr, false);
                    arrpush(stmnts, stmnt);
                } else {
                    elog(parser, parser->cursors_idx, "expected identifer in compound literal, got %s", identifierskind_stringify(convert.kind));
                    Stmnt stmnt = stmnt_none();
                    arrpush(stmnts, stmnt);
                }
            } else {
                Expr expr = parse_expr(parser);
                arrpush(exprs, expr);
            }
            first = false;
            continue;
        }

        expect(parser, TokComma);
        if (peek(parser).kind == TokRightCurl) {
            break;
        }

        if (is_stmnts) {
            expect(parser, TokDot);
            Token tok = expect(parser, TokIdent);
            Identifiers convert = convert_ident(parser, tok);
            if (convert.kind == IkIdent) {
                expect(parser, TokEqual);
                Stmnt stmnt = parse_var_reassign(parser, convert.expr, false);
                arrpush(stmnts, stmnt);
            } else {
                elog(parser, parser->cursors_idx, "expected identifer in compound literal, got %s", identifierskind_stringify(convert.kind));
                Stmnt stmnt = stmnt_none();
                arrpush(stmnts, stmnt);
            }
        } else {
            Expr expr = parse_expr(parser);
            arrpush(exprs, expr);
        }
    }

    if (is_stmnts) {
        lit.literal.kind = LitkVars;
        lit.literal.vars = stmnts;
    } else {
        lit.literal.kind = LitkExprs;
        lit.literal.exprs = exprs;
    }

    expect(parser, TokRightCurl);
    return lit;
}

Type typedef_from_ident(Expr ident) {
    assert(ident.kind == EkIdent);
    
    return type_typedef(ident.ident, TYPEVAR, ident.cursors_idx);
}

Type parse_type(Parser *parser) {
    Type type = type_none();
    Token tok = peek(parser);

    switch (tok.kind) {
        case TokQuestion: {
            size_t index = (size_t)parser->cursors_idx;

            next(parser);
            Type *subtype = ealloc(sizeof(Type)); *subtype = parse_type(parser);

            type = type_option((Option){
                .subtype = subtype,
                .is_null = false,
                .gen_option = false,
            }, TYPEVAR, index);
        } break;
        case TokStar:
        case TokCaret: {
            Type *of = ealloc(sizeof(Type)); of->kind = TkNone;

            type = type_ptr(
                of,
                TYPECONST ? tok.kind == TokCaret : TYPEVAR,
                (size_t)parser->cursors_idx
            );
            next(parser);
            Type *subtype = ealloc(sizeof(Type)); *subtype = parse_type(parser);
            type.ptr_to = subtype;
        } break;
        case TokLeftSquare: {
            Type *of = ealloc(sizeof(Type)); of->kind = TkNone;
            next(parser);
            Token after = peek(parser);

            if (after.kind == TokRightSquare) {
                next(parser);
                type = type_slice((Slice){
                    .of = of,
                }, TYPEVAR, (size_t)parser->cursors_idx);
            } else {
                Expr *len = ealloc(sizeof(Expr)); 
                if (after.kind == TokIntLit) {
                    *len = parse_expr(parser);
                    expect(parser, TokRightSquare);
                } else if (after.kind == TokUnderscore) {
                    next(parser);
                    expect(parser, TokRightSquare);
                    len->kind = EkNone;
                } else {
                    elog(parser, (size_t)parser->cursors_idx, "expected an integer, underscore, or empty []");
                    len->kind = EkNone;
                }

                type = type_array((Array){
                    .len = len,
                    .of = of,
                }, TYPEVAR, (size_t)parser->cursors_idx);
            }

            Type *subtype = ealloc(sizeof(Type)); *subtype = parse_type(parser);
            if (type.kind == TkSlice) {
                type.slice.of = subtype;
            } else if (type.kind == TkArray) {
                type.array.of = subtype;
            }
        } break;
        case TokIdent: {
            next(parser);
            Identifiers convert = convert_ident(parser, tok);

            if (convert.kind == IkType) {
                type = convert.type;
            } else if (convert.kind == IkKeyword) {
                elog(parser, parser->cursors_idx, "expected a type, got %s", tokenkind_stringify(tok.kind));
                type = type_none();
            } else {
                type = typedef_from_ident(convert.expr);
            }
        } break;
        default:
            break;
    }

    return type;
}

Expr parse_primary(Parser *parser) {
    Token tok = peek(parser);

    switch (tok.kind) {
        case TokLeftCurl:
            next(parser);
            return parse_end_literal(parser, type_none());
        case TokLeftSquare: {
            Type type = parse_type(parser);
            tok = peek(parser);
            if (tok.kind == TokLeftCurl) {
                next(parser);
                return parse_end_literal(parser, type);
            } else {
                strb t = string_from_type(type);
                elog(parser, parser->cursors_idx, "unexpected type %s", t);
                strbfree(t);
                return expr_none();
            }
        } break;
        case TokIdent: {
            Identifiers convert = convert_ident(parser, tok);
            if (convert.kind == IkIdent) {
                next(parser);
                tok = peek(parser);

                if (tok.kind == TokDot) {
                    // <ident>.
                    next(parser);
                    return parse_field_access(parser, convert.expr);
                } else if (tok.kind == TokLeftSquare) {
                    // <ident>[
                    next(parser);
                    return parse_array_index(parser, convert.expr);
                } else if (tok.kind == TokLeftCurl) {
                    // <ident>{
                    // ident must be a typedef
                    next(parser);
                    Type type = typedef_from_ident(convert.expr);
                    return parse_end_literal(parser, type);
                } else if (streq(convert.expr.ident, "c") && tok.kind == TokStrLit) {
                    next(parser);
                    return expr_cstrlit(tok.string, (size_t)parser->cursors_idx);
                }

                // <ident>
                return convert.expr;
            } else if (convert.kind == IkKeyword) {
                next(parser);
                return expr_from_keyword(parser, convert.keyword);
            } else if (convert.kind == IkType) {
                Type type = parse_type(parser);
                tok = peek(parser);

                if (tok.kind == TokLeftCurl) {
                    next(parser);
                    return parse_end_literal(parser, type);
                } else {
                    return expr_type(type, (size_t)parser->cursors_idx);
                }
            } else {
                elog(parser, parser->cursors_idx, "unexpected identifier %s", tok.string);
                return expr_none();
            }
        } break;
        case TokIntLit:
            next(parser);
            return expr_intlit(
                tok.string,
                type_integer(
                    TkUntypedInt,
                    TYPECONST,
                    (size_t)parser->cursors_idx
                ),
                (size_t)parser->cursors_idx
            );
        case TokFloatLit:
            next(parser);
            Expr expr = expr_floatlit(
                tok.string,
                type_decimal(
                    TkUntypedFloat,
                    TYPECONST,
                    (size_t)parser->cursors_idx
                ),
                (size_t)parser->cursors_idx
            );
            return expr;
        case TokCharLit:
            next(parser);
            return expr_charlit(tok.string, (size_t)parser->cursors_idx);
        case TokStrLit:
            next(parser);
            return expr_strlit(tok.string, (size_t)parser->cursors_idx);
        case TokLeftBracket: {
            next(parser);
            size_t index = (size_t)parser->cursors_idx;
            Expr *expr = ealloc(sizeof(Expr)); *expr = parse_expr(parser);
            expect(parser, TokRightBracket);

            return expr_group(expr, type_none(), index);
        } break;
        default:
            elog(parser, parser->cursors_idx, "unexpected token %s", tokenkind_stringify(tok.kind));
            return expr_none();
    }

    return expr_none();
}

Expr parse_end_fn_call(Parser *parser, Expr ident) {
    size_t index = (size_t)parser->cursors_idx;

    bool is_stmnts = false;
    Arr(Expr) exprs = NULL;
    Arr(Stmnt) stmnts = NULL;

    Token tok = peek(parser);
    if (tok.kind != TokRightBracket) {
        if (tok.kind == TokDot) {
            is_stmnts = true;
            next(parser);
            tok = expect(parser, TokIdent);
            Identifiers convert = convert_ident(parser, tok);
            if (convert.kind == IkIdent) {
                expect(parser, TokEqual);
                Stmnt stmnt = parse_var_reassign(parser, convert.expr, false);
                arrpush(stmnts, stmnt);
            } else {
                elog(parser, parser->cursors_idx, "expected identifer in function call, got %s", identifierskind_stringify(convert.kind));
                Stmnt stmnt = stmnt_none();
                arrpush(stmnts, stmnt);
            }
        } else {
            is_stmnts = false;
            arrpush(exprs, parse_expr(parser));
        }
        for (tok = peek(parser); tok.kind == TokComma; tok = peek(parser)) {
            next(parser);

            if (is_stmnts) {
                expect(parser, TokDot);
                tok = expect(parser, TokIdent);
                Identifiers convert = convert_ident(parser, tok);
                if (convert.kind == IkIdent) {
                    expect(parser, TokEqual);
                    Stmnt stmnt = parse_var_reassign(parser, convert.expr, false);
                    arrpush(stmnts, stmnt);
                } else {
                    elog(parser, parser->cursors_idx, "expected identifer in function call, got %s", identifierskind_stringify(convert.kind));
                    Stmnt stmnt = stmnt_none();
                    arrpush(stmnts, stmnt);
                }
            } else {
                arrpush(exprs, parse_expr(parser));
            }
        }
    }

    expect(parser, TokRightBracket);
    Expr *name = ealloc(sizeof(Expr)); *name = ident;
    FnCall fncall = {
        .name = name,
    };
    if (is_stmnts) {
        fncall.arg_kind = LitkVars;
        fncall.args.vars = stmnts;
    } else {
        fncall.arg_kind = LitkExprs;
        fncall.args.exprs = exprs;
    }
    return expr_fncall(fncall, type_none(), index);
}

Expr parse_fn_call(Parser *parser, Expr ident) {
    Expr expr = ident;
    if (ident.kind == EkNone) {
        expr = parse_primary(parser);
    }

    while (true) {
        Token tok = peek(parser);
        if (tok.kind == TokLeftBracket) {
            next(parser);
            expr = parse_end_fn_call(parser, expr);
        } else {
            break;
        }
    }

    return expr;
}

Stmnt stmnt_from_fncall(Expr expr) {
    assert(expr.kind == EkFnCall);
    return stmnt_fncall(expr.fncall, expr.cursors_idx);
}

Expr expr_from_fncall(Stmnt stmnt) {
    assert(stmnt.kind == SkFnCall);
    return expr_fncall(stmnt.fncall, type_none(), stmnt.cursors_idx);
}

Expr parse_unary(Parser *parser) {
    Token op = peek(parser);
    size_t index = (size_t)parser->cursors_idx;

    if (
        op.kind != TokExclaim &&
        op.kind != TokMinus &&
        op.kind != TokAmpersand &&
        op.kind != TokTilde
    ) {
        if (op.kind == TokIdent && (streq(op.string, "cast") || streq(op.string, "sizeof"))) {
            goto resume;
        }
        return parse_fn_call(parser, expr_none());
    }

resume:
    next(parser);
    // handle cast first
    if (op.kind == TokIdent && streq(op.string, "cast")) {
        expect(parser, TokLeftBracket);
        Type type = parse_type(parser);
        expect(parser, TokRightBracket);

        Expr *right = ealloc(sizeof(Expr)); *right = parse_unary(parser);

        return expr_unop((Unop){
            .kind = UkCast,
            .val = right,
        }, type, index);
    }

    Expr *right = ealloc(sizeof(Expr)); *right = parse_unary(parser);
    switch (op.kind) {
        case TokExclaim:
            return expr_unop((Unop){
                .kind = UkNot,
                .val = right,
            }, type_bool(TYPEVAR, index), index);
        case TokMinus:
            return expr_unop((Unop){
                .kind = UkNegate,
                .val = right,
            }, type_none(), index);
        case TokTilde:
            return expr_unop((Unop){
                .kind = UkBitNot,
                .val = right,
            }, type_none(), index);
        case TokAmpersand:
            return expr_unop((Unop){
                .kind = UkAddress,
                .val = right,
            }, type_none(), index);
        case TokIdent:
            if (streq(op.string, "sizeof")) {
                if (right->kind != EkGrouping) {
                    elog(parser, right->cursors_idx, "expected () after `sizeof`");
                    return expr_none();
                }

                return expr_unop((Unop){
                    .kind = UkSizeof,
                    .val = right,
                }, type_integer(TkUntypedInt, TYPECONST, index), index);
            }
            return expr_none();
        default:
            elog(parser, parser->cursors_idx, "unexpected token %s", tokenkind_stringify(op.kind));
            return expr_none();
    }

    return expr_none();
}

Expr parse_factor(Parser *parser) {
    Expr expr = parse_unary(parser);

    for (Token op = peek(parser); op.kind != TokNone; op = peek(parser)) {
        if (op.kind != TokStar && op.kind != TokSlash && op.kind != TokPercent) {
            break;
        }
        next(parser);

        size_t index = (size_t)parser->cursors_idx;
        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_unary(parser);

        if (op.kind == TokStar) {
            expr = expr_binop((Binop){
                .kind = BkMultiply,
                .left = left,
                .right = right,
            }, type_none(), index);
        } else if (op.kind == TokSlash) {
            expr = expr_binop((Binop){
                .kind = BkDivide,
                .left = left,
                .right = right,
            }, type_none(), index);
        } else {
            expr = expr_binop((Binop){
                .kind = BkMod,
                .left = left,
                .right = right,
            }, type_none(), index);
        }
    }

    return expr;
}

Expr parse_term(Parser *parser) {
    Expr expr = parse_factor(parser);

    for (Token op = peek(parser); op.kind != TokNone; op = peek(parser)) {
        if (op.kind != TokPlus && op.kind != TokMinus) {
            break;
        }
        next(parser);

        size_t index = (size_t)parser->cursors_idx;
        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_factor(parser);

        if (op.kind == TokPlus) {
            expr = expr_binop((Binop){
                .kind = BkPlus,
                .left = left,
                .right = right,
            }, type_none(), index);
        } else {
            expr = expr_binop((Binop){
                .kind = BkMinus,
                .left = left,
                .right = right,
            }, type_none(), index);
        }
    }

    return expr;
}

Expr parse_shift(Parser *parser) {
    Expr expr = parse_term(parser);

    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        size_t index = parser->cursors_idx;
        Token after = peek_after(parser);
        
        if (tok.kind != TokLeftAngle && tok.kind != TokRightAngle) {
            break;
        } else if (tok.kind == TokLeftAngle && after.kind != TokLeftAngle) {
            break;
        } else if (tok.kind == TokRightAngle && after.kind != TokRightAngle) {
            break;
        }
        next(parser); next(parser);

        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_term(parser);

        if (tok.kind == TokLeftAngle) {
            expr = expr_binop((Binop){
                .kind = BkLeftShift,
                .left = left,
                .right = right,
            }, type_integer(TkUntypedInt, TYPEVAR, index), index);
        } else {
            expr = expr_binop((Binop){
                .kind = BkRightShift,
                .left = left,
                .right = right,
            }, type_integer(TkUntypedInt, TYPEVAR, index), index);
        }
    }

    return expr;
}

Expr parse_comparison(Parser *parser) {
    Expr expr = parse_shift(parser);

    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        size_t index = (size_t)parser->cursors_idx;
        if (tok.kind != TokLeftAngle && tok.kind != TokRightAngle) {
            break;
        }
        next(parser);

        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_shift(parser);

        Token after = peek(parser);
        if (after.kind == TokEqual) {
            next(parser);

            if (tok.kind == TokLeftAngle) {
                expr = expr_binop((Binop){
                    .kind = BkLessEqual,
                    .left = left,
                    .right = right,
                }, type_bool(TYPEVAR, index), index);
            } else {
                expr = expr_binop((Binop){
                    .kind = BkGreaterEqual,
                    .left = left,
                    .right = right,
                }, type_bool(TYPEVAR, index), index);
            }
        } else {
            if (tok.kind == TokLeftAngle) {
                expr = expr_binop((Binop){
                    .kind = BkLess,
                    .left = left,
                    .right = right,
                }, type_bool(TYPEVAR, index), index);
            } else {
                expr = expr_binop((Binop){
                    .kind = BkGreater,
                    .left = left,
                    .right = right,
                }, type_bool(TYPEVAR, index), index);
            }
        }
    }

    return expr;
}

Expr parse_equality(Parser *parser) {
    Expr expr = parse_comparison(parser);

    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        size_t index = (size_t)parser->cursors_idx;
        if (tok.kind != TokExclaim && tok.kind != TokEqual) {
            break;
        }
        next(parser);

        Token after = next(parser);
        if (after.kind != TokEqual) {
            break;
        }

        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_comparison(parser);
        if (tok.kind == TokExclaim) {
            expr = expr_binop((Binop){
                .kind = BkInequals,
                .left = left,
                .right = right,
            }, type_bool(TYPEVAR, index), index);
        } else {
            expr = expr_binop((Binop){
                .kind = BkEquals,
                .left = left,
                .right = right,
            }, type_bool(TYPEVAR, index), index);
        }
    }

    return expr;
}

Expr parse_bitwise_and(Parser *parser) {
    Expr expr = parse_equality(parser);

    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        if (tok.kind != TokAmpersand) {
            break;
        }
        next(parser);

        size_t index = (size_t)parser->cursors_idx;
        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_equality(parser);
        expr = expr_binop((Binop){
            .kind = BkBitAnd,
            .left = left,
            .right = right,
        }, type_integer(TkUntypedInt, TYPEVAR, index), index);
    }

    return expr;
}

Expr parse_bitwise_xor(Parser *parser) {
    Expr expr = parse_bitwise_and(parser);

    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        if (tok.kind != TokTilde) {
            break;
        }
        next(parser);

        size_t index = (size_t)parser->cursors_idx;
        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_bitwise_and(parser);
        expr = expr_binop((Binop){
            .kind = BkBitXor,
            .left = left,
            .right = right,
        }, type_integer(TkUntypedInt, TYPEVAR, index), index);
    }

    return expr;
}

Expr parse_bitwise_or(Parser *parser) {
    Expr expr = parse_bitwise_xor(parser);

    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        if (tok.kind != TokBar) {
            break;
        }
        next(parser);

        size_t index = (size_t)parser->cursors_idx;
        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_bitwise_xor(parser);
        expr = expr_binop((Binop){
            .kind = BkBitOr,
            .left = left,
            .right = right,
        }, type_integer(TkUntypedInt, TYPEVAR, index), index);
    }

    return expr;
}

Expr parse_and(Parser *parser) {
    Expr expr = parse_bitwise_or(parser);

    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        if (tok.kind != TokIdent) {
            break;
        }

        if (!streq(tok.string, "and")) {
            break;
        }
        next(parser);

        size_t index = (size_t)parser->cursors_idx;
        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_bitwise_or(parser);
        expr = expr_binop((Binop){
            .kind = BkAnd,
            .left = left,
            .right = right,
        }, type_bool(TYPEVAR, index), index);
    }

    return expr;
}

Expr parse_or(Parser *parser) {
    Expr expr = parse_and(parser);

    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        if (tok.kind != TokIdent) {
            break;
        }

        if (!streq(tok.string, "or")) {
            break;
        }
        next(parser);

        size_t index = (size_t)parser->cursors_idx;
        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = parse_equality(parser);
        expr = expr_binop((Binop){
            .kind = BkOr,
            .left = left,
            .right = right,
        }, type_bool(TYPEVAR, index), index);
    }

    return expr;
}

Expr parse_range(Parser *parser) {
    Expr expr;
    if (peek(parser).kind == TokDot && peek_after(parser).kind == TokDot) {
        expr = expr_none();
    } else {
        expr = parse_or(parser);
    }

    for (Token tok = peek(parser); tok.kind != TokNone; tok = peek(parser)) {
        size_t index = parser->cursors_idx;
        Token after = peek_after(parser);

        if (tok.kind != TokDot) {
            break;
        }
        if (after.kind != TokDot) {
            break;
        }
        next(parser); next(parser);

        Expr *left = ealloc(sizeof(Expr)); *left = expr;
        Expr *right = ealloc(sizeof(Expr)); *right = expr_none();
        bool inclusive = false;

        if (peek(parser).kind == TokEqual) {
            next(parser);

            switch (peek(parser).kind) {
                case TokIdent:
                case TokIntLit:
                case TokFloatLit:
                case TokCharLit:
                    *right = parse_or(parser);
                    break;
                default:
                    break;
            }

            inclusive = true;
        } else {
            switch (peek(parser).kind) {
                case TokIdent:
                case TokIntLit:
                case TokFloatLit:
                case TokCharLit:
                    *right = parse_or(parser);
                    break;
                default:
                    break;
            }
        }

        Type *subtype = ealloc(sizeof(Type)); *subtype = type_integer(TkUntypedInt, TYPECONST, index);
        expr = expr_range((RangeLit){
            .start = left,
            .end = right,
            .inclusive = inclusive,
        }, type_range((Range){
            .subtype = subtype,
        }, TYPECONST, index), index);
    }

    return expr;
}

Expr parse_expr(Parser *parser) {
    return parse_range(parser);
}

Expr parse_array_slice(Parser *parser, Expr expr, Expr *range) {
    Expr *e = ealloc(sizeof(Expr)); *e = expr;
    Expr arrslice = expr_arrayslice((ArraySlice){
        .accessing = e,
        .slice = range,
    }, type_none(), (size_t)parser->cursors_idx);
    return arrslice;
}

// expects [ already nexted
// <expr>[
Expr parse_array_index(Parser *parser, Expr expr) {
    Expr *index = ealloc(sizeof(Expr)); *index = parse_expr(parser);
    expect(parser, TokRightSquare);

    if (index->kind == EkRangeLit) {
        return parse_array_slice(parser, expr, index);
    }

    Expr *e = ealloc(sizeof(Expr)); *e = expr;
    Expr arrindex = expr_arrayindex((ArrayIndex){
        .accessing = e,
        .index = index,
    }, type_none(), (size_t)parser->cursors_idx);

    Token tok = peek(parser);
    if (tok.kind == TokDot) {
        next(parser);
        return parse_field_access(parser, arrindex);
    } else if (tok.kind == TokLeftSquare) {
        next(parser);
        return parse_array_index(parser, arrindex);
    }

    return arrindex;
}

// expects '.' already nexted
// <expr>.
Expr parse_field_access(Parser *parser, Expr expr) {
    size_t index = parser->cursors_idx;

    Expr *front = ealloc(sizeof(Expr)); *front = expr;
    Expr *field = ealloc(sizeof(Expr)); *field = expr_none();

    Expr fa = expr_fieldaccess((FieldAccess){
        .accessing = front,
        .field = field,
        .deref = false,
    }, type_none(), index);

    Token tok = next(parser);
    if (tok.kind == TokAmpersand) {
        // <expr>.&
        fa.fieldacc.deref = true;
    } else {
        // <expr>.<ident>
        Identifiers convert = convert_ident(parser, tok);
        if (convert.kind != IkIdent) {
            elog(parser, parser->cursors_idx, "unexpected token %s after field access", tokenkind_stringify(tok.kind));
            *field = expr_none();
            fa.type = type_none();
        } else {
            *field = convert.expr;
            fa.type = field->type;
        }
    }

    tok = peek(parser);
    if (tok.kind == TokNone) {
        elog(parser, parser->cursors_idx, "expected more tokens");
    } else if (tok.kind == TokDot) {
        next(parser); // already checked if none
        return parse_field_access(parser, fa);
    } else if (tok.kind == TokLeftSquare) {
        next(parser); // already checked if none
        return parse_array_index(parser, fa);
    }

    return fa;
}

// <expr> [+-*/%|&~<<>>]=
Stmnt parse_compound_assignment(Parser *parser, Expr expr, Token op, bool expect_semicolon) {
    size_t op_idx = (size_t)parser->cursors_idx;
    Expr *var = ealloc(sizeof(Expr)); *var = expr;
    Expr *val = ealloc(sizeof(Expr)); *val = parse_expr(parser);
    Expr *group = ealloc(sizeof(Expr));
    *group = expr_group(val, type_none(), parser->cursors_idx);

    if (expect_semicolon) expect(parser, TokSemiColon);

    Stmnt reassign = stmnt_varreassign((VarReassign){
        .name = expr,
        .type = type_none(),
        .value = expr_none(),
    }, parser->cursors_idx);

    Expr binop = expr_binop((Binop){
        .kind = 0,
        .left = var,
        .right = group,
    }, type_none(), op_idx);

    switch (op.kind) {
        case TokPlus:
            binop.binop.kind = BkPlus;
            break;
        case TokMinus:
            binop.binop.kind = BkMinus;
            break;
        case TokStar:
            binop.binop.kind = BkMultiply;
            break;
        case TokSlash:
            binop.binop.kind = BkDivide;
            break;
        case TokPercent:
            binop.binop.kind = BkMod;
            break;
        case TokBar:
            binop.binop.kind = BkBitOr;
            break;
        case TokTilde:
            binop.binop.kind = BkBitXor;
            break;
        case TokAmpersand:
            binop.binop.kind = BkBitAnd;
            break;
        case TokLeftAngle:
            binop.binop.kind = BkLeftShift;
            break;
        case TokRightAngle:
            binop.binop.kind = BkRightShift;
            break;
        default:
            assert(false && "unexpected compound token");
            break;
    }
    reassign.varreassign.value = binop;

    return reassign;
}

// returns VarReassign if possible, else StmntNone
Stmnt parse_possible_assignment(Parser *parser, Expr expr, bool expect_semicolon) {
    Token tok = peek(parser);
    if (tok.kind == TokNone) return stmnt_none();

    // TODO: refactor this so that we don't check for each possible compound operator
    // <expr> [+-*/%|&~<<>>]=
    if (tok.kind == TokPlus      ||
        tok.kind == TokMinus     ||
        tok.kind == TokStar      ||
        tok.kind == TokSlash     ||
        tok.kind == TokPercent   ||
        tok.kind == TokBar       ||
        tok.kind == TokAmpersand ||
        tok.kind == TokTilde     ||
        tok.kind == TokLeftAngle ||
        tok.kind == TokRightAngle
    ) {
        next(parser);
        Token after = next(parser);

        if ((tok.kind == TokLeftAngle && after.kind == TokLeftAngle) || (tok.kind == TokRightAngle && after.kind == TokRightAngle)) {
            Token final = next(parser);
            if (final.kind != TokEqual) {
                elog(parser, parser->cursors_idx, "unexpected token %s", tokenkind_stringify(final.kind));
                return parse_next_stmnt(parser);
            }

            return parse_compound_assignment(parser, expr, tok, expect_semicolon);
        } else if (after.kind != TokEqual) {
            elog(parser, parser->cursors_idx, "unexpected token %s", tokenkind_stringify(after.kind));
            return parse_next_stmnt(parser);
        }

        return parse_compound_assignment(parser, expr, tok, expect_semicolon);
    } else if (tok.kind == TokEqual) {
        next(parser);
        return parse_var_reassign(parser, expr, expect_semicolon);
    }

    return stmnt_none();
}

// CAUTION: can return NULL
Arr(Stmnt) parse_block(Parser *parser, TokenKind start, TokenKind end) {
    if (start != TokNone) {
        expect(parser, start);
    }

    Arr(Stmnt) block = NULL;

    Token tok = peek(parser);
    if (tok.kind == end && start != TokNone) {
        next(parser);
        return block;
    }

    for (Stmnt stmnt = parser_parse(parser); stmnt.kind != SkNone; stmnt = parser_parse(parser)) {
        arrpush(block, stmnt);

        tok = peek(parser);
        if (tok.kind == end) {
            next(parser);
            break;
        }
    }

    if (arrlen(block) == 0) {
        expect(parser, end);
    }

    return block;
}

Stmnt *parse_block_curls(Parser *parser) {
    return parse_block(parser, TokLeftCurl, TokRightCurl);
}

Stmnt parse_fn_decl(Parser *parser, Expr ident) {
    size_t index = (size_t)parser->cursors_idx;

    parser->in_func_decl_args = true;
    Stmnt *args = parse_block(parser, TokLeftBracket, TokRightBracket);
    parser->in_func_decl_args = false;

    Type type = parse_type(parser);
    if (type.kind == TkNone) {
        elog(parser, parser->cursors_idx, "expected return type in function declaration");
        return parse_next_stmnt(parser);
    }

    FnDecl fndecl = {
        .name = ident,
        .args = args,
        .type = type,
    };

    Token tok = peek(parser);
    if (tok.kind == TokLeftCurl) {
        Stmnt *body = parse_block_curls(parser);
        fndecl.body = body;
        fndecl.has_body = true;

        return stmnt_fndecl(fndecl, index);
    } else if (tok.kind == TokSemiColon) {
        next(parser);
        fndecl.body = NULL;
        fndecl.has_body = false;

        return stmnt_fndecl(fndecl, index);
    } else {
        elog(parser, parser->cursors_idx, "expected ';' or '{', got %s", tokenkind_stringify(tok.kind));
        return parse_next_stmnt(parser);
    }
}

Stmnt parse_struct_decl(Parser *parser, Expr ident) {
    size_t index = (size_t)parser->cursors_idx;
    Stmnt *fields = parse_block_curls(parser);

    return stmnt_structdecl((StructDecl){
        .name = ident,
        .fields = fields,
    }, index);
}

Stmnt parse_enum_decl(Parser *parser, Expr ident) {
    size_t index = (size_t)parser->cursors_idx;
    parser->in_enum_decl = true;
    Stmnt *fields = parse_block_curls(parser);
    parser->in_enum_decl = false;

    return stmnt_enumdecl((EnumDecl){
        .name = ident,
        .fields = fields,
    }, index);
}

// <ident> : <type?> :
// type can be none
Stmnt parse_const_decl(Parser *parser, Expr ident, Type type) {
    Token tok = peek(parser);
    if (tok.kind == TokNone) return stmnt_none();

    size_t index = (size_t)parser->cursors_idx;
    if (tok.kind == TokIdent) {
        Identifiers convert = convert_ident(parser, tok);
        if (convert.kind == IkKeyword) {
            switch (convert.keyword) {
                case KwFn:
                    next(parser);
                    return parse_fn_decl(parser, ident);
                case KwStruct:
                    next(parser);
                    return parse_struct_decl(parser, ident);
                case KwEnum:
                    next(parser);
                    return parse_enum_decl(parser, ident);
                case KwTrue:
                case KwFalse:
                case KwNull:
                    break;
                default:
                    elog(parser, index, "unexpected token %s", tokenkind_stringify(tok.kind));
                    return parse_next_stmnt(parser);
            }
        }
    }

    // <ident>: <type,
    if (parser->in_func_decl_args) {
        return stmnt_constdecl((ConstDecl){
            .name = ident,
            .type = type,
            .value = expr_none(),
        }, index);
    }

    Expr expr = parse_expr(parser);
    expect(parser, TokSemiColon);

    // <ident>: <type?> : ;
    if (expr.kind == EkNone) {
        elog(parser, index, "expected expression after \":\" in variable declaration");
        return parse_next_stmnt(parser);
    }

    return stmnt_constdecl((ConstDecl){
        .name = ident,
        .type = type,
        .value = expr,
    }, index);
}

Stmnt parse_var_decl(Parser *parser, Expr ident, Type type, bool has_equal) {
    size_t index = (size_t)parser->cursors_idx;

    VarDecl vardecl = {
        .name = ident,
        .type = type,
        .value = expr_none(),
    };

    // <ident>: <type?> = 
    if (!has_equal) {
        return stmnt_vardecl(vardecl, index);
    }

    Expr expr = parse_expr(parser);
    if (parser->in_func_decl_args) {
        if (peek(parser).kind == TokComma) {
            next(parser);
        }

        vardecl.value = expr;
        return stmnt_vardecl(vardecl, index);
    }
    
    expect(parser, TokSemiColon);
    if (expr.kind == EkNone) {
        elog(parser, index, "expected expression after \"=\" in variable declaration");
        return parse_next_stmnt(parser);
    }

    vardecl.value = expr;
    return stmnt_vardecl(vardecl, index);
}

// <ident> :
Stmnt parse_decl(Parser *parser, Expr ident) {
    Token tok = peek(parser);
    if (tok.kind == TokNone) return stmnt_none();

    if (tok.kind == TokColon) {
        next(parser);
        return parse_const_decl(parser, ident, type_none());
    } else if (tok.kind == TokEqual) {
        next(parser);
        return parse_var_decl(parser, ident, type_none(), true);
    } else {
        Type type = parse_type(parser);
        if (type.kind == TkNone) {
            elog(parser, parser->cursors_idx, "expected a type");
            return parse_next_stmnt(parser);
        }

        tok = peek(parser);
        if (tok.kind == TokNone) return stmnt_none();

        if (tok.kind == TokColon) {
            next(parser);
            return parse_const_decl(parser, ident, type);
        } else if (tok.kind == TokEqual) {
            next(parser);
            return parse_var_decl(parser, ident, type, true);
        } else if (tok.kind == TokSemiColon) {
            next(parser);
            if (type.kind == TkNone) {
                elog(parser, parser->cursors_idx, "expected type for variable declaration since it does not have a value");
                return parse_next_stmnt(parser);
            }
            return parse_var_decl(parser, ident, type, false);
        } else if (tok.kind == TokComma) {
            next(parser);
            if (!parser->in_func_decl_args) {
                elog(parser, parser->cursors_idx, "unexpected comma during declaration");
                return parse_next_stmnt(parser);
            }
            return parse_const_decl(parser, ident, type);
        } else if (tok.kind == TokRightBracket) {
            if (!parser->in_func_decl_args) {
                elog(parser, parser->cursors_idx, "unexpected TokenRb during declaration");
                return parse_next_stmnt(parser);
            }
            return parse_const_decl(parser, ident, type);
        } else {
            elog(parser, parser->cursors_idx, "unexpected token %s", tokenkind_stringify(tok.kind));
            return parse_next_stmnt(parser);
        }
    }

    return stmnt_none();
}

Stmnt parse_ident(Parser *parser, Expr ident) {
    assert(ident.kind == EkIdent);

    Token tok = peek(parser);
    if (tok.kind == TokNone) return stmnt_none();
    
    // <ident>. OR <ident>[
    if (tok.kind == TokDot) {
        next(parser); // already checked if none
        Expr reassigned = parse_field_access(parser, ident);

        tok = peek(parser);
        if (tok.kind == TokNone) return stmnt_none();

        return parse_possible_assignment(parser, reassigned, true);
    } else if (tok.kind == TokLeftSquare) {
        next(parser);
        Expr arrindex = parse_array_index(parser, ident);

        tok = peek(parser);
        if (tok.kind == TokNone) return stmnt_none();

        return parse_possible_assignment(parser, arrindex, true);
    }

    Stmnt assign = parse_possible_assignment(parser, ident, true);
    if (assign.kind != SkNone) return assign;

    switch (tok.kind) {
        case TokColon:
            next(parser);
            return parse_decl(parser, ident);
        case TokLeftBracket: {
            Expr expr = parse_fn_call(parser, ident);
            Stmnt stmnt = stmnt_from_fncall(expr);
            expect(parser, TokSemiColon);
            return stmnt;
        }
        case TokSemiColon:
            if (!parser->in_enum_decl) {
                elog(parser, parser->cursors_idx, "unexpected token %s", tokenkind_stringify(tok.kind));
                return parse_next_stmnt(parser);
            }

            next(parser);
            return stmnt_constdecl((ConstDecl){
                .name = ident,
                .type = type_integer(TkI32, TYPECONST, parser->cursors_idx),
            }, parser->cursors_idx);
        default:
            elog(parser, parser->cursors_idx, "unexpected token %s", tokenkind_stringify(tok.kind));
            return parse_next_stmnt(parser);
    }

    return stmnt_none();
}

Stmnt parse_return(Parser *parser) {
    size_t index = (size_t)parser->cursors_idx;
    Token tok = peek(parser);
    if (tok.kind == TokSemiColon) {
        next(parser);
        return stmnt_return((Return){
            .value = expr_none(),
            .type = type_none(),
        }, index);
    }

    Expr expr = parse_expr(parser);
    expect(parser, TokSemiColon);

    return stmnt_return((Return){
        .value = expr,
        .type = type_none(),
    }, index);
}

Stmnt parse_defer(Parser *parser) {
    size_t index = (size_t)parser->cursors_idx;

    Stmnt *defered = ealloc(sizeof(Stmnt));
    *defered = parser_parse(parser);

    return stmnt_defer(defered, index);
}

Stmnt parse_continue(Parser *parser) {
    size_t index = (size_t)parser->cursors_idx;
    expect(parser, TokSemiColon);

    return stmnt_continue(index);
}

Stmnt parse_break(Parser *parser) {
    size_t index = (size_t)parser->cursors_idx;
    expect(parser, TokSemiColon);

    return stmnt_break(index);
}

Stmnt parse_if(Parser *parser) {
    size_t index = (size_t)parser->cursors_idx;

    expect(parser, TokLeftBracket);
    Expr cond = parse_expr(parser);
    expect(parser, TokRightBracket);

    Token capture_tok = peek(parser);
    Expr capture = expr_none();
    // if (<cond>) <[capture]>
    if (capture_tok.kind == TokLeftSquare) {
        next(parser);
        capture_tok = expect(parser, TokIdent);

        Identifiers convert = convert_ident(parser, capture_tok);
        if (convert.kind == IkIdent) {
            capture = convert.expr;
        } else {
            elog(parser, parser->cursors_idx, "capture must be a unique identifier");
            return parse_next_stmnt(parser);
        }
        expect(parser, TokRightSquare);
    }

    Stmnt *body = parse_block_curls(parser);
    Arr(Stmnt) else_block = NULL;

    Token tok = peek(parser);
    if (tok.kind == TokIdent) {
        Identifiers convert = convert_ident(parser, tok);
        if (convert.kind == IkKeyword && convert.keyword == KwElse) {
            next(parser);
            Token after = peek(parser);
            if (after.kind == TokIdent) {
                Identifiers after_conv = convert_ident(parser, after);
                if (after_conv.kind == IkKeyword && after_conv.keyword == KwIf) {
                    next(parser);
                    arrpush(else_block, parse_if(parser));
                } else {
                    elog(parser, parser->cursors_idx, "unexpected identifier %s after `else`", after.string);
                    return parse_next_stmnt(parser);
                }
            } else {
                else_block = parse_block_curls(parser);
            }
        }
    }

    return stmnt_if((If){
        .condition = cond,
        .body = body,
        .capture.ident = capture,
        .capturekind = capture.kind == EkNone ? CkNone : CkIdent,
        .els = else_block,
    }, index);
}

Stmnt parse_extern(Parser *parser) {
    size_t index = (size_t)parser->cursors_idx;
    Stmnt *stmnt = ealloc(sizeof(Stmnt)); *stmnt = parser_parse(parser);
    return stmnt_extern(stmnt, index);
}

Stmnt parse_for(Parser *parser) {
    size_t index = (size_t)parser->cursors_idx;

    expect(parser, TokLeftBracket);
    Token tok = expect(parser, TokIdent);
    Identifiers convert = convert_ident(parser, tok);
    Expr ident;
    if (convert.kind == IkIdent) {
        ident = convert.expr;
    } else {
        elog(parser, parser->cursors_idx, "expected identifer, got reserved word");
        return parse_next_stmnt(parser);
    }

    // for (i:
    tok = peek(parser);
    if (tok.kind == TokColon) {
        next(parser);
        tok = peek(parser);

        Stmnt *vardecl = ealloc(sizeof(Stmnt));
        if (tok.kind == TokEqual) {
            // for (i :=
            next(parser);
            *vardecl = parse_var_decl(parser, ident, type_none(), true);
        } else if (tok.kind == TokIdent) {
            // for (i: <type>
            Type type = parse_type(parser);
            expect(parser, TokEqual);
            *vardecl = parse_var_decl(parser, ident, type, true);
        } else {
            elog(parser, parser->cursors_idx, "unexpected token %s in for loop", tokenkind_stringify(tok.kind));
            return parse_next_stmnt(parser);
        }

        // for (i: <type?> = <expr>; <cond>
        Expr cond = parse_expr(parser);
        expect(parser, TokSemiColon);

        Stmnt *reassign = ealloc(sizeof(Stmnt));
        tok = peek(parser);
        if (tok.kind == TokIdent) {
            next(parser);

            // for (i: <type?> = <expr>; <cond>; i [+-*/]=
            *reassign = parse_possible_assignment(parser, ident, false);
        }
        expect(parser, TokRightBracket);

        Stmnt *body = parse_block_curls(parser);
        return stmnt_for((For){
            .decl = vardecl,
            .condition = cond,
            .reassign = reassign,
            .body = body,
        }, index);
    } else {
        // TODO: support `for (values) [value]`
        elog(parser, parser->cursors_idx, "expected ':', got %s", tokenkind_stringify(tok.kind));
        return parse_next_stmnt(parser);
    }
}

Stmnt parse_directive(Parser *parser) {
    Token tok = next(parser);

    assert(tok.kind == TokDirective);
    Directive directive = parser_get_directive(parser, tok.string);
    Stmnt d = stmnt_directive(directive, parser->cursors_idx);

    switch (directive.kind) {
        case DkOutput:
        case DkLink:
        case DkSyslink: {
            tok = expect(parser, TokStrLit);
            expect(parser, TokSemiColon);
            d.directive.str = tok.string;
        } break;
        default:
            expect(parser, TokSemiColon);
            break;
    }

    return d;
}

Stmnt parser_parse(Parser *parser) {
    Token tok = peek(parser);
    if (tok.kind == TokNone) return stmnt_none();

    switch (tok.kind) {
        case TokIdent: {
            next(parser); // already checked if none
            Identifiers convert = convert_ident(parser, tok);

            if (convert.kind == IkIdent) {
                return parse_ident(parser, convert.expr);
            } else if (convert.kind == IkKeyword) {
                switch (convert.keyword) {
                    case KwReturn:
                        return parse_return(parser);
                    case KwContinue:
                        return parse_continue(parser);
                    case KwBreak:
                        return parse_break(parser);
                    case KwDefer:
                        return parse_defer(parser);
                    case KwIf:
                        return parse_if(parser);
                    case KwExtern:
                        return parse_extern(parser);
                    case KwFor:
                        return parse_for(parser);
                    default:
                        elog(parser, parser->cursors_idx, "unexpected keyword \"%s\"", tok.string);
                        return parse_next_stmnt(parser);
                }
            }
        } break;
        case TokLeftCurl: {
            size_t index = (size_t)parser->cursors_idx;
            return stmnt_block(parse_block_curls(parser), index);
        } break;
        case TokDirective:
            return parse_directive(parser);
        default:
            next(parser);
            elog(parser, parser->cursors_idx, "unexpected token %s", tokenkind_stringify(tok.kind));
            parse_next_stmnt(parser);
            break;
    }

    return stmnt_none();
}
