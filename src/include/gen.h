#ifndef GEN_H
#define GEN_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include "exprs.h"
#include "lexer.h"
#include "sema.h"
#include "stmnts.h"
#include "stb_ds.h"
#include "strb.h"

typedef enum OptLevel {
    OlZero,
    OlOne,
    OlTwo,
    OlThree,
    OlDebug,
    OlFast,
    OlSmall,
} OptLevel;

typedef struct CompileFlags {
    bool keepc;
    OptLevel optimisation;
    Arr(const char*) links;
    const char *output;
} CompileFlags;

typedef struct Defer {
    Stmnt *stmnt;
    uint8_t indent;
} Defer;

typedef struct Gen {
    strb code;
    strb defs;

    size_t def_loc;
    size_t code_loc;

    bool in_defs;
    uint8_t indent;

    CompileFlags compile_flags;

    Dgraph dgraph;
    Arr(Defer) defers;
    Arr(Stmnt) ast;
    Arr(const char*) generated_typedefs;
    Arr(Cursor) cursors;
    const char *filename;
} Gen;

typedef struct MaybeAllocStr {
    strb str;
    bool alloced;
} MaybeAllocStr;

void gen_extern(Gen *gen, Stmnt stmnt);
void gen_fn_decl(Gen *gen, Stmnt stmnt, bool is_extern);
void gen_decl_generic(Gen *gen, Type type);
void gen_generate(Gen *gen);

// for_identifier is specifically when the expression is going to be used as text in an identifier
// for example, the length of an array is an expression but used in the type name
MaybeAllocStr _gen_expr(Gen *gen, Expr expr, bool for_indentifier);
MaybeAllocStr gen_type(Gen *gen, Type type);
void gen_typename(Gen *gen, Type *types, size_t types_len, strb *);
void gen_stmnt(Gen *gen, Stmnt *stmnt);
void gen_block(Gen *gen, Arr(Stmnt) stmnt);
Gen gen_init(Arr(Stmnt) ast, Dgraph dgraph, const char *filename, Arr(Cursor) cursors);

#define gen_expr(gen, expr) _gen_expr((gen), (expr), false)
#define gen_expr_for_identifier(gen, expr) _gen_expr((gen), (expr), true)

#endif // GEN_H
