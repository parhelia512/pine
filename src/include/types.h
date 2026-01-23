#ifndef TYPES_H
#define TYPES_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include "strb.h"

typedef struct Type Type;
typedef struct Expr Expr;

typedef enum TypeKind {
    TkNone,

    TkVoid,
    TkBool,

    TkChar,
    TkString,
    TkCstring,

    TkI8,
    TkI16,
    TkI32,
    TkI64,
    TkIsize,

    TkU8,
    TkU16,
    TkU32,
    TkU64,
    TkUsize,

    TkF32,
    TkF64,

    TkUntypedInt,
    TkUntypedUint, // NOTE: variable of untyped uint cannot receive a value of untyped int
    TkUntypedFloat,
    TkUntypedString,

    TkRange,
    TkSlice,
    TkArray,
    TkPtr,
    TkOption,

    TkTypeDef,
    TkTypeId,

    TkPoison,
} TypeKind;

typedef struct Slice {
    Type *of;
} Slice;

typedef struct Array {
    Type *of;
    Expr *len; // if NULL, infer len
} Array;

typedef struct Option {
    Type *subtype;
    bool is_null;
    bool gen_option;
} Option;

typedef struct Range {
    Type *subtype;
} Range;

typedef struct Type {
    size_t cursors_idx;
    TypeKind kind;
    bool constant;

    union {
        Range range;
        Slice slice;
        Array array;
        Type *ptr_to;
        Option option;
        const char *typedeff;
    };
} Type;

#define CONSTNESS bool
#define TYPECONST true
#define TYPEVAR   false

Type type_from_string(const char *t);
strb string_from_type(Type t);
bool is_untyped(Type t);

Type type_none(void);
Type type_void(CONSTNESS constant, size_t index);
Type type_bool(CONSTNESS constant, size_t index);
Type type_char(CONSTNESS constant, size_t index);
Type type_string(TypeKind kind, CONSTNESS constant, size_t index);
Type type_number(TypeKind kind, CONSTNESS constant, size_t index);
Type type_range(Range v, CONSTNESS constant, size_t index);
Type type_slice(Slice v, CONSTNESS constant, size_t index);
Type type_array(Array v, CONSTNESS constant, size_t index);
Type type_ptr(Type *v, CONSTNESS constant, size_t index);
Type type_option(Option v, CONSTNESS constant, size_t index);
Type type_typedef(const char *v, CONSTNESS constant, size_t index);
Type type_poison(void);

#endif // TYPES_H
