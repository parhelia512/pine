#include <assert.h>
#include "include/strb.h"
#include "include/utils.h"
#include "include/types.h"
#include "include/exprs.h"

Type type_none(void) {
    return (Type){.kind = TkNone};
}

Type type_void(CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkVoid,
        .constant = constant,
        .cursors_idx = index,
    };
}

Type type_bool(CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkBool,
        .constant = constant,
        .cursors_idx = index,
    };
}

Type type_char(CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkChar,
        .constant = constant,
        .cursors_idx = index,
    };
}

Type type_string(TypeKind kind, CONSTNESS constant, size_t index) {
    return (Type){
        .kind = kind,
        .constant = constant,
        .cursors_idx = index,
    };
}

Type type_cstring(CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkCstring,
        .constant = constant,
        .cursors_idx = index,
    };
}

Type type_number(TypeKind kind, CONSTNESS constant, size_t index) {
    return (Type){
        .kind = kind,
        .constant = constant,
        .cursors_idx = index,
    };
}

Type type_range(Range v, CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkRange,
        .constant = constant,
        .cursors_idx = index,
        .range = v,
    };
}

Type type_slice(Slice v, CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkSlice,
        .constant = constant,
        .cursors_idx = index,
        .slice = v,
    };
}

Type type_array(Array v, CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkArray,
        .constant = constant,
        .cursors_idx = index,
        .array = v,
    };
}

Type type_ptr(Type *v, CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkPtr,
        .constant = constant,
        .cursors_idx = index,
        .ptr_to = v,
    };
}

Type type_option(Option v, CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkOption,
        .constant = constant,
        .cursors_idx = index,
        .option = v,
    };
}

Type type_typedef(const char *v, CONSTNESS constant, size_t index) {
    return (Type){
        .kind = TkTypeDef,
        .constant = constant,
        .cursors_idx = index,
        .typedeff = v,
    };
}

Type type_poison(void) {
    return (Type){
        .kind = TkPoison,
    };
}

Type type_from_string(const char *t) {
    if (streq(t, "void")) {
        return (Type){.kind = TkVoid};
    } else if (streq(t, "bool")) {
        return (Type){.kind = TkBool};
    } else if (streq(t, "char")) {
        return (Type){.kind = TkChar};
    } else if (streq(t, "string")) {
        return (Type){.kind = TkString};
    } else if (streq(t, "cstring")) {
        return (Type){.kind = TkCstring};
    } else if (streq(t, "i8")) {
        return (Type){.kind = TkI8};
    } else if (streq(t, "i16")) {
        return (Type){.kind = TkI16};
    } else if (streq(t, "i32")) {
        return (Type){.kind = TkI32};
    } else if (streq(t, "i64")) {
        return (Type){.kind = TkI64};
    } else if (streq(t, "isize")) {
        return (Type){.kind = TkIsize};
    } else if (streq(t, "u8")) {
        return (Type){.kind = TkU8};
    } else if (streq(t, "u16")) {
        return (Type){.kind = TkU16};
    } else if (streq(t, "u32")) {
        return (Type){.kind = TkU32};
    } else if (streq(t, "u64")) {
        return (Type){.kind = TkU64};
    } else if (streq(t, "usize")) {
        return (Type){.kind = TkUsize};
    } else if (streq(t, "f32")) {
        return (Type){.kind = TkF32};
    } else if (streq(t, "f64")) {
        return (Type){.kind = TkF64};
    }

    return (Type){.kind = TkNone};
}

strb string_from_type(Type t) {
    strb ret = NULL;

    switch (t.kind) {
        case TkNone:
            break;
        case TkPoison:
            break;

        case TkTypeDef:
            strbprintf(&ret, "%s", t.typedeff);
            break;

        case TkPtr: {
            strb sub = string_from_type(*t.ptr_to);
            strbprintf(&ret, "*%s", sub);
        } break;
        case TkRange: {
            strb sub = string_from_type(*t.range.subtype);
            strbprintf(&ret, "[..]%s", sub);
        } break;
        case TkSlice: {
            strb sub = string_from_type(*t.slice.of);
            strbprintf(&ret, "[]%s", sub);
            strbfree(sub);
        } break;
        case TkArray: {
            strb sub = string_from_type(*t.array.of);
            strbprintf(&ret, "[%s]%s", t.array.len->lit, sub);
            strbfree(sub);
        } break;
        case TkOption: {
            strb sub = string_from_type(*t.option.subtype);
            strbprintf(&ret, "?%s", sub);
            strbfree(sub);
        } break;

        case TkTypeId:
            strbprintf(&ret, "typeid");
            break;

        case TkVoid:
            strbprintf(&ret, "void");
            break;
        case TkBool:
            strbprintf(&ret, "bool");
            break;

        case TkChar:
            strbprintf(&ret, "char");
            break;
        case TkUntypedString:
            strbprintf(&ret, "untyped_string");
            break;
        case TkString:
            strbprintf(&ret, "string");
            break;
        case TkCstring:
            strbprintf(&ret, "cstring");
            break;

        case TkUntypedInt:
            strbprintf(&ret, "untyped_int");
            break;
        case TkUntypedUint:
            strbprintf(&ret, "untyped_uint");
            break;
        case TkUntypedFloat:
            strbprintf(&ret, "untyped_float");
            break;
        case TkF32:
            strbprintf(&ret, "f32");
            break;
        case TkF64:
            strbprintf(&ret, "f64");
            break;
        case TkU8:
            strbprintf(&ret, "u8");
            break;
        case TkU16:
            strbprintf(&ret, "u16");
            break;
        case TkU32:
            strbprintf(&ret, "u32");
            break;
        case TkU64:
            strbprintf(&ret, "u64");
            break;
        case TkUsize:
            strbprintf(&ret, "usize");
            break;
        case TkI8:
            strbprintf(&ret, "i8");
            break;
        case TkI16:
            strbprintf(&ret, "i16");
            break;
        case TkI32:
            strbprintf(&ret, "i32");
            break;
        case TkI64:
            strbprintf(&ret, "i64");
            break;
        case TkIsize:
            strbprintf(&ret, "isize");
            break;
    }

    return ret;
}

bool is_untyped(Type t) {
    switch (t.kind) {
    case TkUntypedUint:
    case TkUntypedInt:
    case TkUntypedFloat:
        return true;
    default:
        return false;
    }
}
