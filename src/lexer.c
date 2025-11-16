#include "include/stb_ds.h"
#include "include/lexer.h"
#include "include/strb.h"
#include "include/utils.h"
#include "string.h"

#define BUF_CAP 255

Token token_none(void) {
    return (Token){.kind = TokNone};
}
Token token_ident(const char *s) {
    return (Token){.kind = TokIdent, .string = s};
}
Token token_intlit(const char *s) {
    return (Token){.kind = TokIntLit, .string = s};
}
Token token_floatlit(const char *s) {
    return (Token){.kind = TokFloatLit, .string = s};
}
Token token_charlit(const char *s) {
    return (Token){.kind = TokCharLit, .string = s};
}
Token token_strlit(const char *s) {
    return (Token){.kind = TokStrLit, .string = s};
}
Token token_directive(const char *s) {
    return (Token){.kind = TokDirective, .string = s};
}

const char *tokenkind_stringify(TokenKind kind) {
    switch (kind) {
        case TokIdent: return "Ident";
        case TokIntLit: return "IntLit";
        case TokFloatLit: return "FloatLit";
        case TokCharLit: return "CharLit";
        case TokStrLit: return "StrLit";
        case TokDirective: return "Directive";
        case TokColon: return "':'";
        case TokSemiColon: return "';'";
        case TokEqual: return "'='";
        case TokLeftAngle: return "'<'";
        case TokRightAngle: return "'>'";
        case TokLeftBracket: return "'('";
        case TokRightBracket: return "')'";
        case TokLeftCurl: return "'{'";
        case TokRightCurl: return "'}'";
        case TokLeftSquare: return "'['";
        case TokRightSquare: return "']'";
        case TokComma: return "','";
        case TokDot: return "'.'";
        case TokCaret: return "'^'";
        case TokPlus: return "'+'";
        case TokMinus: return "'-'";
        case TokStar: return "'*'";
        case TokSlash: return "'/'";
        case TokPercent: return "'%'";
        case TokBackSlash: return "'\\'";
        case TokBar: return "'|'";
        case TokAmpersand: return "'&'";
        case TokTilde: return "'~'";
        case TokExclaim: return "'!'";
        case TokUnderscore: return "'_'";
        case TokQuestion: return "'?'";
        case TokNone: return "";
    }

    return "";
}

// returns strb, needs to be freed
strb token_stringify(Token tok) {
    strb s = NULL;

    switch (tok.kind) {
        case TokIdent:
            strbprintf(&s, "Ident(%s)", tok.string);
            break;
        case TokIntLit:
            strbprintf(&s, "IntLit(%s)", tok.string);
            break;
        case TokFloatLit:
            strbprintf(&s, "FloatLit(%s)", tok.string);
            break;
        case TokCharLit:
            strbprintf(&s, "CharLit('%s')", tok.string);
            break;
        case TokStrLit:
            strbprintf(&s, "StrLit(\"%s\")", tok.string);
            break;
        case TokDirective:
            strbprintf(&s, "Directive(\"%s\")", tok.string);
            break;
        case TokColon:
        case TokSemiColon:
        case TokEqual:
        case TokLeftAngle:
        case TokRightAngle:
        case TokLeftBracket:
        case TokRightBracket:
        case TokLeftCurl:
        case TokRightCurl:
        case TokLeftSquare:
        case TokRightSquare:
        case TokComma:
        case TokDot:
        case TokCaret:
        case TokPlus:
        case TokMinus:
        case TokStar:
        case TokSlash:
        case TokPercent:
        case TokBackSlash:
        case TokBar:
        case TokAmpersand:
        case TokTilde:
        case TokExclaim:
        case TokUnderscore:
        case TokQuestion:
        case TokNone: {
            const char *kind = tokenkind_stringify(tok.kind);
            strbprintf(&s, "%s", kind);
        } break;
    }

    return s;
}

void print_tokens(Token *tokens) {
    for (size_t i = 0; i < arrlenu(tokens); i++) {
        Token tok = tokens[i];
        strb s = token_stringify(tok);
        printfln("%s", s);
        strbfree(s);
    }
}

static void move_cursor(Lexer *lex) {
    if (lex->ch == '\n') {
        lex->cursor.row += 1;
        lex->cursor.col = 1;
    } else {
        lex->cursor.col += 1;
    }
}

static void push_buffer(Lexer *lex) {
    STRPUSH(lex->buf, BUF_CAP, lex->buf_len, lex->ch);
    move_cursor(lex);
}

static void reset_buffer(Lexer *lex) {
    strclear(lex->buf);
    lex->buf_len = 0;
}

static void resolve_buffer(Lexer *lex) {
    if (strlen(lex->buf) > 0) {
        arrpush(lex->cursors, lex->cursor);

        Token tok;
        uint64_t u64 = 0;
        double f64 = 0;

        if (streq(lex->buf, "_")) {
            tok = (Token){ .kind = TokUnderscore };
        } else if (parse_u64(lex->buf, &u64)) {
            tok = token_intlit(strdup(lex->buf));
        }  else if (parse_f64(lex->buf, &f64)) {
            tok = token_floatlit(strdup(lex->buf));
        } else if (lex->is_directive) {
            tok = token_directive(strdup(lex->buf));
            lex->is_directive = false;
        } else {
            tok = token_ident(strdup(lex->buf));
        }
        arrpush(lex->tokens, tok);
    }

    reset_buffer(lex);
}

static void push_token(Lexer *lex, Token tok) {
    lex->cursor.col += 1;
    arrpush(lex->cursors, lex->cursor);
    arrpush(lex->tokens, tok);
}

static void resolve_buffer_push_token(Lexer *lex, Token tok) {
    resolve_buffer(lex);
    push_token(lex, tok);
}

// exits with 1 if failed
// static char strtochar(const char *s) {
//     if (strlen(s) == 1) {
//         return s[0];
//     }
//
//     if (streq(s, "\\\\")) {
//         return '\\';
//     } else if (streq(s, "\\'")) {
//         return '\'';
//     } else if (streq(s, "\\\"")) {
//         return '\"';
//     } else if (streq(s, "\\n")) {
//         return '\n';
//     } else if (streq(s, "\\r")) {
//         return '\r';
//     } else if (streq(s, "\\t")) {
//         return '\t';
//     } else if (streq(s, "\\v")) {
//         return '\v';
//     } else if (streq(s, "\\f")) {
//         return '\v';
//     } else if (streq(s, "\\a")) {
//         return '\v';
//     } else if (streq(s, "\\b")) {
//         return '\v';
//     } else if (streq(s, "\\e")) {
//         return '\v';
//     }
//
//     // TODO: support \x, etc
//     comp_elog("escape character %s not implemented yet", s);
// }

Lexer lexer_init(void) {
    Lexer lex = {
        .tokens = NULL,
        .cursors = NULL,

        .ch = 0,
        .buf = {0},
        .buf_len = 0,
        .cursor = (Cursor){1, 1},

        .ignore_index = -1,
        .in_single_line_comment = false,
        .in_block_comment = false,
        .escaped = false,
        .in_quotes = false,
        .in_double_quotes = false,
        .is_directive = false,
    };

    return lex;
}

Lexer lexer(const char *source) {
    Lexer lex = lexer_init();

    for (size_t i = 0; i < strlen(source); i++) {
        const char ch = source[i];
        lex.ch = ch;

        if (lex.ignore_index != -1 && i == (size_t)lex.ignore_index)  {
            lex.ignore_index = -1;
            move_cursor(&lex);
            continue;
        }

        if (lex.in_single_line_comment) {
            move_cursor(&lex);
            if (ch == '\n') lex.in_single_line_comment = false;
            continue;
        }

        if (lex.in_block_comment && ch == '*' && AT(source, strlen(source), i + 1) == '/') {
            lex.ignore_index = i + 1;
            lex.in_block_comment = false;
            move_cursor(&lex);
            continue;
        } else if (lex.in_block_comment) {
            move_cursor(&lex);
            continue;
        }

        if (lex.escaped) {
            lex.escaped = false;
            push_buffer(&lex);
            continue;
        }

        if (lex.in_quotes && ch != '\'' && ch != '\\') {
            push_buffer(&lex);
            continue;
        }

        if (lex.in_double_quotes && ch != '"' && ch != '\\') {
            push_buffer(&lex);
            continue;
        }

        switch (ch) {
            case ' ':
            case '\r':
            case '\t':
            case '\n':
                if (ch == '\r') continue;
                resolve_buffer(&lex);
                move_cursor(&lex);
                break;
            case '#':
                resolve_buffer(&lex);
                lex.is_directive = true;
                move_cursor(&lex);
                break;
            case '\'':
                if (lex.in_quotes) {
                    lex.in_quotes = false;
                    arrpush(lex.cursors, lex.cursor);
                    arrpush(lex.tokens, token_charlit(strdup(lex.buf)));
                    reset_buffer(&lex);
                } else {
                    resolve_buffer(&lex);
                    lex.in_quotes = true;
                    move_cursor(&lex);
                }
                break;
            case '"':
                if (lex.in_double_quotes) {
                    lex.in_double_quotes = false;
                    arrpush(lex.cursors, lex.cursor);
                    arrpush(lex.tokens, token_strlit(strdup(lex.buf)));
                    reset_buffer(&lex);
                } else {
                    resolve_buffer(&lex);
                    lex.in_double_quotes = true;
                    move_cursor(&lex);
                }
                break;
            case '.': {
                uint64_t u64 = 0;
                if (AT(source, strlen(source), i + 1) == '.') {
                    lex.ignore_index = i + 1;
                    resolve_buffer_push_token(&lex, (Token){.kind = TokDot});
                    move_cursor(&lex);
                    push_token(&lex, (Token){.kind = TokDot});
                } else if (parse_u64(lex.buf, &u64)) {
                    push_buffer(&lex);
                } else {
                    resolve_buffer_push_token(&lex, (Token){.kind = TokDot});
                }
            } break;
            case '?':
                resolve_buffer_push_token(&lex, (Token){.kind = TokQuestion});
                break;
            case ':':
                resolve_buffer_push_token(&lex, (Token){.kind = TokColon});
                break;
            case '(':
                resolve_buffer_push_token(&lex, (Token){.kind = TokLeftBracket});
                break;
            case ')':
                resolve_buffer_push_token(&lex, (Token){.kind = TokRightBracket});
                break;
            case '{':
                resolve_buffer_push_token(&lex, (Token){.kind = TokLeftCurl});
                break;
            case '}':
                resolve_buffer_push_token(&lex, (Token){.kind = TokRightCurl});
                break;
            case '<':
                resolve_buffer_push_token(&lex, (Token){.kind = TokLeftAngle});
                break;
            case '>':
                resolve_buffer_push_token(&lex, (Token){.kind = TokRightAngle});
                break;
            case '[':
                resolve_buffer_push_token(&lex, (Token){.kind = TokLeftSquare});
                break;
            case ']':
                resolve_buffer_push_token(&lex, (Token){.kind = TokRightSquare});
                break;
            case '=':
                resolve_buffer_push_token(&lex, (Token){.kind = TokEqual});
                break;
            case '!':
                resolve_buffer_push_token(&lex, (Token){.kind = TokExclaim});
                break;
            case ';':
                resolve_buffer_push_token(&lex, (Token){.kind = TokSemiColon});
                break;
            case ',':
                resolve_buffer_push_token(&lex, (Token){.kind = TokComma});
                break;
            case '+':
                resolve_buffer_push_token(&lex, (Token){.kind = TokPlus});
                break;
            case '-':
                resolve_buffer_push_token(&lex, (Token){.kind = TokMinus});
                break;
            case '*':
                if (AT(source, strlen(source), i + 1) == '/') {
                    lex.ignore_index = i + 1;
                    lex.in_block_comment = false;
                    move_cursor(&lex);
                } else {
                    resolve_buffer_push_token(&lex, (Token){.kind = TokStar});
                }
                break;
            case '^':
                resolve_buffer_push_token(&lex, (Token){.kind = TokCaret});
                break;
            case '|':
                resolve_buffer_push_token(&lex, (Token){.kind = TokBar});
                break;
            case '&':
                resolve_buffer_push_token(&lex, (Token){.kind = TokAmpersand});
                break;
            case '~':
                resolve_buffer_push_token(&lex, (Token){.kind = TokTilde});
                break;
            case '/': {
                char next = AT(source, strlen(source), i + 1);
                if (next == '/') {
                    lex.ignore_index = i + 1;
                    lex.in_single_line_comment = true;
                    move_cursor(&lex);
                } else if (next == '*') {
                    lex.ignore_index = i + 1;
                    lex.in_block_comment = true;
                } else {
                    resolve_buffer_push_token(&lex, (Token){.kind = TokSlash});
                }
            } break;
            case '%':
                resolve_buffer_push_token(&lex, (Token){.kind = TokPercent});
                break;
            case '\\':
                if (lex.in_double_quotes || lex.in_quotes) {
                    lex.escaped = true;
                    push_buffer(&lex);
                } else {
                    resolve_buffer_push_token(&lex, (Token){.kind = TokBackSlash});
                }
                break;
            default:
                push_buffer(&lex);
                break;
        }
    }

    return lex;
}
