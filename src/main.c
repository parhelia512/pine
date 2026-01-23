#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "include/exprs.h"
#include "include/lexer.h"
#include "include/stmnts.h"
#include "include/strb.h"
#include "include/utils.h"
#include "include/cli.h"
#include "include/parser.h"
#include "include/sema.h"
#include "include/gen.h"

#define STB_DS_IMPLEMENTATION
#include "include/stb_ds.h"

void compile(CompileFlags flags) {
    const char *cc = get_c_compiler();
    strb com = NULL;
    strbprintf(&com, "%s -o %s output.c ", cc, flags.output);

    char *op = "";
    switch (flags.optimisation) {
        case OlZero:
            op = "-O0";
            break;
        case OlOne:
            op = "-O1";
            break;
        case OlTwo:
            op = "-O2";
            break;
        case OlThree:
            op = "-O3";
            break;
        case OlDebug:
            op = "-Og -g";
            break;
        case OlFast:
            op = "-O3";
            break;
        case OlSmall:
            op = "-Os";
            break;
    }
    strbprintf(&com, "%s", op);

    for (size_t i = 0; i < arrlenu(flags.links); i++) {
        strbprintf(&com, " %s", flags.links[i]);
    }

    // if (DEBUG_MODE) {
    //     printf("%s\n", com);
    // }

    FILE *fd = popen(com, "r");
    if (fd == NULL) {
        comp_elog("failed to compile");
    }

    if (pclose(fd) != 0) {
        comp_elog("failed to compile");
    }

    if (!flags.keepc) {
        remove("output.c");
        remove("output.h");
    }

    strbfree(com);
}

// returns executable name
const char *build(Cli cli) {
    char *content = {0};
    bool content_ok = read_entire_file(cli.filename, &content);
    if (!content_ok) {
        comp_elog("failed to read %s", cli.filename);
    }

    Lexer lex = lexer(content);
    if (arrlen(lex.tokens) != arrlen(lex.cursors)) comp_elog("expected length of tokens and length of cursors to be the same");

    Arr(Stmnt) ast = NULL;
    Parser parser = parser_init(lex, cli.filename);
    for (Stmnt stmnt = parser_parse(&parser); stmnt.kind != SkNone; stmnt = parser_parse(&parser)) {
        arrpush(ast, stmnt);
    }

    if (parser.error_count > 0) {
        exit(1);
    }

    Sema sema = sema_init(ast, cli.filename, lex.cursors);
    sema_analyse(&sema);

    if (sema.error_count > 0) {
        exit(1);
    }

    Gen gen = gen_init(ast, sema.dgraph, cli.filename, lex.cursors);
    gen_generate(&gen);
    gen.compile_flags.keepc = cli.keepc;

    write_entire_file("output.h", gen.defs);
    write_entire_file("output.c", gen.code);

    if (strlen(gen.compile_flags.output) == 0) {
        gen.compile_flags.output = filename_from_path(cli.filename);
    }
    compile(gen.compile_flags);

    free(content);
    return gen.compile_flags.output;
}

void run(Cli cli, const char *exe) {
    strb com = NULL;
#if defined(__linux__) || defined(__APPLE__)
    strbprintf(&com, "./%s", exe);
#elif defined(_WIN32)
    strbprintf(&com, "./%s.exe", exe);
#endif

    for (int i = 0; i < cli.argc; i++) {
        strbprintf(&com, " %s", cli.argv[i]);
    }

    FILE *fd = popen(com, "r");
    if (fd == NULL) {
        comp_elog("failed to run `%s`", com);
    }

    char buf[1024];
    while (fgets(buf, sizeof(buf), fd) != NULL) {
        printf("%s", buf);
    }

    pclose(fd);
    strbfree(com);
}

int main(int argc, char **argv) {
    Cli cli = cli_parse(argv, argc);

    cli_usage(cli, false);
    switch (cli.command) {
        case CommandBuild:
        {
            build(cli);
        } break;
        case CommandRun:
        {
            const char *exe = build(cli);
            run(cli, exe);
        } break;
        default:
            printfln(TERM_RED "error" TERM_END ": no command given");
            cli_usage(cli, true);
            break;
    }

    return 0;
}
