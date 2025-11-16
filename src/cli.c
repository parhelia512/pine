#include <stdarg.h>
#include <stdlib.h>
#include "include/cli.h"
#include "include/utils.h"

const char *cli_commands[CommandCOUNT] = { "build", "run" };

static Cli cli_init(char ***argv, int *argc) {
    return (Cli){
        .help = false,
        .command = CommandNone,
        .keepc = false,
        .filename = "",
        .pass_to_prog = false,
        .argv = *argv,
        .argc = *argc,
    };
}

static char *cli_args_peek(Cli *cli) {
    if (cli->argc == 0) {
        comp_elog("expected another argument");
    }

    return cli->argv[0];
}

static char *cli_args_next(Cli *cli) {
    if (cli->argc == 0) {
        comp_elog("expected another argument");
    }


    char *arg = cli->argv[0];
    cli->argv += 1;
    cli->argc -= 1;
    return arg;
}

static bool cli_is_command(const char *arg) {
    for (int i = 0; i < CommandCOUNT; i++) {
        if (streq(arg, cli_commands[i])) return true;
    }
    return false;
}

static void cli_parse_help(Cli *cli) {
    if (cli->help) {
        comp_elog("unexpected help, help option already set");
    }
    cli->help = true;

    if (cli->argc != 0) {
        char *arg = cli_args_next(cli);
        comp_elog("unexpected %s option after help", arg);
    }
}

static void cli_parse_build(Cli *cli) {
    if (cli->command != CommandNone) {
        comp_elog("unexpected build, %s option already set", cli_commands[cli->command]);
    }

    cli->command = CommandBuild;
    char *arg = cli_args_peek(cli);
    if (cli_is_command(arg)) {
        comp_elog("unexpected %s, expected filename or help", arg);
    }

    cli->filename = arg;
}

static void cli_parse_run(Cli *cli) {
    if (cli->command != CommandNone) {
        comp_elog("unexpected run, %s option already set", cli_commands[cli->command]);
    }

    cli->command = CommandRun;
    char *arg = cli_args_peek(cli);
    if (cli_is_command(arg)) {
        comp_elog("unexpected %s, expected filename or help", arg);
    }

    cli->filename = arg;
}

void cli_usage(Cli cli, bool force) {
    if (!cli.help && !force) {
        return;
    }

    switch (cli.command) {
        case CommandBuild:
            printfln("USAGE:");
            printfln("    build [filename]");
            printfln("    generate executable with entry point file");
            exit(0);
        case CommandRun:
            printfln("USAGE:");
            printfln("    run [filename]");
            printfln("    generate executable with entry point file and immediately run it");
            exit(0);
        default:
            printfln("USAGE:");
            printfln("    build [filename.cur] | build executable");
            printfln("    run [filename.cur] | build and run executable");
            printfln("    help | print this usage message (can be used after a command for specific usage)");
            exit(force);
    }
}

Cli cli_parse(char **argv, int argc) {
    Cli cli = cli_init(&argv, &argc);
    cli_args_next(&cli);

    while (cli.argc != 0) {
        char *arg = cli_args_next(&cli);

        if (streq(arg, "build")) {
            cli_parse_build(&cli);
        } else if (streq(arg, "run")) {
            cli_parse_run(&cli);
        } else if (streq(arg, "help")) {
            cli_parse_help(&cli);
        } else if (streq(arg, "-keepc")) {
            cli.keepc = true;
        } else if (streq(arg, "--")) {
            cli.pass_to_prog = true;
            break;
        }
    }

    return cli;
}
