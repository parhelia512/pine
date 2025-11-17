#ifndef CLI_H
#define CLI_H

#include <stdbool.h>

typedef enum Command {
    CommandNone = -1,
    CommandBuild = 0,
    CommandRun,
    CommandCOUNT,
} Command;

typedef struct Cli {
    Command command;
    bool help;
    bool keepc;
    bool pass_to_prog;
    int argc;
    char **argv;
    char *filename;
} Cli;

void cli_usage(Cli cli, bool force);
Cli cli_parse(char **argv, int argc);

#endif // CLI_H
