CC = gcc
CFLAGS = -Wall -Wextra

SRC_CLI = src/cli.c
BIN_CLI = bin/cli.o
HEADER_CLI = src/include/cli.h

SRC_EVAL = src/eval.c
BIN_EVAL = bin/eval.o
HEADER_EVAL = src/include/eval.h

SRC_EXPRS = src/exprs.c
BIN_EXPRS = bin/exprs.o
HEADER_EXPRS = src/include/exprs.h

SRC_GEN = src/gen.c
BIN_GEN = bin/gen.o
HEADER_GEN = src/include/gen.h

SRC_KEYWORDS = src/keywords.c
BIN_KEYWORDS = bin/keywords.o
HEADER_KEYWORDS = src/include/keywords.h

SRC_LEXER = src/lexer.c
BIN_LEXER = bin/lexer.o
HEADER_LEXER = src/include/lexer.h

SRC_MAIN = src/main.c
BIN_MAIN = bin/main.o

SRC_PARSER = src/parser.c
BIN_PARSER = bin/parser.o
HEADER_PARSER = src/include/parser.h

SRC_SEMA = src/sema.c
BIN_SEMA = bin/sema.o
HEADER_SEMA = src/include/sema.h

SRC_STMNTS = src/stmnts.c
BIN_STMNTS = bin/stmnts.o
HEADER_STMNTS = src/include/stmnts.h

SRC_STRB = src/strb.c
BIN_STRB = bin/strb.o
HEADER_STRB = src/include/strb.h

SRC_TYPECHECK = src/typecheck.c
BIN_TYPECHECK = bin/typecheck.o
HEADER_TYPECHECK = src/include/typecheck.h

SRC_TYPES = src/types.c
BIN_TYPES = bin/types.o
HEADER_TYPES = src/include/types.h

SRC_UTILS = src/utils.c
BIN_UTILS = bin/utils.o
HEADER_UTILS = src/include/utils.h

SRC_BUILTIN_DEFS_TXT = src/pine_builtin_defs.txt
SRC_BUILTIN_DEFS = src/builtin_defs.c
BIN_BUILTIN_DEFS = bin/builtin_defs.o

BINS = $(BIN_CLI) $(BIN_EVAL) $(BIN_GEN) $(BIN_EXPRS) $(BIN_KEYWORDS) $(BIN_LEXER) $(BIN_MAIN) $(BIN_PARSER) $(BIN_SEMA) $(BIN_STMNTS) $(BIN_STRB) $(BIN_TYPECHECK) $(BIN_TYPES) $(BIN_UTILS) $(BIN_BUILTIN_DEFS)

pine: $(BINS)
	$(CC) $(CFLAGS) -o pine $(BINS)

$(SRC_BUILTIN_DEFS): $(SRC_BUILTIN_DEFS_TXT)
	xxd -i -n builtin_defs $(SRC_BUILTIN_DEFS_TXT) > src/builtin_defs.c

$(BIN_BUILTIN_DEFS): $(SRC_BUILTIN_DEFS)
	$(CC) $(CFLAGS) -c $(SRC_BUILTIN_DEFS) -o $(BIN_BUILTIN_DEFS)

$(BIN_CLI): $(SRC_CLI) $(HEADER_CLI)
	$(CC) $(CFLAGS) -c $(SRC_CLI) -o $(BIN_CLI)

$(BIN_EVAL): $(SRC_EVAL) $(HEADER_EVAL)
	$(CC) $(CFLAGS) -c $(SRC_EVAL) -o $(BIN_EVAL)

$(BIN_EXPRS): $(SRC_EXPRS) $(HEADER_EXPRS)
	$(CC) $(CFLAGS) -c $(SRC_EXPRS) -o $(BIN_EXPRS)

$(BIN_GEN): $(SRC_GEN) $(BIN_BUILTIN_DEFS) $(HEADER_GEN)
	$(CC) $(CFLAGS) -c $(SRC_GEN) -o $(BIN_GEN)

$(BIN_KEYWORDS): $(SRC_KEYWORDS) $(HEADER_KEYWORDS)
	$(CC) $(CFLAGS) -c $(SRC_KEYWORDS) -o $(BIN_KEYWORDS)

$(BIN_LEXER): $(SRC_LEXER) $(HEADER_LEXER)
	$(CC) $(CFLAGS) -c $(SRC_LEXER) -o $(BIN_LEXER)

$(BIN_MAIN): $(SRC_MAIN)
	$(CC) $(CFLAGS) -c $(SRC_MAIN) -o $(BIN_MAIN)

$(BIN_PARSER): $(SRC_PARSER) $(HEADER_PARSER)
	$(CC) $(CFLAGS) -c $(SRC_PARSER) -o $(BIN_PARSER)

$(BIN_SEMA): $(SRC_SEMA) $(HEADER_SEMA)
	$(CC) $(CFLAGS) -c $(SRC_SEMA) -o $(BIN_SEMA)

$(BIN_STMNTS): $(SRC_STMNTS) $(HEADER_STMNTS)
	$(CC) $(CFLAGS) -c $(SRC_STMNTS) -o $(BIN_STMNTS)

$(BIN_STRB): $(SRC_STRB) $(HEADER_STRB)
	$(CC) $(CFLAGS) -c $(SRC_STRB) -o $(BIN_STRB)

$(BIN_TYPECHECK): $(SRC_TYPECHECK) $(HEADER_TYPECHECK)
	$(CC) $(CFLAGS) -c $(SRC_TYPECHECK) -o $(BIN_TYPECHECK)

$(BIN_TYPES): $(SRC_TYPES) $(HEADER_TYPES)
	$(CC) $(CFLAGS) -c $(SRC_TYPES) -o $(BIN_TYPES)

$(BIN_UTILS): $(SRC_UTILS) $(HEADER_UTILS)
	$(CC) $(CFLAGS) -c $(SRC_UTILS) -o $(BIN_UTILS)

clean:
	rm -rf bin/*.o pine
