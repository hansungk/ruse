CFLAGS += -g -std=c11 -Wall -Wextra -Wno-unused-function
# CFLAGS += -fsanitize=address,leak,undefined
PROG := ruse
SRCS := main.c codegen.c check.c parse.c lex.c map.c
OBJS := $(SRCS:.c=.o)

all: $(PROG)

$(PROG): $(OBJS)
	@echo CCLD"\t"$@
	@$(CC) $(CFLAGS) -o $(PROG) $(OBJS)

$(OBJS): ruse.h stb_ds.h

.SUFFIXES: .c .o
.c.o:
	@echo CC"\t"$@
	@$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: check clean
check: $(PROG)
	@./test.sh

clean:
	rm -f $(OBJS) $(PROG)
