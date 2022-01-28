PROG := ruse
SRCS := main.c codegen.c check.c parse.c lex.c map.c
OBJS := $(SRCS:.c=.o)

CFLAGS += -g -std=c11 -Wall -Wextra -Wno-unused-function
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
	CFLAGS += -fsanitize=address,leak,undefined
endif

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS)

$(OBJS): ruse.h stb_ds.h

.SUFFIXES: .c .o
.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: test clean
test: $(PROG)
	./test.sh

clean:
	rm -f $(OBJS) $(PROG)
