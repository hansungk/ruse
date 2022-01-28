CFLAGS += -g -std=c11 -Wall -Wextra -Wno-unused-function
CFLAGS += -fsanitize=address,leak,undefined
PROG := ruse
SRCS := main.c codegen.c check.c parse.c lex.c map.c
OBJS := $(SRCS:.c=.o)

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
