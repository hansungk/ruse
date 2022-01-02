CFLAGS += -g -std=c11 -Wall -Wextra -Wno-unused-function
CFLAGS += -fsanitize=address,leak,undefined
PROG := ruse
SRCS := main.c codegen.c check.c parse.c lex.c map.c
OBJS := $(SRCS:.c=.o)
DEPS := $(SRCS:.c=.d)

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS)

.SUFFIXES: .c .o
.c.o:
	$(CC) $(CFLAGS) -MMD -MP -c -o $@ $<

.PHONY: test clean
test: $(PROG)
	./test.sh

clean:
	rm -f $(OBJS) $(DEPS) $(PROG)

-include $(DEPS)
