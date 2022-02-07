CXXFLAGS += -g -std=c++17 -I. -Wall -Wextra -Wno-unused-function \
            -Wno-unused-parameter -fno-omit-frame-pointer
CXXFLAGS += -fsanitize=address,leak,undefined
PROG := ruse
SRCS := src/main.cc src/driver.cc src/sema.cc src/parse.cc src/ast.cc \
        src/lex.cc src/source.cc src/format.cc
OBJS := $(SRCS:.cc=.o)

all: $(PROG)

$(PROG): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(PROG) $(OBJS)

$(OBJS): fmt/core.h
sema.o: sema.h parse.h source.h types.h
parse.o: parse.h ast.h
driver.o: driver.h ast.h parse.h sema.h

.SUFFIXES: .cc .o
.cc.o:
	$(CXX) $(CXXFLAGS) -c -o $@ $<

.PHONY: test clean
test: $(PROG)
	./test.sh

clean:
	rm -f $(OBJS) $(PROG)
