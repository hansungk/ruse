CXXFLAGS += -g -std=c++17 -Wall -Wextra -Wno-unused-function \
            -Wno-unused-parameter -fno-omit-frame-pointer
CXXFLAGS += -fsanitize=address,leak,undefined
PROG := ruse
SRCS := main.cc driver.cc sema.cc parser.cc ast.cc lexer.cc source.cc format.cc
OBJS := $(SRCS:.cc=.o)

all: $(PROG)

$(PROG): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(PROG) $(OBJS)

$(OBJS): fmt/core.h
sema.o: sema.h parser.h source.h types.h
parser.o: parser.h ast.h
driver.o: driver.h ast.h parser.h sema.h

.SUFFIXES: .cc .o
.cc.o:
	$(CXX) $(CXXFLAGS) -c -o $@ $<

.PHONY: test clean
test: $(PROG)
	./test.sh

clean:
	rm -f $(OBJS) $(PROG)
