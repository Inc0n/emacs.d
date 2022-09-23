
TOP = test___PROJECT-NAME__
NAME = __PROJECT-NAME__
SRC = src/__PROJECT-NAME__.v
TEST_SRC= src/test-__PROJECT-NAME__.v
BIN = bin/$(TOP).vvp

$(BIN): $(SRC) $(TEST_SRC)
	@mkdir -p bin
	iverilog -o $(BIN) -s $(TOP) $(SRC) $(TEST_SRC)

.PHONY: all clean test

all: $(BIN)

test: $(BIN)
	vvp $(BIN)

testbench: $(BIN)
	../auto-test-bench.rkt $(BIN) $(NAME).rkt

clean:
	rm -f *.vvp *.vcd

