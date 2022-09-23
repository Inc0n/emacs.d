
TOP = __(skeletor-underscore-proj-name)___tb
NAME = __PROJECT-NAME__
SRC = src/__PROJECT-NAME__.vhdl
TEST_SRC= src/test-__PROJECT-NAME__.vhdl
FLAGS = --std=08

analyze:
	ghdl analyze $(FLAGS) $(SRC) $(TEST_SRC)
	ghdl elaborate $(FLAGS) $(TOP)

all: analyze test

test: analyze
	ghdl run $(FLAGS) $(TOP)

# clean:
	# rm -f *.vvp *.vcd

