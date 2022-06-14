CC           = clang
CFLAGS       = -fPIC -g -fprofile-instr-generate -fcoverage-mapping -fsanitize=address,fuzzer-no-link -fprofile-arcs -ftest-coverage --coverage  #-pedantic -Wall -Wextra -ggdb3
LDFLAGS      = -shared
RM           = rm -f

%.o: %.c
	$(CC) $(CFLAGS) $< -c

lib%.so: %.o
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

all: libbroken-arith.so libgood-arith.so

.PHONY: clean
clean:
	-${RM} *.o *.so
