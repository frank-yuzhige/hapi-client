CC           = gcc
CFLAGS       = -fPIC -g #-pedantic -Wall -Wextra -ggdb3
LDFLAGS      = -shared
RM           = rm -f

%.o: %.c
	$(CC) $(CFLAGS) $< -c

lib%.so: %.o
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

all: libbroken-show.so

.PHONY: clean
clean:
	-${RM} *.o *.so
