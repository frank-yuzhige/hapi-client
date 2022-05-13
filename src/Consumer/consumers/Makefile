CC     = clang
CFLAGS = -O1 -I./../../../test/test-api/broken-arith

%.ll : %.c
	$(CC) $(CFLAGS) -S -emit-llvm $<

ll: c1.ll

.PHONY: clean
clean:
	-${RM} *.o *.so *.ll
