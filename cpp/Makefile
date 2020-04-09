all:
	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -Os -nostdlib -o random.o -c random.c
	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -Os -nostdlib -shared -o librandom.so random.o
	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -Os -o main.o -c main.c
	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -Os -o main main.o -lrandom -L. -Wl,-rpath,.
	strip -R .hash -R .comment librandom.so
	readelf -a librandom.so
	./main
