all:
	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -fno-backtrace -g0 -Os -nostdlib -o random.o -c random.c
	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -fno-backtrace -g0 -Os -nostdlib -shared -o librandom.so random.o
	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -fno-backtrace -g0 -Os -o main.o -c main.c
	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -fno-backtrace -g0 -Os -o main main.o -lrandom -L. -Wl,-rpath,.
	strip -R .hash -R .comment librandom.so
	readelf -a librandom.so
	ls --color -l
	./main

# rwxr-xr-x 1 moritz moritz 12864 Apr  9 23:36 librandom.so
# -rwxr-xr-x 1 moritz moritz 16552 Apr  9 23:36 main
