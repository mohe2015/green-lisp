all:
#	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -Os -nostdlib -o random.o -c random.c
#	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -Os -nostdlib -shared -o librandom.so random.o
#	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -Os -o main.o -c main.c
#	clang -fno-unwind-tables -fno-asynchronous-unwind-tables -Os -o main main.o -lrandom -L. -Wl,-rpath,.
#	strip -s -R .hash -R .comment librandom.so
#	readelf -a librandom.so
#	strip -s -R .hash -R .comment -R .gnu.version_r -R .gnu.version -R .note.ABI-tag main
#	readelf -a main
#	ls --color -l
#	./main
	clang -o main main.c -lgreen-lisp -L..
	LD_LIBRARY_PATH=.. ./main

# rwxr-xr-x 1 moritz moritz 12864 Apr  9 23:36 librandom.so
# -rwxr-xr-x 1 moritz moritz 16552 Apr  9 23:36 main
