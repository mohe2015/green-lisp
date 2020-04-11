all:
	clang -nostdlib -shared -o librandom.so random.c
	strip -s librandom.so
	clang -Os -o main_original main.c -lrandom -L. -Wl,-rpath,.
	./main_original
	clang -o main main.c -lgreen-lisp -L..
	LD_LIBRARY_PATH=.. ./main
