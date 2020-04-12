all:
	clang -nostdlib -shared -o librandom.so random.c
	strip -s -R .hash librandom.so
	clang -o main_original main.c -lrandom -L.
	LD_LIBRARY_PATH=. ./main_original

	clang -o main main.c -lgreen-lisp -L..
	LD_LIBRARY_PATH=.. ./main
