all:
	clang -nostdlib -shared -o librandom.so random.c
	strip -s -R .hash librandom.so
	clang -Os -o main_original main.c -lrandom -L. -Wl,-rpath,.
	clang -o main main.c -lgreen-lisp -L..
	./main_original
	LD_LIBRARY_PATH=.. ./main
