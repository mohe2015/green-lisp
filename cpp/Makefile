all:
	clang -Os -o random.o -c random.c
	clang -Os -shared -o librandom.so random.o
	strip -s random.o
	strip -s librandom.so
