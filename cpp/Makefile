all:
	clang -Os -nostdlib -o random.o -c random.c
	clang -Os -nostdlib -shared -o librandom.so random.o
	clang -Os -o main.o -c main.c
	clang -o main main.o -lrandom -L. -Wl,-rpath,.
	strip -s random.o
	strip -R .gnu.hash -R .hash -R .comment -R .eh_frame_hdr -R .eh_frame librandom.so
	readelf -a librandom.so
	./main
