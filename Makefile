all:
	gcc -flto -Wl,--gc-sections -ffunction-sections -fdata-sections -O3 -Wall -Wextra -Werror -fPIC -shared -o main.so main.c
