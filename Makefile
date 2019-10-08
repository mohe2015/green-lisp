all:
	gcc -flto -Wl,--gc-sections -ffunction-sections -fdata-sections -s -O3 -Wall -Wextra -Werror -fPIC -shared -o libserial-interface.so serial-interface.c
	sudo mv libserial-interface.so /lib64/
	sudo ldconfig -v | grep serial-interface
