# https://www.nesono.com/sites/default/files/lldb%20cheat%20sheet.pdf
LD_LIBRARY_PATH=.. lldb main
settings set target.disable-aslr false
settings show target.disable-aslr
breakpoint set --name main
breakpoint set --name green_lisp_demo
run
stepi

help breakpoint set
breakpoint set -a 0x555555555020

register read
image list
image lookup --address 0x7ffff7fc7000
image dump sections libgreen-lisp.so
image dump symtab libgreen-lisp.so




LD_LIBRARY_PATH=.. gdb main
set disable-randomization off
# https://sourceware.org/gdb/wiki/ReverseDebug
start
target record-full
info record
continue


info registers


break green_lisp_demo
