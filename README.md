# green-lisp
An eco-friendly lisp

# Useful commands

xxd -b binary.bin | less
avr-objdump -d hidden/niborobolib/code/nibo2/tutorial/kapitel_4/obj-nibo2-m128-16/kapitel_4.elf | less
avr-objdump --disassemble-all --architecture=avr51 --target=binary test.bin
