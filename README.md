# green-lisp - An eco-friendly lisp 

## Usage

## Installation

```bash
sudo pacman -S sbcl
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
sbcl --eval '(ql-dist:install-dist "http://dists.cl21.org/cl21.txt")' --eval '(ql:quickload :cl21)' --quit

mkdir ~/.config/common-lisp
nano ~/.config/common-lisp/source-registry.conf
(:source-registry
  (:tree (:home "Documents/green-lisp"))
  :inherit-configuration)
```

## Author

* Moritz Hedtke (Moritz.Hedtke@t-online.de)

## Copyright

Copyright (c) 2019 Moritz Hedtke (Moritz.Hedtke@t-online.de)

## License

Licensed under the AGPL-3.0 License.


## Useful commands

xxd -b binary.bin | less
avr-objdump -d hidden/niborobolib/code/nibo2/tutorial/kapitel_4/obj-nibo2-m128-16/kapitel_4.elf | less
avr-objdump --disassemble-all --architecture=avr51 --target=binary test.bin

avr-objcopy -I binary -O ihex test.bin test.ihex
avrdude -c stk500v2 -P /dev/ttyACM0 -p atmega128 -B 2 -U flash:w:test.ihex 

