#lang racket

;; Cubot Note S: https://en.wikipedia.org/wiki/ARM_Cortex-A7
;; / armhf (armv7 with floating point unit)
;; https://developer.arm.com/architectures/cpu-architecture/a-profile/exploration-tools
;; https://developer.arm.com/docs/ddi0597/e/base-instructions-alphabetic-order
;; https://developer.arm.com/docs/ddi0595/e/aarch32-system-registers
;; https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads


;; syscall: r7 params: r0 r1 r2 r3 r4 r5
;; swi #0
;; error: r0