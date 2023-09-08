# Haskell RISC-V Emulator

## Installing
```
$ cabal install
```

## Usage
```
$ cabal run haskell-riscv-emulator <path_to_elf_executable>
```

## Example
Save the following to a file named ```hello.s```:
```
.global _start

_start:
  addi a7, zero, 64
  addi a0, zero, 1
  la a1, helloworld
  addi a2, zero, 13
  ecall

  addi a7, zero, 93
  addi a0, zero, 0
  ecall

helloworld:
  .ascii "Hello World\n"
```

Now assemble and link with:
```
$ riscv64-linux-gnu-as hello.s -o hello.o
$ riscv64-linux-gnu-gcc -o hello hello.o -nostdlib -static
```

This produces the executable ```hello```.
Now run:
```
$ cabal run haskell-riscv-emulator hello
Hello World
```
