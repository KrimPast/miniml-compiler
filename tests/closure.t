  $ compile () { ../src/main.exe $1; }

  $ add_start () { 
  > echo "
  > .global main
  > main:
  >     li a0, $2
  >     li a1, ${3:-0}
  >     call $1
  >     li a7, 94
  >     ecall"; 
  > }

  $ run () { 
  > riscv64-linux-gnu-gcc $1 -o a.out &&
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./a.out
  > }

  $ compile_and_run() {
  > ASM=$(basename "$1" .mml).S
  > compile $1 >> $ASM
  > run $ASM
  > }

Partial function using test
  $ echo "
  > let add a b = a + b
  > let func x y = 
  >     let f = add x in
  >     let s = f y in 
  >     s" > closure.mml

*** Closures ***
Partial function using test
  $ add_start func 56 75 > closure.S
  $ compile_and_run closure.mml
  [131]

  $ add_start func 101 91 > closure.S
  $ compile_and_run closure.mml
  [192]
