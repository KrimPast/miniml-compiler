  $ compile () { ../src/main.exe $1; }

  $ add_start () { 
  > echo "
  > .global _start
  > _start:
  >     li a0, $2
  >     li a1, ${3:-0}
  >     call $1
  >     li a7, 94
  >     ecall"; 
  > }

  $ run () { 
  > riscv64-linux-gnu-as $1 -o temp.o &&
  > riscv64-linux-gnu-ld temp.o &&
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./a.out
  > }

  $ compile_and_run() {
  > ASM=$(basename "$1" .mml).S
  > compile $1 >> $ASM
  > run $ASM
  > }

Factorial:
  $ echo "
  > let rec fac n =
  > if n <= 1 then 1
  > else 
  >     let prev = fac (n - 1) in
  >     n * prev" > fac.mml

Fibonacci:
  $ echo "
  > let rec fib n =
  >   if n <= 1 then 1
  >   else 
  > 	let n1 = fib n-1 in
  > 	let n2 = fib n-2 in
  > 	n1 + n2" > fib.mml

Gcd:
  $ echo "
  > let rec gcd a b =
  >   if b = 0 then a
  >   else 
  >       let r = a - (a / b) * b in
  >       gcd b r" > gcd.mml

*** Factorial tests ***
Factorial test (n = 4)
  $ add_start fac 4 > fac.S
  $ compile_and_run fac.mml
  [24]

Factorial test (n = 5)
  $ add_start fac 5 > fac.S
  $ compile_and_run fac.mml
  [120]

Factorial test (n = 6)
  $ add_start fac 6 > fac.S
  $ compile_and_run fac.mml
  [208]


*** Fibonacci tests ***
Fibonacci test (n = 5)
  $ add_start fib 5 > fib.S
  $ compile_and_run fib.mml
  [8]

Fibonacci test (n = 6)
  $ add_start fib 6 > fib.S
  $ compile_and_run fib.mml
  [13]

Fibonacci test (n = 7)
  $ add_start fib 7 > fib.S
  $ compile_and_run fib.mml
  [21]

*** Gcd tests ***
  $ add_start gcd 21 14 > gcd.S
  $ compile_and_run gcd.mml
  [7]

  $ add_start gcd 14 21 > gcd.S
  $ compile_and_run gcd.mml
  [7]

  $ add_start gcd 14 0 > gcd.S
  $ compile_and_run gcd.mml
  [14]

  $ add_start gcd 0 14 > gcd.S
  $ compile_and_run gcd.mml
  [14]

  $ add_start gcd 686 512 > gcd.S
  $ compile_and_run gcd.mml
  [2]
