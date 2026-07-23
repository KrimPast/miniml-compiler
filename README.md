# Simple MiniML compiler
## Implemented
- Compilation of recursive `factorial`, `fibonacci` and `gcd` functions.
- Support of local variables.
- Support of complex arithmetic equations.
- Closures.
- `if`-`then`-`else` clause.
## Building
```sh
dune build --profile release
```
## Usage
Go to build folder:
```sh
cd ./_build/default/src/
```
In this folder you will see `main.exe`.

After executing this program and putting as first argument miniML program, the RISC-V assembly code will be output to the console, which is what the compiler produces.

If you want to execute this code, you need to have RISC-V architecture on your device or download special utilities:
- GCC for RISC-V
- QEMU and its supporting for RISC-V

For example, here are the required packages for Arch Linux:
### Arch Linux
```sh
sudo pacman -S riscv64-linux-gnu-gcc qemu-user qemu-system-riscv qemu-tools
```

## Building source code in assembler and executing binaries
You can find examples of miniML code in `./examples/` directory.
```sh
./main.exe infile.mml > main.S
```
Don't forget put `main` body to use your function.
Example for `factorial`:
```S
main:
    li a0, 5
    call fac
    li a7, 94
    ecall
```
And after:
```sh
riscv64-linux-gnu-gcc main.S -o main.out && 
qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./a.out
```

### Where is result
You will see it if you enter this:
```sh
echo $?
```
