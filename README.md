# Linre - miniML compiler
## Implemented
- Compilation of recursive `factorial`, `fibonacci` and `gcd` functions.
- Closures, partial function application
- `if`-`then`-`else` clause.
- Support of
    - definition and calling of functions
    - local variables
    - complex arithmetic equations
## Building
```sh
dune build --profile release
```
## Usage
```sh
dune exec -- linre <infile.mml>
```
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
dune exec -- linre infile.mml > main.S
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
riscv64-linux-gnu-gcc main.S && 
qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./a.out
```

### Where is result
You will see it if you enter this:
```sh
echo $?
```
