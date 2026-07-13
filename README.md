# Hardcoded factorial compiler
## Building
```sh
dune build --profile release
```
## Usage
Go to examples folder:
```sh
cd ./_build/default/examples/
```
In this folder you will see `factorial.exe` and `fibonacci.exe`.

After executing the programs, the RISC-V assembly code will be output to the console, which is what the compiler produces.

If you want to execute this code, you need to have RISC-V architecture on your device or download special utilities:
- GCC for RISC-V
- QEMU and its supporting for RISC-V

For example, here are the required packages for Arch Linux:
### Arch Linux:
```sh
sudo pacman -S riscv64-linux-gnu-gcc qemu-user qemu-system-riscv qemu-tools
```

### Building source code in assembler and executing binaries
```sh
./fibonacci.exe > main.S &&
riscv64-linux-gnu-as main.S -o main.o &&
riscv64-linux-gnu-ld main.o &&
qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./a.out
```