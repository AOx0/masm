# masm

A simple assembler for x86_64 assembly that generates relocatable ELF files.

## Install

### Nightly Rust

To install/compile the assembler you need to have rust installed. You can install rust from [here](https://www.rust-lang.org/tools/install).
The project uses the nightly toolchain, so you may need to install it with:

```bash
rustup toolchain install nightly
```

### Install with Cargo

Clone the repository and run
```bash
cargo install --path ./path/to/masm
```

Or directly install it from git:
```bash
cargo install --git https://github.com/AOx0/masm
```

## Usage 

To compile an assembly file run:

```bash
masm -p <input> [-o <output>]
```

This generates a relocatable ELF file. To link (on linux) it run:

```bash
ld -o main <output>
```

As with any linkable file, you may need to specify the linker library paths and necessary arguments to link external symbols.

## Example

With the assembly file `main.s`:

```asm
end:
    mov  rax, 0x3c  ;; sys_exit
    mov  rdi, 10    ;; arg0 = 10
    syscall         ;; call

main:
    call end        ;; call end
```

Here the `main` function is the entrypoint, the entrypoint routine should always be named main or _start

We compile the file with:

```bash
masm -p main.s -o main.o
```

And link it with:

```bash
ld -o main main.o
```

Now we can run the program with:

```bash
./main
```

As expected,it exits with the status code 10