# curly

An attempt at creating a toy ISA along with a """functional""" toolchain and some example programs.

## Usage

Run `zig build run-emulator` to run the emulator and `zig build run-assembler <source> <output>` to run the assembler.
Currently the emulator runs some hardcoded code and assembler produces a list of tokens.

You can currently use my [Comp](https://github.com/czapek1337/comp) fork to write high level code that compiles into runnable Curly code.
