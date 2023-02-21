# shic
SIC assembler & emulator, debugger developed in Haskell.

## How to install

Install [haskell stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Clone this project.
```sh
$ git clone https://github.com/bivoje/shic.git
```

`cd` into the project root.
```sh
$ cd shic
```

Then run
```
$ stack install
```

Now 3 executables are copied to your local environment. The exact path can be known by
```sh
$ stack path --local-bin
```

You might want to add it to your `$PATH`. In linux, run
```
echo 'PATH=$PATH:'`stack path --local-bin` >> ~/.bashrc
```

## Executables

### shic-asm
```sh
$ sic-asm <src.asm> <dst.obj>
```

Assembler for SIC assembly code. Reads file `<src.asm>`, then creates a objective file named `<dst.obj>`.


### shic-emul

```sh
$ shic-emul <src.obj> <memsize> [<device file>]
```

SIC machine emulator. 

`<src.obj>` is a file containing objective code of assemble SIC source.

`<memsize>` is size of the memory in bytes for the machine. Must be decimal integer form without sign.

`<device file>` is optional, a file that specifies available devices.
The file should contain a number of lines of form
```
(<device id>, Device (<in-device type>) (<out-device type>))
```
`<device id>` is a byte that is used to refer to the device.

`<in-device type>` is one of the followings
- `FailIn`: Always fail, results in 0 when read
- `NullIn`: Always success, results in 0 when read
- `ForwardIn`: Forward stdin to this device.
- `Tucked <list of bytes>`: Provide bytes from given list, starting from head.
`<list of bytes>` should be in form `[b1, b2, b3 ... ]` .

`<out-device type>` is one of the followings
- `FailOut`: Always fail, nothing happens when written to
- `NullOut`: Always success, nothing happens when written to
- `ForwardOut`: Forward written bytes to stdout.
- `Dump <device name>`: Report written bytes to stdout in hexadecimal form with a `device name`.
`<device name>` should be enclosed in double quotes.

Devices for unspecified device id defaults to `Device (FailIn) (FailOut)`.

If the `<device file>` is not specified, `Device (ForwardIn) (Dump "dev<i>")` is mapped to device 0 to 10.

Actually, each line of `<device file>` is parsed by Haskell's `readMaybe` function for type `(Byte, Device DevIn DevOut)`, so redundant whitespaces or parenthesis' are ok to added/removed.

### shic-dbg

```sh
$ shic-gdb <src.obj> <memsize> [<device file>]
```
Almost identical to `shic-emul` except that when you run it, it stops before the programs starting point and you get cpu/memory state dump, a command prompt. (like `gdb` does).

```
Registers =======
  a:  000000	x:  000000	l: 000000
  pc: 000114	sw: 000000 (EQ)
Memory ==========
            0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
  000110 | 00 03 03 06 3C 01 17 E0 01 12 30 01 68 04 01 09 
Instruction =====
  (000114) 3C0117 =  J   0117
cmd:
``` 

Supported commands are
- empty line: Does nothing and prompts again.
- `quit`: Exits the debugger
- `n <number>`: Run `<number>` instructions. `<number>` should be greater than 0.
- `b [<address>]`: Run until program counter (`pc`) becomes `<address>`. If address is omitted, it defaults to current `pc`. (There is a known bug. see [TODO list](#todo))
- `w <reg> <==>/<!=> <number>`: Run until given register gets the value equal to `<number>`. `<reg>` is one of `a`, `x`, `l`, `pc`, `sw`. `<number>` can be signed/unsigned hex/dec format. Specifying `pc` register is same as using `b [<address>]` command.
- `mem <address>` or `mem +<address>`: Adds memory view points. memory segments containing `<address>` would be dumped to screen when the debugger stops execution and prompt to user.
- `mem -<address>`: Remove memory viewpoints set by `mem +<address>`.


## Demo

`test/sample4.asm` contains a SIC program that calculates number of 1 bits from given number. Let's assemble and run it.

After building the project, in project's root directory, run
```sh
$ shic-asm test/sample4.asm sample4.obj
```
It will assemble SIC source and create objective file `sample4.obj`.

`test/sample4_devs` contains device specifications for this program.
```sh
$ cat test/sample4_devs
(3, Device (Tucked [0x72, 0xA3, 0x97]) NullOut)
(6, Device NullIn (Dump "outputdev"))
```

Bytes `0x72`, `0xA3`, `0x97` will be written to device 3.
The program would be counting 1's in `0x72A397` (SIC uses big-endian).
And outputs result (13) to device 6, which is labeled as `"outputdev"`.

We can use this file with `shic-emul`.
```sh
$ shic-emul sample4.obj 1000 test/sample4_devs
```
It prints

```
Device dev: Byte 0x0 '' is written
Device dev: Byte 0x0 '' is written
Device dev: Byte 0xd '' is written
```
as expected.


## <a name="todo"></a> TODO list

- In debugger, setting breakpoint at current position does nothing and stops right there.
- Enable multiple breakpoints in debugger.
- Enable overwriting values in memory/registers in debugger.
- Enable assemble/disassemble code in debugger.

