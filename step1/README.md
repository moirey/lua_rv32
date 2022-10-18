用luajit寫了一個riscv32的emulator

在step1主要是驗證可以跑riscv32 的binary code, 可以利用下面的指令編譯出一個簡單的binary檔案進行驗證

../riscv/bin/riscv64-unknown-elf-gcc -nostdlib -march=rv32i -mabi=ilp32 --entry main add.c -o add
../riscv/bin/riscv64-unknown-elf-objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.text.init --section=.data add > add.dump
../riscv/bin/riscv64-unknown-elf-objcopy -O binary add add.bin

luajit rv32.lua os.bin

