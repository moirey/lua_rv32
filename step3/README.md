mini-riscv-os的[step3 - switch context](https://github.com/cccriscv/mini-riscv-os/tree/master/03-MultiTasking)卡了一陣子，
後來發現是rv32 M extention沒有實作, 補上後就可以跑了。

另外一個重點是lua跑起來比較慢，所以他的[lib_delay](https://github.com/cccriscv/mini-riscv-os/blob/master/03-MultiTasking/lib.c)內建是用50000 * time, 
我把它降到500才可以跑得起來。

