mini-riscv-os的step3 - switch context卡了一陣子，
後來發現是rv32 M extention沒有實作, 補上後就可以跑了。

另外一個重點是lua跑起來比較慢，所以他的lib_delay內建是用50000 * time, 
我把它降到500才可以跑得起來。

