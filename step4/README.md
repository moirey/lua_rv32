mini-riscv-os的[step4 - TimerInterrupt](https://github.com/cccriscv/mini-riscv-os/tree/master/04-TimerInterrupt)
與[step5 - Preemptive](https://github.com/cccriscv/mini-riscv-os/tree/master/05-Preemptive)

這一階段就是加了一個時間累計器，超過mtimecmp就會打一次interrupt出來，然後把timer flag關掉。

然後也參考[semu](https://github.com/jserv/semu)跟[rv32emu](https://github.com/sysprog21/rv32emu)加入check_interrupt跟take_trap的功能.

