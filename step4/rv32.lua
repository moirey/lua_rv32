local bit = require'bit'
local ffi = require'ffi'

local DRAM_BASE = 0x80000000
local DRAM_SIZE = 1024*1024
local band, rshift, lshift = bit.band, bit.rshift, bit.lshift
local bor, tohex, bnot = bit.bor, bit.tohex, bit.bnot

local function DBG(str)
-- io.write(str)
end

local function dram_load(dram, addr, size)
	local load_switch = {
		[8] = function(dram, addr) --print("addr:"..addr..",mem:"..dram.mem[addr]) 
      return dram.mem[addr] 
    end,
		[16] = function(dram, addr) return bor(dram.mem[addr], lshift(dram.mem[addr+1],8)) end,	
		[32] = function(dram, addr) return  bor(dram.mem[addr], 
                                          lshift(dram.mem[addr+1],8),
			                                    lshift(dram.mem[addr+2],16), 
                                          lshift(dram.mem[addr+3],24)) end,
		[64] = function(dram, addr) print("can't support 64 bit") end

	}
  local r_addr = ffi.new('uint32_t[1]',addr-DRAM_BASE)[0]

	local ret = load_switch[size](dram,r_addr)
  --print("dram_load: addr:"..tohex(addr-DRAM_BASE)..",val:"..ret.."/"..tohex(ret))
  return ret
end

local function dram_store(dram, addr, size, value)
  local store_switch = {
    [8] = function (dram, addr, value) dram.mem[addr] = band(value,0xff) end,
    [16] = function (dram, addr, value) 
            dram.mem[addr] = band(value, 0xff)
            dram.mem[addr+1] = band( rshift(value,8), 0xff)
          end,
    [32] = function (dram, addr, value) 
            dram.mem[addr] = band(value, 0xff)
            dram.mem[addr+1] = band( rshift(value,8), 0xff)
            dram.mem[addr+2] = band( rshift(value,16), 0xff)
            dram.mem[addr+3] = band( rshift(value,24), 0xff)
            --io.write("store("..tohex(addr).."):"..band(value,0xff).." "..band( rshift(value,8),0xff).." "..band( rshift(value,16),0xff).." "..band( rshift(value,24),0xff).."\n");
            --io.write("store dram:"..band(dram.mem[addr],0xff).." "..band( rshift(dram.mem[addr+1],8),0xff).." "..band( rshift(dram.mem[addr+2],16),0xff).." "..band( rshift(dram.mem[addr+3],24),0xff).."\n");
          end,
    [64] = function (dram, addr, value) print("can't support 64 bit") end
  }
  
  --DBG("dram_store: addr:"..bit.tohex(addr).."\n")
  local r_addr = ffi.new('uint32_t[1]',addr-DRAM_BASE)[0]
  store_switch[size](dram, r_addr, value)

end

local CLINT_BASE = 0x2000000
local CLINT_SIZE = 0x10000
local CLINT_MTIMECMP = (CLINT_BASE + 0x4000)
local CLINT_MTIME = (CLINT_BASE + 0xbff8)

local PLIC_BASE = 0xc000000
local PLIC_SIZE = 0x4000000
local PLIC_PENDING = (PLIC_BASE + 0x1000)
local PLIC_SENABLE = (PLIC_BASE + 0x2080)
local PLIC_SPRIORITY = (PLIC_BASE + 0x201000)
local PLIC_SCLAIM = (PLIC_BASE + 0x201004)

local UART_BASE = 0x10000000
local UART_SIZE = 0x100
local VIRTIO_BASE = 0x10001000
local VIRTIO_SIZE = 0x1000

local UART = {}
local UART_RHR = UART_BASE
local UART_THR = UART_BASE 
local UART_LCR = UART_BASE + 3
local UART_LSR = UART_BASE + 5
local UART_LSR_RX = 1
local UART_LSR_TX = 0x20
local UART_LSR_EMPTY = 0x40

local function uart_new(uart)
  uart.data = {}
  for i=0,UART_SIZE do
    UART.data[i] = 0
  end
  UART.data[UART_LSR - UART_BASE] = bor(UART.data[UART_LSR - UART_BASE], UART_LSR_TX, UART_LSR_EMPTY);
  --print("uart_new: LSR:"..uart.data[UART_LSR - UART_BASE]..",addr:"..UART_LSR-UART_BASE..",tx:"..UART_LSR_TX)
end


local CPU_MODE = { USER = 0, SUPERVISOR = 1, MACHINE = 3 }
local SUPERVISOR_REG = { SSTATUS = 0x100, SIE = 0x104, STVEC = 0x105, SEPC = 0x141, SCAUSE = 0x142, 
                      STVAL = 0x143, SIP = 0x144, SATP = 0x180}

local MACHINE_CSR = { MSTATUS = 0x300, MEDELEG = 0x302, MIDELEG = 0x303, MIE = 0x304, MTVEC = 0x305,               
                      MEPC = 0x341, MCAUSE = 0x342, MTVAL = 0x343, MIP = 0x344 }

-- bit position
local MIP = { SSIP = 1, MSIP = 3, STIP = 5, MTIP = 7, SEIP =  9, MEIP =  11 }

--  CSR define
local function cpu_load_csr(cpu, csr_addr)
  if ( csr_addr == SUPERVISOR_REG.SIE ) then
    print("cpu_load_csr: need more process this register...")
    os.exit();
  end
  return cpu.csrs[csr_addr]
end

local function cpu_store_csr(cpu, csr_addr, value)
  if ( csr_addr == SUPERVISOR_REG.SIE ) then
    print("cpu_store_csr: need more process this register...")
    os.exit();
  end
  cpu.csrs[csr_addr] = value
end


local ONE_SEC_CLOCK = 10000000
local real_clock = 0
local function accumulate_timer(cpu)
  local curr_clock = os.clock()
  cpu.bus.clint.mtime = cpu.bus.clint.mtime + (curr_clock - real_clock) * ONE_SEC_CLOCK
  real_clock = curr_clock
  if ( cpu.bus.clint.mtime > cpu.bus.clint.mtimecmp and cpu.bus.clint.timer_signal==1 ) then
    cpu_store_csr(cpu, MACHINE_CSR.MIP, bor(cpu_load_csr(cpu, MACHINE_CSR.MIP), lshift(1, MIP.MTIP)));
    cpu.bus.clint.timer_signal = 0
    --print("timer signal raise")
  end
  --print("mtime:"..cpu.bus.clint.mtime..",cmp:"..cpu.bus.clint.mtimecmp)

end

local function clint_load(clint, addr, size)
  if (size ~= 32) then print("clint_load size has to set 32"); return 0 end
  if ( addr == CLINT_MTIMECMP ) then
  --print("clint_load: mtimecmp addr:"..tohex(addr).."\n");
    return clint.mtimecmp
  elseif (addr == CLINT_MTIME) then
  --print("clint_load: mtime addr:"..tohex(addr).."\n");
    return clint.mtime
  else return 0 end

end

local function clint_store(clint, addr, size, value)
  if ( size ~= 32 ) then print("clint_store size has to set 32"); return end
  if ( addr == CLINT_MTIMECMP ) then
  --print("clint_store: timecmp addr:"..tohex(addr)..", value:"..value.."\n");
    clint.mtimecmp = value
    clint.timer_signal = 1
  elseif (addr == CLINT_MTIME) then
  --print("clint_store: time addr:"..tohex(addr)..", value:"..value.."\n");
    clint.mtime = value
  end
end

local function uart_is_interrupting(uart)
    local interrupting = uart.interrupting 
    uart.interrupting = false 
    return interrupting
end

local function uart_load(uart, addr, size)
  --print("uart_load, addr:"..tohex(addr))
  if (addr == UART_RHR) then
    uart.data[UART_LSR - UART_BASE] = band(uart.data[UART_LSR-UART_BASE], 0xfe);
  end
  return uart.data[addr-UART_BASE] 
end

local function bus_load(bus, addr, size)

  local addr = ffi.new('uint32_t[1]',addr)[0]
  --DBG("bus_load: addr:"..tohex(addr).."\n")

  if ( addr >= CLINT_BASE and addr < (CLINT_BASE + CLINT_SIZE)) then
    return clint_load(bus.clint, addr, size)
  elseif ( addr >= PLIC_BASE and addr < (PLIC_BASE+ PLIC_SIZE)) then
    print("can't process plic")
    os.exit()
 
  --      return plic_load(bus->plic, addr, size, result);
  elseif ( addr >= UART_BASE and addr < (UART_SIZE+UART_BASE)) then
    return uart_load(bus.uart, addr, size)
  elseif ( addr >= VIRTIO_BASE and addr < (VIRTIO_BASE+ VIRTIO_SIZE)) then 
    print("can't process virtio_load")
    os.exit()
 
 --       return virtio_load(bus->virtio, addr, size, result);
  elseif ( addr >= DRAM_BASE ) then
    return dram_load(bus.dram, addr, size)
  end

  print("bus_load: "..(addr>=DRAM_BASE).." can't deal with addr:"..tohex(addr)..",DRAM_BASE:"..tohex(DRAM_BASE));
  os.exit();
end

local function uart_store(uart, addr, size, value)
  if ( addr == UART_THR ) then
    io.write(string.char(value))
  else
    uart.data[addr - UART_BASE] = band(value , 0xff);

  end

end 

local function bus_store(bus, addr, size, value)
  local addr = ffi.new('uint32_t[1]',addr)[0]
  if ( addr >= CLINT_BASE and addr < (CLINT_BASE + CLINT_SIZE)) then
    clint_store(bus.clint, addr, size, value);
  elseif ( addr >= PLIC_BASE and addr < (PLIC_BASE+ PLIC_SIZE)) then
    print("can't process plic")
    os.exit()

  --      return plic_load(bus->plic, addr, size, result);
  elseif ( addr >= UART_BASE and addr < (UART_SIZE+UART_BASE)) then
    uart_store(bus.uart, addr, size, value)
  elseif ( addr > VIRTIO_BASE and addr < (VIRTIO_BASE+ VIRTIO_SIZE)) then
    print("can't process virtio_load")
    os.exit()

 --       return virtio_load(bus->virtio, addr, size, result);
  elseif (DRAM_BASE <= addr) then 
    dram_store(bus.dram, addr, size, value)
  else 
    print("bus_store: can't deal with addr:"..tohex(addr));
    os.exit()
  end
end

local function cpu_init(cpu)
  cpu.regs[0] = 0
  cpu.regs[2] = DRAM_BASE + DRAM_SIZE
  cpu.pc = DRAM_BASE
end

local function cpu_load(cpu, addr, size)
  return bus_load(cpu.bus, addr, size)
end

local function cpu_store(cpu, addr, size, value)
  bus_store(cpu.bus, addr, size, value)
end


local function cpu_update_paging(cpu, csr_addr)
  if (csr_addr ~= SUPERVISOR_REG.SATP) then return end
  print("cpu_update_paging: need more process this register...")
  os.exit();
end
-- end CSR define

local function cpu_fetch(cpu, addr, size)
  return bus_load(cpu.bus, cpu.pc, 32)
end

local I_TYPE = 0x13
local S_TYPE = 0x23
local L_TYPE = 3
local R_TYPE = 0x33
local ZICSR_TYPE = 0x73
local B_TYPE = 0x63
local LUI = 0x37
local JALR = 0x67
local JAL = 0x6f


local FUNC = {
  imm_I = 0, rd = 1, rs1 = 2, rs2 = 3, shamt = 4, imm_U = 5, csr = 6,
}

local SRLI = 0
local SRAI = 32

local I_func = {
  [FUNC.imm_I] = function(inst) 
                   local imm = ffi.new('int32_t[1]', band(inst, 0xfff00000))
                   --print( band(imm[0], 0xfff00000) )
                   local n1 = rshift(band(imm[0], 0xfff00000), 20)
                   if ( rshift(imm[0], 31) == 1 ) then local n2 = ffi.new('int32_t[1]', bit.bor(n1, 0xfffff000)); n1 = n2[0] end      
                   return n1
                 end,
  [FUNC.rd] = function(inst) return band(rshift(inst, 7), 0x1f) end,
  [FUNC.rs1] = function(inst) return band(rshift(inst, 15), 0x1f) end,
  [FUNC.rs2] = function(inst) return band(rshift(inst, 20), 0x1f) end,
  [FUNC.shamt] = function(inst) return band(rshift(inst, 20), 0x3f) end
}

local CSR_func = {                                                                                                                            
  [FUNC.csr] = function(inst) return rshift(band(inst, 0xfff00000), 20) end,
  [FUNC.rd] = function(inst) return band(rshift(inst, 7), 0x1f) end,
  [FUNC.rs1] = function(inst) return band(rshift(inst, 15), 0x1f) end,

  [FUNC.rs2] = function(inst) return band(rshift(inst, 20), 0x1f) end
}

local R_func = {
  [FUNC.rd] = function(inst) return band(rshift(inst, 7), 0x1f) end,
  [FUNC.rs1] = function(inst) return band(rshift(inst, 15), 0x1f) end,
  [FUNC.rs2] = function(inst) return band(rshift(inst, 20), 0x1f) end
}

local S_func = {
  [FUNC.imm_I] =  function(inst) 
                    local imm1 = ffi.new('int32_t[1]', band(inst, 0xfe000000))
                    local imm = bor(rshift(imm1[0],20), band(rshift(inst, 7),0x1f))
                    if ( rshift(imm1[0], 31) == 1 ) then local n2 = ffi.new('int32_t[1]', bor(imm, 0xfffff000)); imm = n2[0] end
                    return imm
                  end,
  [FUNC.rs1] = function(inst) return band(rshift(inst, 15), 0x1f) end,
  [FUNC.rs2] = function(inst) return band(rshift(inst, 20), 0x1f) end,
}

local B_func = {
  [FUNC.imm_I] =  function(inst)
                    local imm11 = band( rshift(inst, 7), 0x1 )
                    local imm1 =  band( rshift(inst, 8), 0xf )
                    local imm5 =  band( rshift(inst, 25), 0x3f )
                    local imm12 = band( rshift(inst, 31), 0x1 )

                    local imm = bor( lshift(imm1,1), lshift(imm5,5), lshift(imm11,11), lshift(imm12,12))
                    if ( imm12 == 1 ) then local n2 = ffi.new('int32_t[1]', bor(imm, 0xffffe000)); imm = n2[0] end
                    return imm
                  end,
  [FUNC.rs1] = function(inst) return band(rshift(inst, 15), 0x1f) end,
  [FUNC.rs2] = function(inst) return band(rshift(inst, 20), 0x1f) end,
}

local L_func = {
  [FUNC.imm_I] =  function(inst) 
                    local imm1 = ffi.new('int32_t[1]', band(inst, 0xfff00000))
                    local imm = rshift(imm1[0],20)
                    if ( rshift(imm1[0], 31) == 1 ) then local n2 = ffi.new('int32_t[1]', bit.bor(imm, 0xfffff000)); imm = n2[0] end
                    return imm
                    --return rshift(band(inst, 0xfff00000), 20) 
                  end,
  [FUNC.rd] = function(inst) return band(rshift(inst, 7), 0x1f) end,
  [FUNC.rs1] = function(inst) return band(rshift(inst, 15), 0x1f) end,
}

local OP_I = {
  ADDI = 0, SLLI = 1, SLTI = 2, SLTIU = 3, XORI = 4, SRI = 5 ,
  ORI = 6, ANDI = 7,
}
local exec_OP_I = {
  [OP_I.ADDI] = function(cpu, inst) 
                  DBG("ADDI rd:"..I_func[FUNC.rd](inst)..",rs1:"..I_func[FUNC.rs1](inst)..",imm:"..I_func[FUNC.imm_I](inst).."\n")
                  cpu.regs[ I_func[FUNC.rd](inst) ] = cpu.regs[ I_func[FUNC.rs1](inst) ] + I_func[FUNC.imm_I](inst) 
                end,
  [OP_I.SLLI] = function(cpu, inst)
                  DBG("SLLI rd:"..I_func[FUNC.rd](inst)..",rs1:"..I_func[FUNC.rs1](inst)..",shamt:"..I_func[FUNC.shamt](inst).."\n")
                  cpu.regs[ I_func[FUNC.rd](inst) ] = lshift(cpu.regs[ I_func[FUNC.rs1](inst) ], I_func[FUNC.shamt](inst))
                end,
  [OP_I.ORI] = function(cpu, inst) 
    DBG("ORI rd:"..I_func[FUNC.rd](inst)..",rs1:"..I_func[FUNC.rs1](inst)..",imm:"..I_func[FUNC.imm_I](inst).."\n")
    cpu.regs[ I_func[FUNC.rd](inst)] = bor(cpu.regs[ I_func[FUNC.rs1](inst)], I_func[FUNC.imm_I](inst) )
  end,

  [OP_I.XORI] = function(cpu, inst)
    DBG("XORI rd:"..I_func[FUNC.rd](inst)..",rs1:"..I_func[FUNC.rs1](inst)..",imm:"..I_func[FUNC.imm_I](inst).."\n")
    cpu.regs[ I_func[FUNC.rd](inst)] = bit.bxor(cpu.regs[ I_func[FUNC.rs1](inst)], I_func[FUNC.imm_I](inst) )
  end,

  [OP_I.ANDI] = function(cpu, inst) 
    DBG("ANDI rd:"..I_func[FUNC.rd](inst)..",rs1:"..I_func[FUNC.rs1](inst)..",imm:"..I_func[FUNC.imm_I](inst).."\n")
    cpu.regs[ I_func[FUNC.rd](inst)] = band(cpu.regs[ I_func[FUNC.rs1](inst)], I_func[FUNC.imm_I](inst) );
  end,
}

local OP_B = {
  BEQ = 0, BNE = 1, BGE = 5, BLTU = 6, BGEU = 7,
}

local exec_OP_B = {
  [OP_B.BEQ] = function(cpu, inst)
    DBG("BEQ: rs1:"..B_func[FUNC.rs1](inst)..",rs2:"..B_func[FUNC.rs2](inst)..",imm:"..B_func[FUNC.imm_I](inst).."\n")
    if ( cpu.regs[ B_func[FUNC.rs1](inst)] == cpu.regs[ B_func[FUNC.rs2](inst)] ) then
      cpu.pc = cpu.pc + B_func[FUNC.imm_I](inst) - 4
    end
  end,
  [OP_B.BNE] = function(cpu, inst)
    DBG("BNE: rs1:"..B_func[FUNC.rs1](inst)..",rs2:"..B_func[FUNC.rs2](inst)..",imm:"..B_func[FUNC.imm_I](inst).."\n")
    if ( cpu.regs[ B_func[FUNC.rs1](inst)] ~= cpu.regs[ B_func[FUNC.rs2](inst)] ) then
      cpu.pc = cpu.pc + B_func[FUNC.imm_I](inst) - 4
    end    
  end,

  [OP_B.BGE] = function(cpu, inst)
    DBG("BGE: rs1:"..B_func[FUNC.rs1](inst)..",rs2:"..B_func[FUNC.rs2](inst)..",imm:"..B_func[FUNC.imm_I](inst).."\n")
    if ( cpu.regs[ B_func[FUNC.rs1](inst)] >= cpu.regs[ B_func[FUNC.rs2](inst)] ) then
      cpu.pc = cpu.pc + B_func[FUNC.imm_I](inst) - 4
    end    

  end, 

  [OP_B.BLTU] = function(cpu, inst)
    DBG("BLTU: rs1:"..B_func[FUNC.rs1](inst)..",rs2:"..B_func[FUNC.rs2](inst)..",imm:"..B_func[FUNC.imm_I](inst).."\n")
    if ( cpu.regs[ B_func[FUNC.rs1](inst)] < cpu.regs[ B_func[FUNC.rs2](inst)] ) then
      cpu.pc = cpu.pc + B_func[FUNC.imm_I](inst) - 4
    end    

  end,

  [OP_B.BGEU] = function(cpu, inst)
    DBG("BGEU: rs1:"..B_func[FUNC.rs1](inst)..",rs2:"..B_func[FUNC.rs2](inst)..",imm:"..B_func[FUNC.imm_I](inst).."\n")
    if ( cpu.regs[ B_func[FUNC.rs1](inst)] >= cpu.regs[ B_func[FUNC.rs2](inst)] ) then
      cpu.pc = cpu.pc + B_func[FUNC.imm_I](inst) - 4
    end    

  end,



}

local OP_R = {
  ADD = 0, SLL = 1, SLT = 2, SLTU = 3, XOR = 4, SRL = 5, OR = 6, AND = 7, SUB = 8, SRA = 9,
}

local exec_OP_R = {
  [OP_R.ADD] = function(cpu, inst)
                  cpu.regs[ R_func[FUNC.rd](inst) ] = cpu.regs[ R_func[FUNC.rs1](inst) ] + cpu.regs[ R_func[FUNC.rs2](inst) ]
                end,
  [OP_R.SUB] = function(cpu, inst)
    cpu.regs[ R_func[FUNC.rd](inst) ] = cpu.regs[ R_func[FUNC.rs1](inst) ] - cpu.regs[ R_func[FUNC.rs2](inst) ]
  end,
 [OP_R.OR] = function(cpu, inst)
    cpu.regs[ R_func[FUNC.rd](inst) ] = bor(cpu.regs[ R_func[FUNC.rs1](inst) ] , cpu.regs[ R_func[FUNC.rs2](inst) ] )
  end,

  [OP_R.AND] = function(cpu, inst)
    cpu.regs[ R_func[FUNC.rd](inst) ] = band(cpu.regs[ R_func[FUNC.rs1](inst) ] , cpu.regs[ R_func[FUNC.rs2](inst) ] )
  end,
}

local OP_M = {
  MUL = 0, MULH = 1, MULHSU = 2, MULHU = 3, DIV = 4, DIVU = 5, REM = 6, REMU = 7,
}

local exec_OP_M = {
  [OP_M.MUL] = function(cpu, inst)
    cpu.regs[ R_func[FUNC.rd](inst) ] = cpu.regs[ R_func[FUNC.rs1](inst) ] * cpu.regs[ R_func[FUNC.rs2](inst) ]
  end,

  [OP_M.DIV] = function(cpu, inst)
    local divisor = ffi.new('int32_t[1]',cpu.regs[ R_func[FUNC.rs2](inst) ])[0]
    local dividend = ffi.new('int32_t[1]', cpu.regs[ R_func[FUNC.rs1](inst) ])[0]
    if divisor == 0 then cpu.regs[ R_func[FUNC.rd](inst) ] = 0 
    elseif divisor == -1 and dividend == -2147483648 then 
      cpu.regs[ R_func[FUNC.rd](inst) ] = cpu.regs[ R_func[FUNC.rs1](inst) ]
    else 
      cpu.regs[ R_func[FUNC.rd](inst) ] = ffi.new('int32_t[1]', dividend / divisor)[0]
    end
    DBG("DIV: "..dividend.." / "..divisor.." = "..cpu.regs[ R_func[FUNC.rd](inst) ].."\n")
  end,

  [OP_M.REM] = function(cpu, inst)
    local divisor = ffi.new('int32_t[1]',cpu.regs[ R_func[FUNC.rs2](inst) ])[0]
    local dividend = ffi.new('int32_t[1]', cpu.regs[ R_func[FUNC.rs1](inst) ])[0]
    if divisor == 0 then cpu.regs[ R_func[FUNC.rd](inst) ] = dividend 
    elseif divisor == -1 and dividend == -2147483648 then cpu.regs[ R_func[FUNC.rd](inst) ] = 0 
    else cpu.regs[ R_func[FUNC.rd](inst) ] = dividend % divisor end

    DBG("REM: "..dividend.." % "..divisor.." = "..cpu.regs[ R_func[FUNC.rd](inst) ].."\n")
   end,

}


local OP_S = {
  SB = 0, SH = 1, SW = 2,
}
local exec_OP_S = {
  [OP_S.SB] = function(cpu, inst)
     DBG("SB rs2:"..S_func[FUNC.rs2](inst)..",rs1:"..S_func[FUNC.rs1](inst)..",imm:"..S_func[FUNC.imm_I](inst).."\n")
     cpu_store(cpu, cpu.regs[ S_func[FUNC.rs1](inst) ] + S_func[FUNC.imm_I](inst), 8, cpu.regs[ S_func[FUNC.rs2](inst) ])
  end,

  [OP_S.SW] = function(cpu, inst) 
     DBG("SW rs2:"..S_func[FUNC.rs2](inst)..",rs1:"..S_func[FUNC.rs1](inst)..",imm:"..S_func[FUNC.imm_I](inst)..",addr:"..tohex(cpu.regs[ S_func[FUNC.rs1](inst) ] + S_func[FUNC.imm_I](inst))..",val:"..tohex(cpu.regs[ S_func[FUNC.rs2](inst) ]).."\n")
     cpu_store(cpu, cpu.regs[ S_func[FUNC.rs1](inst) ] + S_func[FUNC.imm_I](inst), 32, cpu.regs[ S_func[FUNC.rs2](inst) ]) 
  end,
}

local OP_L = {
  LB = 0, LH = 1, LW = 2, LBU = 4, LHU = 5,
}
local exec_OP_L = {
  [OP_L.LW] = function(cpu, inst) 
    DBG("LW rd:"..L_func[FUNC.rd](inst)..",rs1:"..L_func[FUNC.rs1](inst)..",imm:"..L_func[FUNC.imm_I](inst)..",addr:"..tohex(cpu.regs[ L_func[FUNC.rs1](inst) ] + L_func[FUNC.imm_I](inst)).."\n")
    cpu.regs[ L_func[FUNC.rd](inst) ] = ffi.new('int32_t[1]',cpu_load(cpu, cpu.regs[ L_func[FUNC.rs1](inst) ] + L_func[FUNC.imm_I](inst), 32 ))[0]
   -- DBG("LW rd:"..L_func[FUNC.rd](inst)..",rs1:"..L_func[FUNC.rs1](inst)..",imm:"..L_func[FUNC.imm_I](inst)..",addr:"..tohex(cpu.regs[ L_func[FUNC.rs1](inst) ] + L_func[FUNC.imm_I](inst))..",val:"..cpu.regs[L_func[FUNC.rd](inst)].."\n")
  end,

  [OP_L.LBU] = function(cpu, inst)
    DBG("LBU rd:"..L_func[FUNC.rd](inst)..",rs1:"..L_func[FUNC.rs1](inst)..",imm:"..L_func[FUNC.imm_I](inst)..",ret:"..cpu_load(cpu, cpu.regs[ L_func[FUNC.rs1](inst) ] + L_func[FUNC.imm_I](inst) ,8).."\n")
    cpu.regs[ L_func[FUNC.rd](inst) ] = ffi.new('uint8_t[1]', cpu_load(cpu, cpu.regs[ L_func[FUNC.rs1](inst) ] + L_func[FUNC.imm_I](inst) ,8))[0]
  end,
}

local OP_ZICSR = {
  ECALL = 0, CSRRW = 1, CSRRS = 2, CSRRC = 3, EBREAK = 8, MRET = 0x302,
}

local exec_OP_CSR = {
  [OP_ZICSR.MRET] = function(cpu, inst)
    cpu.pc = cpu_load_csr(cpu, MACHINE_CSR.MEPC);
    local mpp = band(rshift(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS), 11), 3)
    if mpp == 2 then cpu.mode = CPU_MODE.MACHINE 
    elseif mpp == 1 then cpu.mode = CPU_MODE.SUPERVISOR
    else cpu.mode = CPU_MODE.USER end
    
    cpu_store_csr(cpu, MACHINE_CSR.MSTATUS,
                 band(rshift(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS),7), 1) == 1
               and bor(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS), lshift(1, 3))
               or band(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS), bnot(lshift(1, 3))))
    cpu_store_csr(cpu, MACHINE_CSR.MSTATUS, bor(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS), lshift(1, 7)));
    cpu_store_csr(cpu, MACHINE_CSR.MSTATUS, band(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS),bnot(lshift(3, 11))))
  end,

  [OP_ZICSR.CSRRW] = function(cpu, inst)
    local csr_addr = CSR_func[FUNC.csr](inst)
    local t = cpu_load_csr(cpu, csr_addr)
    cpu_store_csr(cpu, csr_addr, cpu.regs[ CSR_func[FUNC.rs1](inst)])
    cpu.regs[ CSR_func[FUNC.rd](inst) ] = t
    cpu_update_paging(cpu, csr_addr)
  end,

  [OP_ZICSR.CSRRS] =  function(cpu, inst) 
                        local csr_addr = CSR_func[FUNC.csr](inst)
                        local t = cpu_load_csr(cpu, csr_addr);
                        DBG("CSRRS: csr_addr:"..csr_addr..",t:"..t.."\n")
                        cpu_store_csr(cpu, csr_addr, bor(t,cpu.regs[ CSR_func[FUNC.rs1](inst) ]) );
                        cpu.regs[ CSR_func[FUNC.rd](inst) ] = t;
                        cpu_update_paging(cpu, csr_addr);
                      end,
}

local function cpu_execute(cpu, inst)
  local opcode = band(inst, 0x7f)
  local funct3 = band( rshift(inst, 12), 0x7 )
  local funct7 = band( rshift(inst, 25), 0x7f )

  DBG("\ninst: [ "..tohex(inst).." ], opcode:"..bit.tohex(opcode)..",funct3:"..bit.tohex(funct3)..",funct7:"..bit.tohex(funct7).."\n")
  cpu.regs[0] = 0
  if ( opcode == I_TYPE ) then
    if ( funct3 == OP_I.SRI) then
      exec_OP_I[funct3+funct7](cpu, inst)
    else 
      exec_OP_I[funct3](cpu,inst)
    end
  elseif ( opcode == B_TYPE ) then
    exec_OP_B[funct3](cpu, inst)
  elseif ( opcode == S_TYPE ) then
    exec_OP_S[funct3](cpu,inst)

  elseif ( opcode == L_TYPE ) then
    exec_OP_L[funct3](cpu, inst)
  elseif ( opcode == R_TYPE ) then
    if funct7 == 1 then
      exec_OP_M[funct3](cpu, inst)
    elseif ( funct3 == 0 and funct7 == 32 ) then
      exec_OP_R[OP_R.SUB](cpu, inst)
    elseif ( funct3 == 5 and funct7 == 32 ) then
      exec_OP_R[OP_R.SRA](cpu, inst)
    else
      exec_OP_R[funct3](cpu, inst)
    end
  elseif ( opcode == ZICSR_TYPE ) then
    local imm = I_func[FUNC.imm_I](inst)
    if funct3 == 0 then
      if imm == 0x302 then
        exec_OP_CSR[OP_ZICSR.MRET](cpu, inst)
      end      
    else
      exec_OP_CSR[funct3](cpu, inst)
    end 
  elseif ( opcode == LUI ) then
    -- LUI lui
    DBG("lui: rd:.."..I_func[FUNC.rd](inst)..",imm:"..tohex(band(inst, 0xfffff000)).."\n")
    cpu.regs[ I_func[FUNC.rd](inst)] = band(inst, 0xfffff000);
  elseif ( opcode == 23 ) then
    -- AUIPC auipc
    local imm = ffi.new('int32_t[1]', band(inst, 0xfffff000) );
    DBG("auipc: imm:"..imm[0]..",rd:"..I_func[FUNC.rd](inst).."\n")
    cpu.regs[ I_func[FUNC.rd](inst)] = cpu.pc + imm[0] - 4;

  elseif ( opcode == JAL ) then
    -- JAL jal
    cpu.regs[ I_func[FUNC.rd](inst)] = cpu.pc;
    local imm12 = band(rshift(inst, 12), 0xff)
    local imm11 = band(rshift(inst, 20), 0x1)
    local imm1  = band(rshift(inst, 21), 0x3ff)
    local imm20 = band(rshift(inst, 31), 0x1)

    --DBG("jal: imm12:"..tohex(imm12)..",imm11:"..tohex(imm11)..",imm1:"..tohex(imm1)..",imm20:"..imm20.."\n")
    local imm = bor( lshift(imm1,1), lshift(imm11,11), lshift(imm12,12), lshift(imm20,20))
    if ( imm20 == 1 ) then local n2 = ffi.new('int32_t[1]', bor(imm, 0xfffe0000)); imm = n2[0] end          
    DBG("jal: imm:"..imm..",rd:"..I_func[FUNC.rd](inst).."\n")
    cpu.pc = cpu.pc + imm - 4;
  elseif ( opcode == JALR ) then
    -- JALR jalr
    local tpc = cpu.pc;
    local t = ffi.new('int32_t[1]', band(inst, 0xfff00000))
    local imm = rshift(t[0], 20)
    if ( rshift(t[0],31 ) == 1) then local n2 = ffi.new('uint32',bor( imm, 0xfffff000 )); imm = n2[0] end

    DBG("JALR: tpc:"..tohex(tpc)..",t:"..t[0]..",imm:"..imm..",rs1:"..I_func[ FUNC.rs1](inst)..",rd:"..I_func[FUNC.rd](inst).."\n")
    cpu.pc = band(cpu.regs[ I_func[ FUNC.rs1](inst)] + imm, 0xfffffffe)

    cpu.regs[ I_func[FUNC.rd](inst)] = tpc;
  else
    print("ERRO: can't process opcode:"..bit.tohex(opcode)..",funct3:"..bit.tohex(funct3)..",funct7:"..bit.tohex(funct7))
    os.exit()
  end
  

end

local function dump_mem(cpu)
  for i=0,DRAM_SIZE-1 do
    DBG(tohex(cpu.bus.dram.mem[i],2).." ")
  end 
  DBG("\n")

end

local function dump_registers(cpu)
  for i=0,15 do
    DBG(tohex(cpu.regs[i]).." ")
  end
  DBG("\n")

  for i=16,31 do
    DBG(tohex(cpu.regs[i]).." ")
  end
  DBG("\n")


  DBG("PC:"..tohex(cpu.pc).."\n")

  --dump_mem(cpu)
end

local INTER = { NONE = 0,  SUPERVISOR_SOFTWARE_INTERRUPT = 1, MACHINE_SOFTWARE_INTERRUPT = 3,
    SUPERVISOR_TIMER_INTERRUPT = 5, MACHINE_TIMER_INTERRUPT = 7, SUPERVISOR_EXTERNAL_INTERRUPT = 9,
    MACHINE_EXTERNAL_INTERRUPT = 11 }


local function cpu_check_interrupt(cpu)
  if cpu.mode == CPU_MODE.MACHINE then 
    local mstatus = cpu_load_csr(cpu, MACHINE_CSR.MSTATUS)
    if band( rshift(mstatus, 3), 1) == 0 then return INTER.NONE end
  end

  if (cpu.mode == CPU_MODE.SUPERVISOR and band( rshift(cpu_load_csr(cpu, SSTATUS), 1),  1) == 0) then return INTER.NONE end 

  while (true) do
    local irq
    if (uart_is_interrupting(cpu.bus.uart)) then irq = UART_IRQ 
--    elseif (virtio_is_interrupting(cpu->bus->virtio)) then
  --     bus_disk_access(cpu->bus);
    --   irq = VIRTIO_IRQ;
    else break end

    bus_store(cpu.bus, PLIC_SCLAIM, 32, irq);
    cpu_store_csr(cpu, MIP, bor(cpu_load_csr(cpu, MIP) , MIP_SEIP));
  end

  local pending = band(cpu_load_csr(cpu, MACHINE_CSR.MIE) , cpu_load_csr(cpu, MACHINE_CSR.MIP))
  if ( band(rshift(pending, MIP.MTIP),1) == 1) then
    cpu_store_csr(cpu, MACHINE_CSR.MIP, band(cpu_load_csr(cpu, MACHINE_CSR.MIP), bnot(lshift(1, MIP.MTIP))));
    return INTER.MACHINE_TIMER_INTERRUPT
  end

  if ( band( rshift(pending,MIP.STIP), 1) == 1) then 
    cpu_store_csr(cpu, MACHINE_CSR.MIP, band(cpu_load_csr(cpu, MACHINE_CSR.MIP), bit.bnot(lshift(1, MIP_STIP))));
    return INTER.SUPERVISOR_TIMER_INTERRUPT
  end 

  return INTER.NONE
end 

local function cpu_take_trap(cpu, intr)
  local exception_pc = cpu.pc - 4;
  local prev_mode = cpu.mode;

  local is_interrupt = (intr ~= INTER.NONE);
  --  uint64_t cause = e;
  local cause = 0
  if (is_interrupt) then 
    cause = bor(lshift( 1, 31), intr);

    if (prev_mode <= CPU_MODE.SUPERVISOR and
        (band( rshift(cpu_load_csr(cpu, MACHINE_CSR.MEDELEG), band(cause, bnot(lshift(1,31)))), 1) ~= 0)) then
        cpu.mode = CPU_MODE.SUPERVISOR;
        print("trap1")
        if (is_interrupt) then
          local vec = cpu_load_csr(cpu, SUPERVISOR_REG.STVEC)
          if vec == 1 then vec = (4 * cause) else vec =  0 end
          cpu.pc = band(cpu_load_csr(cpu, SUPERVISOR_REG.STVEC), bnot(1)) + vec;
        else 
          cpu.pc = band(cpu_load_csr(cpu, SUPERVISOR_REG.STVEC), bnot(1));
        end

        cpu_store_csr(cpu, SUPERVISOR_REG.SEPC, band(exception_pc, bnot(1)))
        cpu_store_csr(cpu, SUPERVISOR_REG.SCAUSE, cause)
        cpu_store_csr(cpu, SUPERVISOR_REG.STVAL, 0)

        local sstaus = band(rshift(cpu_load_csr(cpu, SUPERVISOR_REG.SSTATUS), 1), 1)
        if ( sstaus == 1 ) then 
          sstaus = bor (cpu_load_csr(cpu, SUPERVISOR_REG.SSTATUS) , lshift(1, 5))
        else
          sstaus = band(cpu_load_csr(cpu, SUPERVISOR_REG.SSTATUS), bnot(lshift(1,5)))
        end

        cpu_store_csr(cpu, SUPERVISOR_REG.SSTATUS,sstaus) 
        cpu_store_csr(cpu, SUPERVISOR_REG.SSTATUS, band(cpu_load_csr(cpu, SUPERVISOR_REG.SSTATUS), bnot(2)))

        if (prev_mode == CPU_MODE.USER) then 
          cpu_store_csr(cpu, SUPERVISOR_REG.SSTATUS, band(cpu_load_csr(cpu, SUPERVISOR_REG.SSTATUS), bnot(lshift(1 , 8))));
        else 
          cpu_store_csr(cpu, SUPERVISOR_REG.SSTATUS, bor(cpu_load_csr(cpu, SUPERVISOR_REG.SSTATUS), lshift(1 ,8)))
        end
    else 
      cpu.mode = CPU_MODE.MACHINE;

      if (is_interrupt) then
        local vec = band(cpu_load_csr(cpu, MACHINE_CSR.MTVEC), 1) == 1 and 4 * cause or 0;
        cpu.pc = band(cpu_load_csr(cpu, MACHINE_CSR.MTVEC), bnot(1)) + vec;                                                       
      else 
        cpu.pc = band(cpu_load_csr(cpu, MACHINE_CSR.MTVEC), bnot(1));
      end 

      cpu_store_csr(cpu, MACHINE_CSR.MEPC, band(exception_pc, bnot(1)));
      cpu_store_csr(cpu, MACHINE_CSR.MCAUSE, cause);
      cpu_store_csr(cpu, MACHINE_CSR.MTVAL, 0);
      cpu_store_csr(cpu, MACHINE_CSR.MSTATUS,
                      band(rshift(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS), 3), 1)==1
                          and bor(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS), lshift(1, 7))
                          or band(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS), bnot(lshift(1, 7))))
      cpu_store_csr(cpu, MACHINE_CSR.MSTATUS, band(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS), bnot(lshift(1, 3))))
      cpu_store_csr(cpu, MACHINE_CSR.MSTATUS, band(cpu_load_csr(cpu, MACHINE_CSR.MSTATUS), bnot(lshift(3, 11))))
    end 
  end    

end

local dram = {}
local mem = {}
dram.mem = mem

-- alloc 8k dram mem
--for i = 0, DRAM_SIZE do
 -- dram.mem[i] = 0
--end


local CLINT = {}
CLINT.mtime = 0
CLINT.mtimecmp = 0
CLINT.timer_signal = 0

local BUS = {}
BUS.dram = dram
BUS.uart = UART
BUS.clint = CLINT 

local CPU = {}
CPU.regs = {}
CPU.csrs = {}
CPU.mode = CPU_MODE.MACHINE

-- register x0 ~ x31
for i=0,31 do
  CPU.regs[i] = 0
end

for i=0,4095 do
  CPU.csrs[i] = 0
end

CPU.pc = 0
CPU.bus = BUS


cpu_init(CPU)
uart_new(UART)
dump_registers(CPU)

local file = io.open(arg[1],"rb")
local rom = file:read("*all")
file:close()

for i=0,#rom-1 do
  dram.mem[i] = rom:byte(i+1)
  --io.write(bit.tohex(dram.mem[i], 2).." ")
end

for i=#rom,DRAM_SIZE do
  dram.mem[i] = 0
end

local interr = 0
while (true) do
  local inst = cpu_fetch(CPU)
  CPU.pc = CPU.pc + 4
  cpu_execute(CPU,inst)
  interr = cpu_check_interrupt(CPU)
  if interr ~= INTER.NONE then 
    cpu_take_trap(CPU, interr)
  end

  if ( CPU.pc == 0 ) then break end
  accumulate_timer(CPU);
  dump_registers(CPU)
end

--dump_mem(CPU)
