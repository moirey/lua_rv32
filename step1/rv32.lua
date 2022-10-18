local bit = require'bit'
local ffi = require'ffi'

local DRAM_BASE = 0x80000000
local DRAM_SIZE = 1*1024
local band, rshift, lshift = bit.band, bit.rshift, bit.lshift
local bor, tohex = bit.bor, bit.tohex

local function dram_load(dram, addr, size)
	local load_switch = {
		[8] = function(dram, addr) return dram.mem[addr] end,
		[16] = function(dram, addr) return bor(dram.mem[addr], lshift(dram.mem[addr+1],8)) end,	
		[32] = function(dram, addr) return  bor(dram.mem[addr], 
                                          lshift(dram.mem[addr+1],8),
			                                    lshift(dram.mem[addr+2],16), 
                                          lshift(dram.mem[addr+3],24)) end,
		[64] = function(dram, addr) print("can't support 64 bit") end

	}
  --print("dram_load: addr:"..bit.tohex(addr-DRAM_BASE))

	return load_switch[size](dram,addr-DRAM_BASE)
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
            --io.write("store("..addr.."):"..band(value,0xff).." "..band( rshift(value,8),0xff).." "..band( rshift(value,16),0xff).." "..band( rshift(value,24),0xff).."\n");
            --io.write("store dram:"..band(dram.mem[addr],0xff).." "..band( rshift(dram.mem[addr+1],8),0xff).." "..band( rshift(dram.mem[addr+2],16),0xff).." "..band( rshift(dram.mem[addr+3],24),0xff).."\n");
          end,
    [64] = function (dram, addr, value) print("can't support 64 bit") end
  }
  
  store_switch[size](dram, addr-DRAM_BASE, value)

end

local function bus_load(bus, addr, size)
  return dram_load(bus.dram, addr, size)
end

local function bus_store(bus, addr, size, value)
  dram_store(bus.dram, addr, size, value)
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

local function cpu_fetch(cpu, addr, size)
  return bus_load(cpu.bus, cpu.pc, 32)
end

local I_TYPE = 19
local S_TYPE = 35
local L_TYPE = 3
local R_TYPE = 51

local OP_I = {
  ADDI = 0, SLLI = 1, SLTI = 2, SLTIU = 3, XORI = 4, SRI = 5 ,
  ORI = 6, ANDI = 7,
}

local OP_S = {
  SB = 0, SH = 1, SW = 2,
}

local OP_L = {
  LB = 0, LH = 1, LW = 2, 
}

local OP_R = {
  ADD = 0, SLL = 1,
}

local FUNC = {
  imm_I = 0, rd = 1, rs1 = 2, rs2 = 3
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
     --               local imm = ffi.new('int32_t[1]', bit.bor( band(rshift(inst, 7),0x1f), rshift(band(inst, 0xfe0000), 0x7f), 20))
                    if ( rshift(imm1[0], 31) == 1 ) then local n2 = ffi.new('int32_t[1]', bit.bor(imm, 0xfffff000)); imm = n2[0] end
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


local exec_OP_I = {
  [OP_I.ADDI] = function(cpu, inst) 
                  print("ADDI rd:"..I_func[FUNC.rd](inst)..",rs1:"..I_func[FUNC.rs1](inst)..",imm:"..I_func[FUNC.imm_I](inst))
                  cpu.regs[ I_func[FUNC.rd](inst) ] = cpu.regs[ I_func[FUNC.rs1](inst) ] + I_func[FUNC.imm_I](inst) 
                end
}

local exec_OP_R = {
  [OP_R.ADD] = function(cpu, inst)
                  print("ADD rd:"..R_func[FUNC.rd](inst)..",rs1:"..R_func[FUNC.rs1](inst)..",rs2:"..R_func[FUNC.rs2](inst))
                  cpu.regs[ R_func[FUNC.rd](inst) ] = cpu.regs[ R_func[FUNC.rs1](inst) ] + cpu.regs[ R_func[FUNC.rs2](inst) ]
                end
}

local exec_OP_S = {
  [OP_S.SW] = function(cpu, inst) 
                print("SW rs2:"..S_func[FUNC.rs2](inst)..",rs1:"..S_func[FUNC.rs1](inst)..",imm:"..S_func[FUNC.imm_I](inst))
                cpu_store(cpu, cpu.regs[ S_func[FUNC.rs1](inst) ] + S_func[FUNC.imm_I](inst), 32, cpu.regs[ S_func[FUNC.rs2](inst) ]) 
              end
}

local exec_OP_L = {
  [OP_L.LW] = function(cpu, inst) 
                print("LW rd:"..L_func[FUNC.rd](inst)..",rs1:"..L_func[FUNC.rs1](inst)..",imm:"..L_func[FUNC.imm_I](inst))
                cpu.regs[ L_func[FUNC.rd](inst) ] = cpu_load(cpu, cpu.regs[ L_func[FUNC.rs1](inst) ] + L_func[FUNC.imm_I](inst), 32 ) 
              end
}

local function cpu_execute(cpu, inst)
  print("inst:"..tohex(inst));
  local opcode = band(inst, 0x7f)
  local funct3 = band( rshift(inst, 12), 0x7 )
  local funct7 = band( rshift(inst, 25), 0x7f )

  cpu.regs[0] = 0
  if ( opcode == I_TYPE ) then
    if ( funct3 == OP_I.SRI) then
      exec_OP_I[funct3+funct7](cpu, inst)
    else 
      exec_OP_I[funct3](cpu,inst)
    end
  
  elseif ( opcode == S_TYPE ) then
    exec_OP_S[funct3](cpu,inst)

  elseif ( opcode == L_TYPE ) then
    exec_OP_L[funct3](cpu, inst)
  elseif ( opcode == R_TYPE ) then
    exec_OP_R[funct3](cpu, inst)
  elseif ( opcode == 103 ) then
    -- JALR jalr
    local tpc = cpu.pc;
    local t = ffi.new('int32_t[1]', band(inst, 0xfff00000))
    local imm = rshift(t[0], 20)
    if ( rshift(t[0],31 ) == 1) then local n2 = ffi.new('uint32',bor( imm, 0xfffff000 )); imm = n2[0] end
    
    cpu.pc = band(cpu.regs[ I_func[ FUNC.rs1](inst)] + imm, 0xfffffffe)

    cpu.regs[ I_func[FUNC.rd](inst)] = tpc;
  else
    print("ERRO: can't process opcode:"..bit.tohex(opcode)..",funct3:"..bit.tohex(funct3)..",funct7:"..bit.tohex(funct7))
    os.exit()
  end
  

end

local function dump_mem(cpu)
  for i=0,DRAM_SIZE-1 do
    io.write(tohex(cpu.bus.dram.mem[i],2).." ")
  end 
  io.write("\n")

end

local function dump_registers(cpu)
  for i=0,15 do
    io.write(tohex(cpu.regs[i]).." ")
  end
  io.write("\n")

  for i=16,31 do
    io.write(tohex(cpu.regs[i]).." ")
  end
  io.write("\n")


  io.write("PC:"..tohex(cpu.pc).."\n")

  --dump_mem(cpu)
end


local dram = {}
local mem = {}
dram.mem = mem

-- alloc 8k dram mem
for i = 0, DRAM_SIZE do
  dram.mem[i] = 0
end

--print(dram_load(dram,DRAM_BASE,8))
--print(dram_load(dram,DRAM_BASE,16))
--print(dram_load(dram,DRAM_BASE,32))
--print(dram_load(dram,DRAM_BASE,64))
--

local BUS = {}
BUS.dram = dram

local CPU = {}
CPU.regs = {}
-- register x0 ~ x31
for i=0,31 do
  CPU.regs[i] = 0
end

CPU.pc = 0
CPU.bus = BUS


cpu_init(CPU)
dump_registers(CPU)

local file = io.open(arg[1],"rb")
local rom = file:read("*all")
file:close()

for i=0,#rom-1 do
  dram.mem[i] = rom:byte(i+1)
  --io.write(bit.tohex(dram.mem[i], 2).." ")
end

while (true) do
  local inst = cpu_fetch(CPU)
  CPU.pc = CPU.pc + 4
  cpu_execute(CPU,inst)

  if ( CPU.pc == 0 ) then break end
  dump_registers(CPU)
end

dump_mem(CPU)
