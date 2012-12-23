--cpu_4004_tb.vhd --- 
--
--Filename: cpu_4004_tb.vhd
--Description: 
--Author: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Maintainer: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Created: Sat Dec 15 22:05:52 2012 (+0100)
--Version: 0.1
--Last-Updated: Sun Dec 23 15:26:45 2012 (+0100)
--          By: oddbjorn
--    Update #: 53
--URL: 
--Keywords: 
--Compatibility: 
--
--

--Commentary: 
--
--
--
--

--Change Log:
--
--
--
--This program is free software; you can redistribute it and/or
--modify it under the terms of the GNU General Public License as
--published by the Free Software Foundation; either version 3, or
--(at your option) any later version.
--
--This program is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--General Public License for more details.
--
--You should have received a copy of the GNU General Public License
--along with this program; see the file COPYING.  If not, write to
--the Free Software Foundation, Inc., 51 Franklin Street, Fifth
--Floor, Boston, MA 02110-1301, USA.
--
--

--Code:

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sim_pkg.all;
use work.cpu_400X_pkg.all;

-------------------------------------------------------------------------------

entity cpu_4004_tb is

end entity cpu_4004_tb;

-------------------------------------------------------------------------------

architecture test of cpu_4004_tb is

  constant ID_CPU : string := "cpu";

  -- component ports
  signal clk             : std_logic;
  signal reset_n         : std_logic;
  signal cpu_input  : cpu_4004_input_type;
  signal cpu_output : cpu_4004_output_type;

  -- clock
  constant clk_freq   : real := 66.0e6;
  constant clk_period : time := (1 sec) / clk_freq;

  --sim
  signal stop : boolean := false;

  ---- Used to load a new memory content
  --type memory_load_type is record
  --  memory : memory_type;
  --  load   : boolean;
  --end record memory_load_type;
  --signal memory_load : memory_load_type;

  signal memory_cpu : memory_type;
  signal memory_new : memory_type;
  signal memory_load : boolean;

  
begin  -- architecture test

  -- component instantiation
  DUT : entity work.cpu_4004
    port map (
      clk             => clk,
      reset_n         => reset_n,
      cpu_input  => cpu_input,
      cpu_output => cpu_output);

  -- purpose: Generate cloks
  driver_clock(clk, clk_period);

  -- purpose: Test sequencer
  p_test_seq : process is

    --purpose: Check passive outputs
    procedure check_passive (
      constant msg : in string := "Checking passive outputs";
      constant id  :    string := "reset") is
    begin  -- procedure check_passive
      report_log_assert(cpu_output.beep = '0', "Beep not passive after reset", ID_CPU, error);
      report_log_assert(cpu_output.digit_latch = '0', "Digit latch not passive after reset", ID_CPU, error);
      report_log_assert(cpu_output.led_fetch = '0', "LED fetch not passive after reset", ID_CPU, error);
      report_log_assert(cpu_output.led_dec = '0', "LED dec not passive after reset", ID_CPU, error);
      report_log_assert(cpu_output.led_exec = '0', "LED exec not passive after reset", ID_CPU, error);
      report_log_assert(cpu_output.mem_wr = '0', "Mem write not passive after reset", ID_CPU, error);
      report_log_assert(cpu_output.mem_rd = '0', "Mem read not passive after reset", ID_CPU, error);
      report_log_assert(cpu_output.halted = '1', "halt not active after reset", ID_CPU, error);
      report_log_assert(cpu_output.running = '0', "running not passive after reset", ID_CPU, error);
      report_log_assert(cpu_output.paused = '0', "paused not passive after reset", ID_CPU, error);
    end procedure check_passive;

    --purpose: Drive inputs passive
    procedure drive_passive is
    begin  -- procedure drive_passive
      report_log("Driveing inputs passive", ID_RST, info);
      cpu_input.run  <= '0';
      cpu_input.step <= '0';
    --cpu_input.mem_rdata <= 0;
    end procedure drive_passive;

    -- purpose: Load content into memory
    procedure load_memory (
      constant memory : in memory_type) is
    begin  -- procedure load_memory
      wait until rising_edge(clk);
      memory_new <= memory;
      memory_load   <= true;
      wait until rising_edge(clk);
      memory_load   <= false;
    end procedure load_memory;

    -- purpose: Stop the CPU
    procedure stop_cpu is
    begin  -- procedure stop_cpu
      cpu_input.stop <= '1';
      wait until cpu_output.halted = '1' for 10 * clk_period;
      report_log_assert(cpu_output.halted = '1', "Assertion of stop did not halt the CPU",
                        ID_CPU, error);
      cpu_input.stop <= '0';
    end procedure stop_cpu;

    -- purpose: Run the CPU
    procedure run_cpu is
    begin  -- procedure run_cpu
      cpu_input.run <= '1';
      wait until cpu_output.running = '1' for 10 * clk_period;
      report_log_assert(cpu_output.running = '1', "Assertion of run did not start the CPU",
                        ID_CPU, error);
      cpu_input.run <= '0';
    end procedure run_cpu;

    -- purpose: Test the halt instruction
    procedure test_halt is
      variable memory : memory_type;
    begin  -- procedure test_halt
      report_log("Testing the halt instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (HALT, INC_R0, DEC_R0, INC_R1,
                 DEC_R1, ADD_R0_R1, SUB_R0_R1, PRINT_R0,
                 JP_IF_R0_NZ, JP_IF_R0_N, LOAD_R0, LOAD_R1,
                 STORE_R0, STORE_R1, SWAP_R0_ADDR, SWAP_R1_ADDR);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      wait until cpu_output.halted = '1' for 5 * clk_period;
      report_log_assert(cpu_output.halted = '1', "HALT instructiod did not halt CPU", ID_CPU, error);
      report_log_assert(cpu_output.running = '0', "running asserted after CPU is halted", ID_CPU, error);
      report_log_assert(cpu_output.paused = '0', "paused asserted after CPU is halted", ID_CPU, error);

      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);
      
    end procedure test_halt;
    
  begin  -- process p_test_seq
    report_init("cpu_4004_sim.out");
    drive_passive;
    driver_reset(clk, reset_n, 1 us, '0', false, true);
    check_passive("Checking passive output", "reset");

    --One byte instructions
    test_halt;
    --test_r0_inc;
    --test_r0_dec;
    --test_r1_inc;
    --test_r1_dec;
    --test_add;
    --test_sub;
    --test_print;

    --Two byte instructions
    --test_jp_if_nz;
    --test_jp_if_z;
    --test_load_r0;
    --test_load_r1;
    --test_store_r0;
    --test_store_r1;
    --test_swap_r0;
    --test_swap_r1;

    wait for 1 us;
    sim_end;
  end process p_test_seq;

  -- purpose: Simulate the memory
  p_memory : process (clk, reset_n) is

  begin  -- process p_memory
    if reset_n = '0' then
      for n in addr_range loop
        memory_cpu(n) <= 0;
      end loop;  -- n
    elsif rising_edge(clk) then

      --CPU read
      if cpu_output.mem_rd = '1' then
        cpu_input.mem_rdata <= memory_cpu(cpu_output.mem_addr);
      end if;

      --CPU write
      if cpu_output.mem_wr = '1' then
        memory_cpu(cpu_output.mem_addr) <= cpu_output.mem_wdata;
      end if;

      --Load new content
      if memory_load then
        memory_cpu <= memory_new;
      end if;

    end if;
  end process p_memory;

end architecture test;

-------------------------------------------------------------------------------

configuration cpu_4004_tb_test_cfg of cpu_4004_tb is
  for test
  end for;
end cpu_4004_tb_test_cfg;

-------------------------------------------------------------------------------

--cpu_4004_tb.vhd ends here
