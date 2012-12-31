--cpu_4004_tb.vhd --- 
--
--Filename: cpu_4004_tb.vhd
--Description: 
--Author: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Maintainer: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Created: Sat Dec 15 22:05:52 2012 (+0100)
--Version: 0.1
--Last-Updated: Mon Dec 31 16:24:09 2012 (+0100)
--          By: oddbjorn
--    Update #: 154
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
use work.cpu_4004_pkg.all;

-------------------------------------------------------------------------------

entity cpu_4004_tb is

end entity cpu_4004_tb;

-------------------------------------------------------------------------------

architecture test of cpu_4004_tb is

  constant ID_CPU : string := "cpu";

  -- component ports
  signal clk        : std_logic;
  signal reset_n    : std_logic;
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

  signal memory_cpu  : memory_type;
  signal memory_new  : memory_type;
  signal memory_load : boolean;

  
begin  -- architecture test

  -- component instantiation
  DUT : entity work.cpu_4004
    port map (
      clk        => clk,
      reset_n    => reset_n,
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
      memory_new  <= memory;
      memory_load <= true;
      wait until rising_edge(clk);
      memory_load <= false;
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

    -- purpose: Test that the CPU is halted
    procedure test_halted is
    begin  -- procedure test_halted
      wait until cpu_output.halted = '1' for 10 * clk_period;
      report_log_assert(cpu_output.halted = '1', "HALT instructiod did not halt CPU", ID_CPU, error);
      report_log_assert(cpu_output.running = '0', "running asserted after CPU is halted", ID_CPU, error);
      report_log_assert(cpu_output.paused = '0', "paused asserted after CPU is halted", ID_CPU, error);
    end procedure test_halted;

    -- purpose: Test the halt instruction
    procedure test_halt is
      variable memory : memory_type;
    begin  -- procedure test_halt
      report_log("Testing the HALT instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (HALT, INC_R0, DEC_R0, INC_R1,
                 DEC_R1, ADD_R0_R1, SUB_R0_R1, PRINT_R0,
                 JP_IF_R0_NZ, JP_IF_R0_Z, LOAD_R0, LOAD_R1,
                 STORE_R0, STORE_R1, SWAP_R0_ADDR, SWAP_R1_ADDR);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);
    end procedure test_halt;

    -- purpose: Test the inc_r0 instruction
    procedure test_inc_r0 is
      variable memory : memory_type;
    begin  -- procedure test_inc_r0
      report_log("Testing the INC_R0 instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (15 => HALT, others => INC_R0);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      for n in 1 to 15 loop
        if not (cpu_output.led_fetch = '1') then
          wait until cpu_output.led_fetch = '1' for 5 * clk_period;
        end if;
        report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
        wait until cpu_output.led_dec = '1' for 5 * clk_period;
        report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
        wait until cpu_output.led_exec = '1' for 5 * clk_period;
        report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);
        if not (cpu_output.reg_r0 = n) then
          wait until cpu_output.reg_r0 = n for 5 * clk_period;
        end if;
        report_log_assert(cpu_output.reg_r0 = n, "Wrong R0 value, expected: " &
                          integer'image(n) & " got: " & integer'image(cpu_output.reg_r0), ID_CPU, error);
      end loop;  -- n      

      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);
    end procedure test_inc_r0;

    -- purpose: Test the dec_r0 instruction
    procedure test_dec_r0 is
      variable memory : memory_type;
    begin  -- procedure test_dec_r0
      report_log("Testing the DEC_R0 instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (15 => HALT, others => DEC_R0);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      for n in 15 downto 1 loop
        if not (cpu_output.led_fetch = '1') then
          wait until cpu_output.led_fetch = '1' for 5 * clk_period;
        end if;
        report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
        wait until cpu_output.led_dec = '1' for 5 * clk_period;
        report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
        wait until cpu_output.led_exec = '1' for 5 * clk_period;
        report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);
        if not (cpu_output.reg_r0 = n) then
          wait until cpu_output.reg_r0 = n for 5 * clk_period;
        end if;
        report_log_assert(cpu_output.reg_r0 = n, "Wrong R0 value, expected: " &
                          integer'image(n) & " got: " & integer'image(cpu_output.reg_r0), ID_CPU, error);
      end loop;  -- n      

      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);
    end procedure test_dec_r0;

    -- purpose: Test the inc_r1 instruction
    procedure test_inc_r1 is
      variable memory : memory_type;
    begin  -- procedure test_inc_r1
      report_log("Testing the INC_R1 instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (15 => HALT, others => INC_R1);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      for n in 1 to 15 loop
        if not (cpu_output.led_fetch = '1') then
          wait until cpu_output.led_fetch = '1' for 5 * clk_period;
        end if;
        report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
        wait until cpu_output.led_dec = '1' for 5 * clk_period;
        report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
        wait until cpu_output.led_exec = '1' for 5 * clk_period;
        report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);
        if not (cpu_output.reg_r1 = n) then
          wait until cpu_output.reg_r1 = n for 5 * clk_period;
        end if;
        report_log_assert(cpu_output.reg_r1 = n, "Wrong R1 value, expected: " &
                          integer'image(n) & " got: " & integer'image(cpu_output.reg_r1), ID_CPU, error);
      end loop;  -- n      

      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);
    end procedure test_inc_r1;

    -- purpose: Test the dec_r1 instruction
    procedure test_dec_r1 is
      variable memory : memory_type;
    begin  -- procedure test_dec_r1
      report_log("Testing the DEC_R1 instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (15 => HALT, others => DEC_R1);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      for n in 15 downto 1 loop
        if not (cpu_output.led_fetch = '1') then
          wait until cpu_output.led_fetch = '1' for 5 * clk_period;
        end if;
        report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
        wait until cpu_output.led_dec = '1' for 5 * clk_period;
        report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
        wait until cpu_output.led_exec = '1' for 5 * clk_period;
        report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);
        if not (cpu_output.reg_r1 = n) then
          wait until cpu_output.reg_r1 = n for 5 * clk_period;
        end if;
        report_log_assert(cpu_output.reg_r1 = n, "Wrong R1 value, expected: " &
                          integer'image(n) & " got: " & integer'image(cpu_output.reg_r1), ID_CPU, error);
      end loop;  -- n      

      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);
    end procedure test_dec_r1;

    -- purpose: Test the load_r0 instruction
    procedure test_load_r0 (
      constant value : in byte) is
      variable memory : memory_type;
    begin  -- procedure test_load_r0
      report_log("Testing the LOAD_R0 instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (0 => LOAD_R0, 1 => value, others => HALT);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      if not (cpu_output.led_fetch = '1') then
        wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);
      if not (cpu_output.reg_r0 = value) then
        wait until cpu_output.reg_r0 = value for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.reg_r0 = value, "Wrong R0 value, expected: " &
                        integer'image(value) & " got: " & integer'image(cpu_output.reg_r0), ID_CPU, error);

      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);

    end procedure test_load_r0;

    -- purpose: Test the load_r1 instruction
    procedure test_load_r1 (
      constant value : in byte) is
      variable memory : memory_type;
    begin  -- procedure test_load_r1
      report_log("Testing the LOAD_R1 instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (0 => LOAD_R1, 1 => value, others => HALT);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      if not (cpu_output.led_fetch = '1') then
        wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);
      if not (cpu_output.reg_r1 = value) then
        wait until cpu_output.reg_r1 = value for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.reg_r1 = value, "Wrong R1 value, expected: " &
                        integer'image(value) & " got: " & integer'image(cpu_output.reg_r1), ID_CPU, error);

      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);

    end procedure test_load_r1;

    -- purpose: Test the add_r0_r1 instruction
    procedure test_add_r0_r1 (
      constant a : in byte;
      constant b : in byte) is
      variable res    : byte;
      variable memory : memory_type;
    begin  -- procedure test_add_r0_r1
      report_log("Testing the ADD_R0_R1 instruction", ID_CPU, info);
      res := (a + b) mod byte_values;
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (0      => LOAD_R0, 1 => a, 2 => LOAD_R1, 3 => b, 4 => ADD_R0_R1,
                 others => HALT);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      --Wait for the two load instructions to finish
      wait until cpu_output.led_exec = '1' for 20 * clk_period;
      wait until cpu_output.led_exec = '1' for 20 * clk_period;

      --The instruction under test
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);
      if not (cpu_output.reg_r0 = res) then
        wait until cpu_output.reg_r0 = res for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.reg_r0 = res, "Wrong R0 value, expected: " &
                        integer'image(res) & " got: " & integer'image(cpu_output.reg_r0), ID_CPU, error);

      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);
    end procedure test_add_r0_r1;

    -- purpose: Test the sub_r0_r1 instruction
    procedure test_sub_r0_r1 (
      constant a : in byte;
      constant b : in byte) is
      variable res    : byte;
      variable memory : memory_type;
    begin  -- procedure test_sub_r0_r1
      report_log("Testing the SUB_R0_R1 instruction", ID_CPU, info);
      res := (a - b) mod byte_values;
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (0      => LOAD_R0, 1 => a, 2 => LOAD_R1, 3 => b, 4 => SUB_R0_R1,
                 others => HALT);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      --Wait for the two load instructions to finish
      wait until cpu_output.led_exec = '1' for 20 * clk_period;
      wait until cpu_output.led_exec = '1' for 20 * clk_period;

      --The instruction under test
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);
      if not (cpu_output.reg_r0 = res) then
        wait until cpu_output.reg_r0 = res for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.reg_r0 = res, "Wrong R0 value, expected: " &
                        integer'image(res) & " got: " & integer'image(cpu_output.reg_r0), ID_CPU, error);

      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);
    end procedure test_sub_r0_r1;

    -- purpose: Test the print_r0 instruction
    procedure test_print_r0 (
      constant value : in byte) is
      variable memory : memory_type;
    begin  -- procedure test_print_r0
      report_log("Testing the PRINT_R0 instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (0 => LOAD_R0, 1 => value, 2 => PRINT_R0, others => HALT);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      --Wait for the load instruction to finish
      wait until cpu_output.led_exec = '1' for 20 * clk_period;

      --The instruction under test
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);
      if not (cpu_output.digit_latch = '1') then
        wait until cpu_output.digit_latch = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.digit_latch = '1', "Digit latch not asserted", ID_CPU, error);
      report_log_assert(cpu_output.digit_value = value, "Wrong digit value, expected: " &
                        integer'image(value) & " got: " & integer'image(cpu_output.digit_value),
                        ID_CPU, error);

      test_halted;
      report_log_assert(memory_cpu = memory_new, "Memory content modified", ID_CPU, error);
    end procedure test_print_r0;

    -- purpose: Test the store_r0 instruction
    procedure test_store_r0 (
      constant value : in byte;
      constant addr  : in byte) is
      variable memory : memory_type;
    begin  -- procedure test_store_r0
      report_log("Testing the STORE_R0 instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (0 => LOAD_R0, 1 => value, 2 => STORE_R0, 3 => addr, others => HALT);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      --Wait for the load instruction to finish
      wait until cpu_output.led_exec = '1' for 20 * clk_period;

      --The instruction under test
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);

      if not (cpu_output.mem_wr = '1') then
        wait until cpu_output.mem_wr = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.mem_wr = '1', "mem_wr not asserted", ID_CPU, error);
      report_log_assert(cpu_output.mem_wdata = value, "Wrong mem_wdata value, expected: " &
                        integer'image(value) & " got: " & integer'image(cpu_output.mem_wdata),
                        ID_CPU, error);
      report_log_assert(cpu_output.mem_addr = addr, "Wrong mem_addr value, expected: " &
                        integer'image(addr) & " got: " & integer'image(cpu_output.mem_addr),
                        ID_CPU, error);

      test_halted;
      memory(addr) := value;
      report_log_assert(memory_cpu = memory, "Memory content incorrect", ID_CPU, error);
    end procedure test_store_r0;

    -- purpose: Test the store_r1 instruction
    procedure test_store_r1 (
      constant value : in byte;
      constant addr  : in byte) is
      variable memory : memory_type;
    begin  -- procedure test_store_r1
      report_log("Testing the STORE_R1 instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (0 => LOAD_R1, 1 => value, 2 => STORE_R1, 3 => addr, others => HALT);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      --Wait for the load instruction to finish
      wait until cpu_output.led_exec = '1' for 20 * clk_period;

      --The instruction under test
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);

      if not (cpu_output.mem_wr = '1') then
        wait until cpu_output.mem_wr = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.mem_wr = '1', "mem_wr not asserted", ID_CPU, error);
      report_log_assert(cpu_output.mem_wdata = value, "Wrong mem_wdata value, expected: " &
                        integer'image(value) & " got: " & integer'image(cpu_output.mem_wdata),
                        ID_CPU, error);
      report_log_assert(cpu_output.mem_addr = addr, "Wrong mem_addr value, expected: " &
                        integer'image(addr) & " got: " & integer'image(cpu_output.mem_addr),
                        ID_CPU, error);

      test_halted;
      memory(addr) := value;
      report_log_assert(memory_cpu = memory, "Memory content incorrect", ID_CPU, error);
    end procedure test_store_r1;

    -- purpose: Test the swap_r0_addr instruction
    procedure test_swap_r0_addr (
      constant value1 : in byte;
      constant value2 : in byte;
      constant addr   : in byte) is
      variable memory : memory_type;
    begin  -- procedure test_swap_r0_addr
      report_log("Testing the SWAP_R0_ADDR instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (0      => LOAD_R0, 1 => value1, 2 => SWAP_R0_ADDR, 3 => addr,
                 others => HALT);
      memory(addr) := value2;
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      --Wait for the load instruction to finish
      wait until cpu_output.led_exec = '1' for 20 * clk_period;

      --The instruction under test
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);

      if not (cpu_output.mem_rd = '1') then
        wait until cpu_output.mem_rd = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.mem_rd = '1', "mem_rd not asserted", ID_CPU, error);
      report_log_assert(cpu_output.mem_addr = addr, "Wrong mem_addr value, expected: " &
                        integer'image(addr) & " got: " & integer'image(cpu_output.mem_addr),
                        ID_CPU, error);

      if not (cpu_output.mem_wr = '1') then
        wait until cpu_output.mem_wr = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.mem_wr = '1', "mem_wr not asserted", ID_CPU, error);
      report_log_assert(cpu_output.mem_wdata = value1, "Wrong mem_wdata value, expected: " &
                        integer'image(value1) & " got: " & integer'image(cpu_output.mem_wdata),
                        ID_CPU, error);
      report_log_assert(cpu_output.mem_addr = addr, "Wrong mem_addr value, expected: " &
                        integer'image(addr) & " got: " & integer'image(cpu_output.mem_addr),
                        ID_CPU, error);

      if not (cpu_output.reg_r0 = value2) then
        wait until cpu_output.reg_r0 = value2 for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.reg_r0 = value2, "Wrong R0 value, expected: " &
                        integer'image(value2) & " got: " & integer'image(cpu_output.reg_r0),
                        ID_CPU, error);

      test_halted;
      memory(addr) := value1;
      report_log_assert(memory_cpu = memory, "Memory content incorrect", ID_CPU, error);
    end procedure test_swap_r0_addr;

    -- purpose: Test the swap_r1_addr instruction
    procedure test_swap_r1_addr (
      constant value1 : in byte;
      constant value2 : in byte;
      constant addr   : in byte) is
      variable memory : memory_type;
    begin  -- procedure test_swap_r1_addr
      report_log("Testing the SWAP_R1_ADDR instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      --Load test program
      memory := (0      => LOAD_R1, 1 => value1, 2 => SWAP_R1_ADDR, 3 => addr,
                 others => HALT);
      memory(addr) := value2;
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      --Wait for the load instruction to finish
      wait until cpu_output.led_exec = '1' for 20 * clk_period;

      --The instruction under test
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);

      if not (cpu_output.mem_rd = '1') then
        wait until cpu_output.mem_rd = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.mem_rd = '1', "mem_rd not asserted", ID_CPU, error);
      report_log_assert(cpu_output.mem_addr = addr, "Wrong mem_addr value, expected: " &
                        integer'image(addr) & " got: " & integer'image(cpu_output.mem_addr),
                        ID_CPU, error);

      if not (cpu_output.mem_wr = '1') then
        wait until cpu_output.mem_wr = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.mem_wr = '1', "mem_wr not asserted", ID_CPU, error);
      report_log_assert(cpu_output.mem_wdata = value1, "Wrong mem_wdata value, expected: " &
                        integer'image(value1) & " got: " & integer'image(cpu_output.mem_wdata),
                        ID_CPU, error);
      report_log_assert(cpu_output.mem_addr = addr, "Wrong mem_addr value, expected: " &
                        integer'image(addr) & " got: " & integer'image(cpu_output.mem_addr),
                        ID_CPU, error);

      if not (cpu_output.reg_r1 = value2) then
        wait until cpu_output.reg_r1 = value2 for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.reg_r1 = value2, "Wrong R1 value, expected: " &
                        integer'image(value2) & " got: " & integer'image(cpu_output.reg_r1),
                        ID_CPU, error);

      test_halted;
      memory(addr) := value1;
      report_log_assert(memory_cpu = memory, "Memory content incorrect", ID_CPU, error);
    end procedure test_swap_r1_addr;

    -- purpose: Test the jp_if_r0_nz instruction
    procedure test_jp_if_r0_nz (
      constant value : in byte) is
      variable memory : memory_type;
    begin  -- procedure test_jp_if_r0_nz
      report_log("Testing the JP_IF_R0_NZ instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      report_log_assert(value > 0, "The test value must be greater than 0", ID_CPU, error);
      
      --Load test program
      memory := (
        --Test zero
        0 => JP_IF_R0_NZ,
        1 => 6,

        --Test non zero
        2 => LOAD_R0,
        3 => value,
        4 => JP_IF_R0_NZ,
        5 => 12,

        --Print error
        6 => LOAD_R0,
        7 => 10,
        8 => PRINT_R0,

        --Print succsess
        12 => LOAD_R0,
        13 => 5,
        14 => PRINT_R0,

        others => HALT);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      --The instruction under test
      if not (cpu_output.led_fetch = '1') then
        wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);

      wait until cpu_output.digit_latch = '1' for 50 * clk_period;
      report_log_assert(cpu_output.digit_latch = '1', "digit_latch not asserted", ID_CPU, error);
      report_log_assert(cpu_output.digit_value = 5, "Wrong digit_value, expected: " &
                        integer'image(10) & " got: " & integer'image(cpu_output.digit_value),
                        ID_CPU, error);
      
      test_halted;
      report_log_assert(memory_cpu = memory, "Memory content incorrect", ID_CPU, error);
    end procedure test_jp_if_r0_nz;

    -- purpose: Test the jp_if_r0_z instruction
    procedure test_jp_if_r0_z (
      constant value : in byte) is
      variable memory : memory_type;
    begin  -- procedure test_jp_if_r0_z
      report_log("Testing the JP_IF_R0_Z instruction", ID_CPU, info);
      wait until rising_edge(clk);
      stop_cpu;

      report_log_assert(value > 0, "The test value must be greater than 0", ID_CPU, error);
      
      --Load test program
      memory := (
        --Test non zero
        0 => LOAD_R0,
        1 => value,
        2 => JP_IF_R0_Z,
        3 => 8,

        --Test non zero
        4 => LOAD_R0,
        5 => 0,
        6 => JP_IF_R0_Z,
        7 => 12,

        --Print error
        8 => LOAD_R0,
        9 => 5,
        10 => PRINT_R0,

        --Print succsess
        12 => LOAD_R0,
        13 => 10,
        14 => PRINT_R0,

        others => HALT);
      load_memory(memory);

      --Run the CPU
      run_cpu;

      --Test the response
      --Wait for the load instruction to finish
      wait until cpu_output.led_exec = '1' for 20 * clk_period;

      --The instruction under test
      if not (cpu_output.led_fetch = '1') then
        wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      end if;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_fetch = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_fetch = '1', "Fetch not asserted", ID_CPU, error);
      wait until cpu_output.led_dec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_dec = '1', "Decode not asserted", ID_CPU, error);
      wait until cpu_output.led_exec = '1' for 5 * clk_period;
      report_log_assert(cpu_output.led_exec = '1', "Exec not asserted", ID_CPU, error);

      wait until cpu_output.digit_latch = '1' for 50 * clk_period;
      report_log_assert(cpu_output.digit_latch = '1', "digit_latch not asserted", ID_CPU, error);
      report_log_assert(cpu_output.digit_value = 10, "Wrong digit_value, expected: " &
                        integer'image(10) & " got: " & integer'image(cpu_output.digit_value),
                        ID_CPU, error);
      
      test_halted;
      report_log_assert(memory_cpu = memory, "Memory content incorrect", ID_CPU, error);
    end procedure test_jp_if_r0_z;

  begin  -- process p_test_seq
    report_init("cpu_4004_sim.out");
    drive_passive;
    driver_reset(clk, reset_n, 1 us, '0', false, true);
    check_passive("Checking passive output", "reset");

    --One byte instructions
    test_halt;
    test_inc_r0;
    test_dec_r0;
    test_inc_r1;
    test_dec_r1;
    test_add_r0_r1(5, 13);
    test_add_r0_r1(4, 7);
    test_sub_r0_r1(13, 7);
    test_sub_r0_r1(2, 11);
    test_print_r0(2);
    test_print_r0(15);

    --Two byte instructions
    test_jp_if_r0_nz(1);
    test_jp_if_r0_nz(15);
    test_jp_if_r0_z(1);
    test_jp_if_r0_z(15);
    test_load_r0(5);
    test_load_r0(10);
    test_load_r1(12);
    test_load_r1(3);
    test_store_r0(3, 14);
    test_store_r0(13, 2);
    test_store_r1(15, 15);
    test_store_r1(1, 3);

    test_swap_r0_addr(3, 14, 15);
    test_swap_r0_addr(12, 7, 6);
    test_swap_r1_addr(5, 10, 13);
    test_swap_r1_addr(12, 3, 5);

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
