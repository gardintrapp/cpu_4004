--cpu_4004.vhd --- 
--
--Filename: cpu_4004.vhd
--Description: 
--Author: Oddbjørn Norstrand <gardintrapp@gmail.com
--Maintainer: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Created: Sat Dec 15 21:38:10 2012 (+0100)
--Version: 0.1
--Last-Updated: Mon Dec 31 16:24:20 2012 (+0100)
--          By: oddbjorn
--    Update #: 132
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

entity cpu_4004 is

  generic (
    READ_LATENCY : natural := 2);

  port (
    clk        : in  std_logic;
    reset_n    : in  std_logic;
    cpu_input  : in  cpu_4004_input_type;
    cpu_output : out cpu_4004_output_type);

end entity cpu_4004;

architecture twoproc of cpu_4004 is

  type state_type is (halted, fetch_addr, fetch_read, decode, decode_two,
                      ex_halt, ex_inc_r0, ex_dec_r0, ex_inc_r1, ex_dec_r1,
                      ex_add_r0_r1, ex_sub_r0_r1, ex_print_r0, ex_jp_if_r0_nz,
                      ex_jp_if_r0_z, ex_load_r0, ex_load_r1, ex_store_r0,
                      ex_store_r1, ex_swap_r0_addr_0, ex_swap_r0_addr_1,
                      ex_swap_r1_addr_0, ex_swap_r1_addr_1, paused);
  type reg_type is record
    cpu_input       : cpu_4004_input_type;
    cpu_output      : cpu_4004_output_type;
    state           : state_type;
    cnt             : byte;
    second_ins_byte : boolean;
    data_not_ins    : boolean;
    data            : byte;
    addr            : byte;
  end record reg_type;
  signal r, rin : reg_type;
  
begin  -- architecture twoproc

  -- purpose: Main logic
  -- type   : combinational
  p_comb : process (cpu_input, r) is
    variable v : reg_type;
  begin  -- process p_comb
    v := r;

    --Read inputs
    v.cpu_input := cpu_input;

    --Default values
    v.cpu_output.beep        := '0';
    v.cpu_output.digit_latch := '0';
    v.cpu_output.led_fetch   := '0';
    v.cpu_output.led_dec     := '0';
    v.cpu_output.led_exec    := '0';
    v.cpu_output.mem_wr      := '0';
    v.cpu_output.mem_rd      := '0';
    v.cpu_output.halted      := '0';
    v.cpu_output.running     := '1';
    v.cpu_output.paused      := '0';

    --The CPU state machine
    case r.state is
      when halted =>
        --Wait for the CPU to be started 
        v.cpu_output.halted      := '1';
        v.cpu_output.running     := '0';
        v.cpu_output.paused      := '0';
        v.second_ins_byte        := false;
        v.data_not_ins           := false;
        v.addr                   := 0;
        v.data                   := 0;
        v.cpu_output.digit_value := 0;
        v.cpu_output.mem_wdata   := 0;
        v.cpu_output.mem_addr    := 0;
        if r.cpu_input.run = '1' then
          v.state             := fetch_addr;
          v.cpu_output.reg_ip := 0;
          v.cpu_output.reg_is := 0;
          v.cpu_output.reg_r0 := 0;
          v.cpu_output.reg_r1 := 0;
        end if;

      when fetch_addr =>
        --Address the instruction pointed to by the instruction pointer
        v.cpu_output.led_fetch := '1';
        v.cpu_output.mem_rd    := '1';
        if r.data_not_ins then
          v.cpu_output.mem_addr := r.addr;
        else
          v.cpu_output.mem_addr := r.cpu_output.reg_ip;
          v.cpu_output.reg_ip   := (r.cpu_output.reg_ip + 1) mod byte_values;
        end if;
        v.state := fetch_read;
        v.cnt   := READ_LATENCY;
        
      when fetch_read =>
        --Wait for data and read the instruction addressed in fetch_addr
        v.cpu_output.led_fetch := '1';
        if r.cnt = 0 then
          if r.data_not_ins then
            v.data_not_ins := false;
            v.data         := r.cpu_input.mem_rdata;
            if r.cpu_output.reg_is = SWAP_R0_ADDR then
              v.state := ex_swap_r0_addr_1;
            elsif r.cpu_output.reg_is = SWAP_R1_ADDR then
              v.state := ex_swap_r1_addr_1;
            else
              v.state := halted;
            end if;
          elsif r.second_ins_byte then
            v.second_ins_byte := false;
            v.data            := r.cpu_input.mem_rdata;
            v.state           := decode_two;
          else
            v.cpu_output.reg_is := r.cpu_input.mem_rdata;
            v.state             := decode;
          end if;
        else
          v.cnt := r.cnt - 1;
        end if;
        
      when decode =>
        --Decode the fetched instruction
        v.cpu_output.led_dec := '1';
        case r.cpu_output.reg_is is
          when HALT =>
            v.state := ex_halt;
          when INC_R0 =>
            v.state := ex_inc_r0;
          when DEC_R0 =>
            v.state := ex_dec_r0;
          when INC_R1 =>
            v.state := ex_inc_r1;
          when DEC_R1 =>
            v.state := ex_dec_r1;
          when ADD_R0_R1 =>
            v.state := ex_add_r0_r1;
          when SUB_R0_R1 =>
            v.state := ex_sub_r0_r1;
          when PRINT_R0 =>
            v.state := ex_print_r0;
          when JP_IF_R0_NZ | JP_IF_R0_Z | LOAD_R0 | LOAD_R1 | STORE_R0 | STORE_R1 | SWAP_R0_ADDR | SWAP_R1_ADDR =>
            --Fetch second byte for all two byte instructions
            v.second_ins_byte := true;
            v.state           := fetch_addr;
          when others =>
            v.state := ex_halt;
        end case;
            
      when decode_two =>
        --Second decode stage for two byte instructions
        v.cpu_output.led_dec := '1';
        case r.cpu_output.reg_is is
          when JP_IF_R0_NZ =>
            v.state := ex_jp_if_r0_nz;
          when JP_IF_R0_Z =>
            v.state := ex_jp_if_r0_z;
          when LOAD_R0 =>
            v.state := ex_load_r0;
          when LOAD_R1 =>
            v.state := ex_load_r1;
          when STORE_R0 =>
            v.state := ex_store_r0;
          when STORE_R1 =>
            v.state := ex_store_r1;
          when SWAP_R0_ADDR =>
            v.state := ex_swap_r0_addr_0;
          when SWAP_R1_ADDR =>
            v.state := ex_swap_r1_addr_0;
          when others =>
            v.state := ex_halt;
        end case;
            
      when ex_halt =>
        --Execute the halt instruction
        v.cpu_output.led_exec := '1';
        v.state               := halted;
        
      when ex_inc_r0 =>
        --Increment the R0 register
        v.cpu_output.led_exec := '1';
        v.cpu_output.reg_r0   := (r.cpu_output.reg_r0 + 1) mod byte_values;
        v.state               := fetch_addr;
        
      when ex_dec_r0 =>
        --Decrement the R0 register
        v.cpu_output.led_exec := '1';
        v.cpu_output.reg_r0   := (r.cpu_output.reg_r0 - 1) mod byte_values;
        v.state               := fetch_addr;

      when ex_inc_r1 =>
        --Increment the R1 register
        v.cpu_output.led_exec := '1';
        v.cpu_output.reg_r1   := (r.cpu_output.reg_r1 + 1) mod byte_values;
        v.state               := fetch_addr;
        
      when ex_dec_r1 =>
        --Decrement the R1 register
        v.cpu_output.led_exec := '1';
        v.cpu_output.reg_r1   := (r.cpu_output.reg_r1 - 1) mod byte_values;
        v.state               := fetch_addr;

      when ex_add_r0_r1 =>
        --Add R0 and R1 and store the result in R0
        v.cpu_output.led_exec := '1';
        v.cpu_output.reg_r0   := (r.cpu_output.reg_r0 + r.cpu_output.reg_r1) mod byte_values;
        v.state               := fetch_addr;

      when ex_sub_r0_r1 =>
        --Subtract R1 from R0 and store the result in R0
        v.cpu_output.led_exec := '1';
        v.cpu_output.reg_r0   := (r.cpu_output.reg_r0 - r.cpu_output.reg_r1) mod byte_values;
        v.state               := fetch_addr;

      when ex_print_r0 =>
        --Printout the value of R0
        v.cpu_output.led_exec    := '1';
        v.cpu_output.digit_value := r.cpu_output.reg_r0;
        v.cpu_output.digit_latch := '1';
        v.state                  := fetch_addr;

      when ex_jp_if_r0_nz =>
        --JUmp to address pointed to by data is R0 != 0
        v.cpu_output.led_exec := '1';
        if not (r.cpu_output.reg_r0 = 0) then
          v.cpu_output.reg_ip := r.data;
        end if;
        v.state := fetch_addr;

      when ex_jp_if_r0_z =>
        --JUmp to address pointed to by data is R0 == 0
        v.cpu_output.led_exec := '1';
        if r.cpu_output.reg_r0 = 0 then
          v.cpu_output.reg_ip := r.data;
        end if;
        v.state := fetch_addr;

      when ex_load_r0 =>
        --Load the fetched data into R0
        v.cpu_output.led_exec := '1';
        v.cpu_output.reg_r0   := r.data;
        v.state               := fetch_addr;

      when ex_load_r1 =>
        --Load the fetched data into R1
        v.cpu_output.led_exec := '1';
        v.cpu_output.reg_r1   := r.data;
        v.state               := fetch_addr;

      when ex_store_r0 =>
        --Store the content of R0 in memory
        v.cpu_output.led_exec  := '1';
        v.cpu_output.mem_wdata := r.cpu_output.reg_r0;
        v.cpu_output.mem_addr  := r.data;
        v.cpu_output.mem_wr    := '1';
        v.state                := fetch_addr;

      when ex_store_r1 =>
        --Store the content of R0 in memory
        v.cpu_output.led_exec  := '1';
        v.cpu_output.mem_wdata := r.cpu_output.reg_r1;
        v.cpu_output.mem_addr  := r.data;
        v.cpu_output.mem_wr    := '1';
        v.state                := fetch_addr;

      when ex_swap_r0_addr_0 =>
        --Swap the content of R0 with the contnet of a memory location
        --Part 0 read
        v.cpu_output.led_exec := '1';
        v.data_not_ins        := true;
        v.addr                := r.data;
        v.state               := fetch_addr;

      when ex_swap_r0_addr_1 =>
        --Swap the content of R0 with the contnet of a memory location
        --Part 1 write
        v.cpu_output.reg_r0    := r.data;
        v.cpu_output.led_exec  := '1';
        v.cpu_output.mem_wdata := r.cpu_output.reg_r0;
        v.cpu_output.mem_addr  := r.addr;
        v.cpu_output.mem_wr    := '1';
        v.state                := fetch_addr;

      when ex_swap_r1_addr_0 =>
        --Swap the content of R1 with the contnet of a memory location
        --Part 0 read
        v.cpu_output.led_exec := '1';
        v.data_not_ins        := true;
        v.addr                := r.data;
        v.state               := fetch_addr;

      when ex_swap_r1_addr_1 =>
        --Swap the content of R1 with the contnet of a memory location
        --Part 1 write
        v.cpu_output.reg_r1    := r.data;
        v.cpu_output.led_exec  := '1';
        v.cpu_output.mem_wdata := r.cpu_output.reg_r1;
        v.cpu_output.mem_addr  := r.addr;
        v.cpu_output.mem_wr    := '1';
        v.state                := fetch_addr;

      when others =>
        null;
    end case;


    --Drive outputs
    cpu_output <= r.cpu_output;

    --Drive register
    rin <= v;
  end process p_comb;

  -- purpose: Register
  -- type   : sequential
  p_reg : process (clk, reset_n) is
  begin  -- process p_reg
    if reset_n = '0' then
      r.cpu_output.halted      <= '1';
      r.cpu_output.running     <= '0';
      r.cpu_output.paused      <= '0';
      r.cpu_output.beep        <= '0';
      r.cpu_output.digit_latch <= '0';
      r.cpu_output.led_fetch   <= '0';
      r.cpu_output.led_dec     <= '0';
      r.cpu_output.led_exec    <= '0';
      r.cpu_output.mem_wr      <= '0';
      r.cpu_output.mem_rd      <= '0';
      r.state                  <= halted;
    elsif rising_edge(clk) then
      r <= rin;
    end if;
  end process p_reg;

end architecture twoproc;

--cpu_4004.vhd ends here
