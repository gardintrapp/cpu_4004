--cpu_4004.vhd --- 
--
--Filename: cpu_4004.vhd
--Description: 
--Author: Oddbjørn Norstrand <gardintrapp@gmail.com
--Maintainer: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Created: Sat Dec 15 21:38:10 2012 (+0100)
--Version: 0.1
--Last-Updated: Sun Dec 23 14:45:23 2012 (+0100)
--          By: oddbjorn
--    Update #: 32
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

entity cpu_4004 is
  
  port (
    clk        : in  std_logic;
    reset_n    : in  std_logic;
    cpu_input  : in  cpu_4004_input_type;
    cpu_output : out cpu_4004_output_type);

end entity cpu_4004;

architecture twoproc of cpu_4004 is

  type state_type is (halted, fetch_addr, fetch_read, decode, ex_halt, ex_inc_r0, ex_dec_r0, ex_inc_r1, ex_dec_r1, paused);
  type reg_type is record
    cpu_input  : cpu_4004_input_type;
    cpu_output : cpu_4004_output_type;
    reg_ip     : byte;
    reg_is     : byte;
    reg_r0     : byte;
    reg_r1     : byte;
    state      : state_type;
  end record reg_type;
  signal r, rin : reg_type;
  
begin  -- architecture twoproc

  -- purpose: Main logic
  -- type   : combinational
  p_comb : process (cpu_input, r, r.cpu_output) is
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
        v.cpu_output.halted  := '1';
        v.cpu_output.running := '0';
        v.cpu_output.paused  := '0';
        if r.cpu_input.run = '1' then
          v.state  := fetch_addr;
          v.reg_ip := 0;
          v.reg_is := 0;
          v.reg_r0 := 0;
          v.reg_r1 := 0;
        end if;

      when fetch_addr =>
        --Address the instruction pointed to by the instruction pointer
        v.cpu_output.led_fetch := '1';
        v.cpu_output.mem_rd := '1';
        v.cpu_output.mem_addr := r.reg_ip;
        v.state := fetch_read;

      when fetch_read =>
        --Read the instruction addressed in fetch_addr
        v.cpu_output.led_fetch := '1';
        v.reg_is := r.cpu_input.mem_rdata;
        v.state := decode;
        
      when decode =>
        --Decode the fetched instruction
        v.cpu_output.led_dec := '1';
        case r.reg_is is
          when HALT =>
            v.state := ex_halt;
          when others =>
            null;
        end case;

      when ex_halt =>
        --Execute the halt instruction
        v.cpu_output.led_dec := '1';
        v.state := halted;
        
        
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
    elsif rising_edge(clk) then
      r <= rin;
    end if;
  end process p_reg;

end architecture twoproc;

--cpu_4004.vhd ends here
