--cpu_4004_pkg.vhd --- 
--
--Filename: cpu_4004_pkg.vhd
--Description: 
--Author: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Maintainer: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Created: Sat Dec 15 21:45:52 2012 (+0100)
--Version: 0.1
--Last-Updated: Mon Dec 31 16:24:01 2012 (+0100)
--          By: oddbjorn
--    Update #: 24
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

package cpu_4004_pkg is

  subtype byte is integer range 0 to 15;
  constant byte_values : integer := byte'high + 1;
  subtype addr_range is integer range 0 to 15;
  type memory_type is array (addr_range) of byte;

  --Instructions
  --One byte instructions
  constant HALT      : byte := 0;
  constant INC_R0    : byte := 1;
  constant DEC_R0    : byte := 2;
  constant INC_R1    : byte := 3;
  constant DEC_R1    : byte := 4;
  constant ADD_R0_R1 : byte := 5;
  constant SUB_R0_R1 : byte := 6;
  constant PRINT_R0  : byte := 7;

  --Two byute instructions
  constant JP_IF_R0_NZ  : byte := 8;
  constant JP_IF_R0_Z   : byte := 9;
  constant LOAD_R0      : byte := 16#a#;
  constant LOAD_R1      : byte := 16#b#;
  constant STORE_R0     : byte := 16#c#;
  constant STORE_R1     : byte := 16#d#;
  constant SWAP_R0_ADDR : byte := 16#e#;
  constant SWAP_R1_ADDR : byte := 16#f#;


  --Input signals to the 4004
  type cpu_4004_input_type is record
    run       : std_logic;
    stop      : std_logic;
    step      : std_logic;
    mem_rdata : byte;
  end record cpu_4004_input_type;

  --Output signals from the 4004
  type cpu_4004_output_type is record
    running     : std_logic;
    paused      : std_logic;
    halted      : std_logic;
    beep        : std_logic;
    digit_value : byte;
    digit_latch : std_logic;
    led_fetch   : std_logic;
    led_dec     : std_logic;
    led_exec    : std_logic;
    mem_wdata   : byte;
    mem_addr    : addr_range;
    mem_wr      : std_logic;
    mem_rd      : std_logic;
    reg_r0      : byte;
    reg_r1      : byte;
    reg_ip      : byte;
    reg_is      : byte;
  end record cpu_4004_output_type;
  
end package cpu_4004_pkg;

--cpu_4004_pkg.vhd ends here
