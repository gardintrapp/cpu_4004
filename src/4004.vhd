--4004.vhd --- 
--
--Filename: 4004.vhd
--Description: 
--Author: Oddbjørn Norstrand <gardintrapp@gmail.com
--Maintainer: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Created: Sat Dec 15 21:38:10 2012 (+0100)
--Version: 0.1
--Last-Updated: Sat Dec 15 21:43:05 2012 (+0100)
--          By: oddbjorn
--    Update #: 6
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

entity cpu_4004 is
  
  port (
    clk : in std_logic;
    reset_n : in std_logic);
end entity cpu_4004;

--4004.vhd ends here
