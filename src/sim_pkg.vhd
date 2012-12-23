--sim_pkg.vhd --- 
--
--Filename: sim_pkg.vhd
--Description: 
--Author: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Maintainer: Oddbjørn Norstrand <gardintrapp@gmail.com>
--Created: Sat Dec 15 19:00:10 2012 (+0100)
--Version: 0.1
--Last-Updated: Sun Dec 16 17:00:44 2012 (+0100)
--          By: oddbjorn
--    Update #: 61
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
use std.textio.all;
use ieee.std_logic_textio.all;

package sim_pkg is

  constant ID_LOG : string := "loging";
  constant ID_SIM : string := "simlation";
  constant ID_RST : string := "reset";
  constant ID_CLK : string := "clock";

  --File to write log to
  file report_file : text open write_mode is "report.out";

  --Message severity
  type severity_type is (info, note, warning, error, failure);
  --Log messages with at least this severity level
  shared variable report_log_threshold : severity_type := info;

  --Number of suppresed messages. A message is suppressed if it is ignored du
  --to a severity level lover than report_log_threshold
  shared variable report_sup_msgs : natural := 0;

  --Number of messages with same severity level  
  type num_type is array (0 to 4) of natural;
  shared variable report_num : num_type := (others => 0);

  --Initialize the report file
  procedure report_init (
    constant file_name : in string := "report.out");

  --Write a log entry
  procedure report_log (
    constant msg : in string;
    constant id  : in string;
    constant sev : in severity_type);

  --Write a log entry if the assertion fails
  procedure report_log_assert (
    constant condition : in boolean;
    constant msg       : in string;
    constant id        : in string;
    constant sev       : in severity_type);

  --Write a report summary
  procedure report_log_summary;

  --End the simulation
  procedure sim_end;

  --Drive reset
  procedure driver_reset (
    signal clk             : in  std_logic;
    signal reset           : out std_logic;
    constant length        : in  time      := 1 us;
    constant active        : in  std_logic := '0';
    constant assert_sync   : in  boolean   := false;
    constant deassert_sync : in  boolean   := true;
    constant msg           : in  string    := "Driving reset";
    constant id            : in  string    := ID_RST);

  --Drive clock
  procedure driver_clock (
    signal clk      : out std_logic;
    constant period : in time := 100 ns;
    constant phase  : in time := 0 ns;
    constant msg    : in string := "Starting clock generator";
    constant id     : in string := ID_CLK);

end package sim_pkg;

package body sim_pkg is
  
  procedure report_init (
    constant file_name : in string := "report.out") is
    variable status : file_open_status;
  begin  -- procedure report_init
    report_log("Logging to file " & file_name, ID_SIM, info);
    file_close(report_file);
    file_open(status, report_file, file_name, write_mode);
    assert status = open_ok report "Error opening report file." severity error;
    report file_open_status'image(status) severity note;
  end procedure report_init;

  procedure report_log (
    constant msg : in string;
    constant id  : in string;
    constant sev : in severity_type) is
    variable report_line : line;
  begin  -- procedure report_log
    if severity_type'pos(sev) >= severity_type'pos(report_log_threshold) then
      write(report_line, time'image(now) & " " & id & " " & severity_type'image(sev) & " : " & msg);
      writeline(report_file, report_line);
    else
      report_sup_msgs := report_sup_msgs + 1;
    end if;
    report_num(severity_type'pos(sev)) := report_num(severity_type'pos(sev)) + 1;
  end procedure report_log;

  procedure report_log_assert (
    constant condition : in boolean;
    constant msg       : in string;
    constant id        : in string;
    constant sev       : in severity_type) is
  begin  -- procedure report_log_assert
    if not condition then
      report_log(msg, id, sev);
    end if;
  end procedure report_log_assert;

  procedure report_log_summary is
    variable report_line : line;
    constant seperator   : string := "-------------------------------------------------------------------------------";
  begin  -- procedure report_log_summary
    write(report_line, seperator);
    writeline(report_file, report_line);
    write(report_line, string'("Summary"));
    writeline(report_file, report_line);
    write(report_line, seperator);
    writeline(report_file, report_line);
    for n in report_num'range loop
      write(report_line, severity_type'image(severity_type'val(n)) & "s: " & integer'image(report_num(n)));
      writeline(report_file, report_line);
    end loop;  -- n
    write(report_line, string'("Suppressed messged: ") & integer'image(report_sup_msgs));
    writeline(report_file, report_line);
    write(report_line, seperator);
    writeline(report_file, report_line);
    write(report_line, string'("Result: "));
    if report_num(severity_type'pos(error)) > 0 or report_num(severity_type'pos(failure)) > 0 then
      write(report_line, string'("FAIL"));
    elsif report_num(severity_type'pos(warning)) > 0 then
      write(report_line, string'("PASS with warnings"));
    else
      write(report_line, string'("PASS"));
    end if;
    writeline(report_file, report_line);
    write(report_line, seperator);
    writeline(report_file, report_line);
  end procedure report_log_summary;

  procedure sim_end is
  begin  -- procedure sim_end

    report_log("Ending simulation", ID_SIM, info);
    report_log_summary;
    assert false report "Ending simulation. This is not a failure" severity failure;
    
  end procedure sim_end;

  procedure driver_reset (
    signal clk             : in  std_logic;
    signal reset           : out std_logic;
    constant length        : in  time      := 1 us;
    constant active        : in  std_logic := '0';
    constant assert_sync   : in  boolean   := false;
    constant deassert_sync : in  boolean   := true;
    constant msg           : in  string    := "Driving reset";
    constant id            : in  string    := ID_RST) is
  begin  -- procedure driver_reset
    report_log(msg, id, info);
    reset <= not active;
    if assert_sync then
      wait until rising_edge(clk);
    end if;
    reset <= active;
    wait for length;
    if deassert_sync then
      wait until rising_edge(clk);
    end if;
    reset <= not active;
  end procedure driver_reset;

  procedure driver_clock (
    signal clk      : out std_logic;
    constant period : in  time   := 100 ns;
    constant phase  : in  time   := 0 ns;
    constant msg    : in  string := "Starting clock generator";
    constant id     : in  string := ID_CLK) is
    variable v_clk : std_logic;
  begin  -- procedure driver_clock
    report_log(msg, id, info);
    v_clk := '0';
    clk <= '0';
    wait for phase;
    l_clk_gen: loop
      wait for period / 2;
      v_clk := not v_clk;
      clk <= v_clk;
    end loop l_clk_gen;
  end procedure driver_clock;
  
end package body sim_pkg;



--sim_pkg.vhd ends here
