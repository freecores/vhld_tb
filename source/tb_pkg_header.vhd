-------------------------------------------------------------------------------
--             Copyright 2007  Ken Campbell
-------------------------------------------------------------------------------
-- $Author: sckoarn $
--
-- $Date: 2007-04-06 04:06:48 $
--
-- $Name: not supported by cvs2svn $
--
-- $Id: tb_pkg_header.vhd,v 1.1.1.1 2007-04-06 04:06:48 sckoarn Exp $
--
-- $Source: /home/marcus/revision_ctrl_test/oc_cvs/cvs/vhld_tb/source/tb_pkg_header.vhd,v $
--
-- Description :  The the testbench package header file.
--                Initial GNU release.
--
------------------------------------------------------------------------------
--This file is part of The VHDL Test Bench.
--
--    The VHDL Test Bench is free software; you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation; either version 2 of the License, or
--    (at your option) any later version.
--
--    Foobar is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with The VHDL Test Bench; if not, write to the Free Software
--    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-------------------------------------------------------------------------------
-- Revision History:
-- $Log: not supported by cvs2svn $
--
-------------------------------------------------------------------------------
library IEEE;

use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use std.textio.all;
library ieee_proposed;
use ieee_proposed.STD_LOGIC_1164_additions.all;

package ew_tb_pkg2 is

  -- Constants
  constant max_str_len   : integer := 256;
  constant max_field_len : integer := 48;
  constant c_stm_text_len  : integer := 128;
  -- file handles
  file stimulus     : text;             -- file main file
  file include_file : text;             -- file declaration for includes

  -- Type Def's
  type base is (bin, oct, hex, dec);
--  subtype stack_element is integer range 0 to 8192;
  type stack_register is array(7 downto 0) of integer;
  type state_register is array(7 downto 0) of boolean;
  type int_array      is array(1 to 16) of integer;
  
  subtype text_line  is string(1 to max_str_len);
  subtype text_field is string(1 to max_field_len);
  subtype stm_text is string(1 to c_stm_text_len);
  type stm_text_ptr is access stm_text;
  -- define the stimulus line record and access
  type stim_line;
  type stim_line_ptr is access stim_line;     -- Pointer to stim_line record
  type stim_line is record 
    instruction:   text_field;
    inst_field_1:  text_field;
    inst_field_2:  text_field;
    inst_field_3:  text_field;
    inst_field_4:  text_field;
    inst_field_5:  text_field;
    inst_field_6:  text_field;
    txt:           stm_text_ptr;
    line_number:   integer;      -- sequence line
    num_of_lines:  integer;      -- total number of lines
    file_line:     integer;      -- file line number
    file_name:     text_line;
    next_rec:      stim_line_ptr;
  end record;
  -- define the variables field and pointer
  type var_field;
  type var_field_ptr is access var_field;  -- pointer to var_field
  type var_field is record
    var_name:     text_field;
    var_index:    integer;
    var_value:    integer;
    next_rec:     var_field_ptr;
  end record;
  -- define the instruction structure
  type inst_def;
  type inst_def_ptr is access inst_def;
  type inst_def is record
    instruction:     text_field;
    instruction_l:   integer;
    params:          integer;
    next_rec:        inst_def_ptr;
  end record;
  
---*****************************************************************************
  -- Function Declaration
--  function str_len(variable line: text_line) return text_field;
--  function fld_len(s : in text_field) integer;
    function ew_to_slv(value : in integer;
                       length : in integer) return std_logic_vector;

    function c2std_vec(c: in character) return std_logic_vector;
    
--------------------------------------------------------------------------------
  -- Procedure declarations
--------------------------------------------------------------------------
-- define_instruction
--    inputs     file_name  the file to be read from
--
--    output     file_line  a line of text from the file
  procedure define_instruction(variable inst_set: inout inst_def_ptr;
                               constant inst:     in    string;
                               constant args:     in    integer);

--------------------------------------------------------------------------------
--  index_variable
--     inputs:
--               index:  the index of the variable being accessed
--     outputs:
--               Variable Value
--               valid  is 1 if valid 0 if not
  procedure index_variable(variable var_list : in  var_field_ptr;
                           variable index    : in  integer;
                           variable value    : out integer;
                           variable valid    : out integer);

--------------------------------------------------------------------------------
--  update_variable
--     inputs:
--               index:  the index of the variable being accessed
--     outputs:
--               Variable Value
--               valid  is 1 if valid 0 if not
  procedure update_variable(variable var_list : in  var_field_ptr;
                            variable index    : in  integer;
                            variable value    : in  integer;
                            variable valid    : out integer);

-------------------------------------------------------------------------------
-- read_instruction_file
--  This procedure reads the instruction file, name passed throught file_name.
--  Pointers to records are passed in and out.  A table of variables is created
--  with variable name and value (converted to integer).  The instructions are
--  parsesed into the inst_sequ list.  Instructions are validated against the
--  inst_set which must have been set up prior to loading the instruction file.
  procedure read_instruction_file(constant file_name:  string;
                                  variable inst_set:   inout inst_def_ptr;
                                  variable var_list:   inout var_field_ptr;
                                  variable inst_sequ:  inout stim_line_ptr);

------------------------------------------------------------------------------
-- access_inst_sequ
--   This procedure retreeves an instruction from the sequence of instructions.
--   Based on the line number you pass to it, it returns the instruction with
--   any variables substituted as integers.
  procedure access_inst_sequ(variable inst_sequ  :  in  stim_line_ptr;
                             variable var_list   :  in  var_field_ptr;
                             variable sequ_num   :  in  integer;
                             variable inst       :  out text_field;
                             variable p1         :  out integer;
                             variable p2         :  out integer;
                             variable p3         :  out integer;
                             variable p4         :  out integer;
                             variable p5         :  out integer;
                             variable p6         :  out integer;
                             variable txt        :  out stm_text_ptr;
                             variable inst_len   :  out integer;
                             variable fname      :  out text_line;
                             variable file_line  :  out integer
                             );
------------------------------------------------------------------------
--  tokenize_line
--    This procedure takes a type text_line in and returns up to 6
--    tokens and the count in integer valid, as well if text string
--    is found the pointer to that is returned.
  procedure tokenize_line(variable text_line:   in  text_line;
                          variable token1:      out text_field;
                          variable token2:      out text_field;
                          variable token3:      out text_field;
                          variable token4:      out text_field;
                          variable token5:      out text_field;
                          variable token6:      out text_field;
                          variable token7:      out text_field;
                          variable txt_ptr:     out stm_text_ptr;
                          variable valid:       out integer);
-------------------------------------------------------------------------
-- string convertion
  function ew_to_str(int: integer; b: base) return text_field;
  function to_str(int: integer) return string;

-------------------------------------------------------------------------
--  Procedre print
--    print to stdout  string
  procedure print(s: in string);
-------------------------------------------------------------------------
--  Procedure print stim txt
  procedure txt_print(variable ptr: in stm_text_ptr);
-------------------------------------------------------------------------
--  Procedure print stim txt sub variables found
  procedure txt_print_wvar(variable var_list   :  in  var_field_ptr;
                           variable ptr        :  in  stm_text_ptr;
                           constant b          :  in  base);
-------------------------------------------------------------------------
-- dump inst_sequ
--  This procedure dumps to the simulation window the current instruction
--  sequence.  The whole thing will be dumped, which could be big.
--   ** intended for testbench development debug**
  procedure dump_inst_sequ(variable inst_sequ  :  in  stim_line_ptr);
  
end ew_tb_pkg2;