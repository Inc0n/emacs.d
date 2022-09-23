Library IEEE;
use IEEE.Std_logic_1164.all;

entity __(skeletor-underscore-proj-name)__ is
  -- `i0`, `i1`, and the carry-in `ci` are inputs of the __(skeletor-underscore-proj-name)__.
  -- `s` is the sum output, `co` is the carry-out.
  port (
    i0, i1, ci: in std_logic;
    s, co: out std_logic);
end __(skeletor-underscore-proj-name)__;

architecture rtl of __(skeletor-underscore-proj-name)__ is
begin
  --  This full-__(skeletor-underscore-proj-name)__ architecture contains two concurrent assignments.
  --  Compute the sum.
  s <= i0 xor i1 xor ci;
  --  Compute the carry.
  co <= (i0 and i1) or (i0 and ci) or (i1 and ci);
end rtl;
