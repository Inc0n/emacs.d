Library IEEE;
use IEEE.Std_logic_1164.all;

--  A testbench has no ports.
entity __(skeletor-underscore-proj-name)___tb is
end __(skeletor-underscore-proj-name)___tb;

architecture tb of __(skeletor-underscore-proj-name)___tb is
  --  Declaration of the component that will be instantiated.
  component __(skeletor-underscore-proj-name)__
    port (
      i0, i1, ci: in std_logic;
      s, co: out std_logic);
  end component;

  --  Specifies which entity is bound with the component.
  for __(skeletor-underscore-proj-name)___0: __(skeletor-underscore-proj-name)__ use entity work.__(skeletor-underscore-proj-name)__;
  signal i0, i1, ci, s, co : std_logic;

  constant clk_period : time := 1 ns;
  constant half_period : time := clk_period / 2;

  -- signal clk : std_logic := '0';
  signal finished : std_logic;


begin
  --  Component instantiation.
  __(skeletor-underscore-proj-name)___0: __(skeletor-underscore-proj-name)__ port map (
    i0 => i0,
    i1 => i1,
    ci => ci,
    s => s,
    co => co);

  -- clock:
    -- clk <= not clk after half_period when finished /= '1' else '0';

  --  This process does the real job.
  tb0: process
    type pattern_type is record
      --  The inputs of the __(skeletor-underscore-proj-name)__.
      i0, i1, ci : bit;
      --  The expected outputs of the __(skeletor-underscore-proj-name)__.
      s, co : bit;
    end record;
    --  The patterns to apply.
    type pattern_array is array (natural range <>) of pattern_type;
    constant patterns : pattern_array :=
      -- TODO: replace this
      (('0', '0', '0', '0', '0'),
       ('0', '0', '1', '1', '0'),
       ('0', '1', '0', '1', '0'),
       ('0', '1', '1', '0', '1'),
       ('1', '0', '0', '1', '0'),
       ('1', '0', '1', '0', '1'),
       ('1', '1', '0', '0', '1'),
       ('1', '1', '1', '1', '1'));
  begin
    --  Check each pattern.
    for i in patterns'range loop
      --  Set the inputs.
      i0 <= patterns(i).i0;
      i1 <= patterns(i).i1;
      ci <= patterns(i).ci;
      --  Wait for the results.
      wait for 1 ns;
      --  Check the outputs.
      assert s = patterns(i).s
        report "bad sum value" severity error;
      assert co = patterns(i).co
        report "bad carry out value" severity error;
    end loop;
    assert false report "all test done" severity note;
    --  Wait forever; this will finish the simulation.
    wait;
  end process;
end tb;
