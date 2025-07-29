library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity DOMNot is
  generic(
    d : Integer := 1
  );
  port (
    clk : in std_logic;
    reset : in std_logic;
    x : in std_logic_vector(d downto 0);
    z : out std_logic_vector(d downto 0)
  );
end DOMNot;

architecture arch of DOMNot is

begin
  z <= x(d downto 1) & (not x(0));
end arch;