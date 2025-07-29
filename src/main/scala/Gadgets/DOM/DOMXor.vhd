library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity DOMXor is
  generic(
    d : Integer := 1
  );
  port (
    clk : in std_logic;
    reset : in std_logic;
    x : in std_logic_vector(d downto 0);
    y : in std_logic_vector(d downto 0);
    z : out std_logic_vector(d downto 0)
  );
end DOMXor;

architecture arch of DOMXor is

begin
  z <= x xor y;
end arch;