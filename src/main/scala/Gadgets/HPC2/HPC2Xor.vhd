library ieee;
use ieee.std_logic_1164.all;

entity HPC2Xor is
  generic (
    d : Integer := 1;
    pipeline : Integer := 1
  );
  port (
    clk : in std_logic;
    reset : in std_logic;
    x : in std_logic_vector(d downto 0);
    y : in std_logic_vector(d downto 0);
    z : out std_logic_vector(d downto 0)
  );
end HPC2Xor;

architecture arch of HPC2Xor is

  component xor_HPC2 is
    generic (
      security_order : integer := 2; -- d
      pipeline       : integer := 1); -- 0/1
    port (
      a 		: in  std_logic_vector(security_order downto 0);
      b 		: in  std_logic_vector(security_order downto 0);
      c 		: out std_logic_vector(security_order downto 0));
  end component;

begin

  xor_hpc : xor_HPC2
  generic map (
    security_order => d,
    pipeline => pipeline
  )
  port map (
    a => x,
    b => y,
    c => z
  );

end architecture arch;