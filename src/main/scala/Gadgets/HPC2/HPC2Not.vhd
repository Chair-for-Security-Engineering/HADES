library ieee;
use ieee.std_logic_1164.all;

entity HPC2Not is
  generic (
    d : Integer := 1;
    pipeline : Integer := 1
  );
  port (
    clk : in std_logic;
    reset : in std_logic;
    x : in std_logic_vector(d downto 0);
    z : out std_logic_vector(d downto 0)
  );
end HPC2Not;

architecture arch of HPC2Not is

  component not_masked is
    generic (
      security_order : integer := 1; -- d
      pipeline       : integer := 1); -- 0/1
    port (
      a 		: in  std_logic_vector(security_order downto 0);
      b 		: out std_logic_vector(security_order downto 0));
  end component;

begin

  not_hpc : not_masked
  generic map (
    security_order => d,
    pipeline => pipeline
  )
  port map (
    a => x,
    b => z
  );

end architecture arch;