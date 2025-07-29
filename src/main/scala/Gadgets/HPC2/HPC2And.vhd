library ieee;
use ieee.std_logic_1164.all;

entity HPC2And is
  generic (
    d : Integer := 1;
    pipeline : Integer := 1
  );
  port (
    clk : in std_logic;
    reset : in std_logic;
    x : in std_logic_vector(d downto 0);
    y : in std_logic_vector(d downto 0);
    r : in std_logic_vector(d*(d+1)/2-1 downto 0);
    z : out std_logic_vector(d downto 0)
  );
end HPC2And;

architecture arch of HPC2And is

  component and_HPC2 is
    generic (
      security_order : integer := 2; -- d
      pipeline			 : integer := 1); -- 0/1
    port (
      a 		: in  std_logic_vector(security_order downto 0);
      b 		: in  std_logic_vector(security_order downto 0);
      r			: in  std_logic_vector(((((security_order + 1) * security_order) / 2) - 1) downto 0); -- (d+1)*d /2
      clk		: in  std_logic;
      c 		: out std_logic_vector(security_order downto 0));
  end component;

begin

  and_hpc : and_HPC2
  generic map (
    security_order => d,
    pipeline => pipeline
  )
  port map(
     a => x,
     b => y,
     r => r,
     clk => clk,
     c => z
  );

end architecture arch;