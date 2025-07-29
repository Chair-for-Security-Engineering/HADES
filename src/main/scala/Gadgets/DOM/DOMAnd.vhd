library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity DOMAnd is
  generic(
    d : Integer := 1
  );
  port (
    clk : in std_logic;
    reset : in std_logic;
    x : in std_logic_vector(d downto 0);
    y : in std_logic_vector(d downto 0);
    r : in std_logic_vector(d*(d+1)/2-1 downto 0);
    z : out std_logic_vector(d downto 0)
  );
end DOMAnd;

architecture arch of DOMAnd is

  component shared_mul is
    generic (
      PIPELINED  : string  := "no";
      NUM_SHARES : integer := 2;
      DATA_WIDTH : integer := 4
    );
    port (
      -- Clock and reset
      ClkxCI : in  std_logic;
      RstxBI : in  std_logic;
      -- Shares of X and Y
      XxDI   : in  std_logic_vector(DATA_WIDTH*NUM_SHARES-1 downto 0);
      YxDI   : in  std_logic_vector(DATA_WIDTH*NUM_SHARES-1 downto 0);
      -- Fresh masks
      ZxDI   : in  std_logic_vector(DATA_WIDTH*((NUM_SHARES-2)*2 + 1)-1 downto 0);
      -- Output Q = X*Y (+ Z)
      QxDO   : out std_logic_vector(DATA_WIDTH*NUM_SHARES-1 downto 0)
    );
  end component;

begin

  mul : shared_mul
  generic map (
    pipelined => "yes",
    num_shares => d+1,
    data_width => 1
  )
  port map (
    ClkxCI => clk,
    RstxBI => not reset,
    -- Shares of X and Y
    XxDI   => x,
    YxDI   => y,
    -- Fresh masks
    ZxDI   => r,
    -- Output Q = X*Y (+ Z)
    QxDO   => z
  );

end arch;