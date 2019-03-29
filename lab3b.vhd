----------------------------------------------------------------------
-- EECS31L/CSE31L Assignment3
-- Locator Behavioral Model
----------------------------------------------------------------------
-- Student First Name : Hayden
-- Student Last Name : Yu
-- Student ID : 66185399
----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;


entity Locator_beh  is
    Port ( Start : in  STD_LOGIC;
           Rst : in  STD_LOGIC;
           Clk : in  STD_LOGIC;
           Loc : out  STD_LOGIC_VECTOR (15 downto 0);
           Done : out  STD_LOGIC);
end Locator_beh;

architecture Behavioral of Locator_beh  is

   TYPE regArray_type IS 
      ARRAY (0 TO 7) OF std_logic_vector(15 DOWNTO 0); 
   SIGNAL regArray : regArray_type :=  (X"0000", X"000A", X"0003", X"0002", X"0006", X"0000", X"0000", X"0000");     

-- do not modify any code above this line
-- additional variables/signals can be declared if needed
-- add your code starting here

TYPE StateType IS
          (vt_state, at_state, at2_state,
           at2divided_state, sum1_state, sum2_state,
           end_state, start_state);
Signal CurrState, NextState: StateType;

Signal temp: std_logic_vector (31 downto 0);

Signal OutReg: std_logic_vector (15 downto 0);

begin

CombLogic: Process(Clk, CurrState, Start)
BEGIN
    If(rising_edge(Clk)) Then
        If(Rst = '1') Then
            CurrState <= start_state;
        Else
            CurrState <= NextState;
        end if;
    end if;
    
    If(CurrState'Event or rising_edge(Start)) Then
        Loc <= (Others=>'Z');
        Done <= '0';
        CASE CurrState is
            When start_state =>
                If(Start = '1') Then
                    NextState <= vt_state;
                Else
                    NextState <= start_state;
                end if;
            When vt_state =>
                    temp <= regArray(2) * regArray(4);
                    regArray(5) <= temp(15 downto 0);
                    NextState <= at_state;
            When at_state =>
                    temp <= regArray(1) * regArray(4);
                    regArray(6) <= temp(15 downto 0);
                    NextState <= at2_state;
            When at2_state =>
                    temp <= regArray(6) * regArray(4);
                    regArray(6) <= temp(15 downto 0);
                    NextState <= at2divided_state;
            When at2divided_state =>
                    regArray(6) <= regArray(6)(15) & regArray(6)(15 downto 1);
                    NextState <= sum1_state;
            When sum1_state =>
                    regArray(7) <= regArray(6) + regArray(5);
                    NextState <= sum2_state;
            When sum2_state =>
                    OutReg <= regArray(7) + regArray(3);
                    NextState <= end_state;
            When end_state =>
                Loc <= OutReg;
                Done  <= '1';
                NextState <= start_state;
            end case;
      end if;
end process;

end Behavioral;

