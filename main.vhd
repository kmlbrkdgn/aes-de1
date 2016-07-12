-- *********************************************************************************************
-- ENTITY           :	main
--
-- DESCRIPTION      :   Top level design    
--
-- DESIGN NOTE      :   This entitiy connects all other components
--
-- REVISION HISTORY :	Demo features added
--
-- **********************************************************************************************

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


ENTITY main IS
PORT(
    --data_in	 :   IN  std_logic_vector(127 downto 0);
	 --key_m		 :   IN  std_logic_vector(127 downto 0);
    --data_out	 :   OUT std_logic_vector(127 downto 0); --Simulation test
   
	 sseg0		 :	  OUT std_logic_vector (6 downto 0);
	 sseg1		 :	  OUT std_logic_vector (6 downto 0);
 	 sseg2		 :	  OUT std_logic_vector (6 downto 0);
 	 sseg3		 :	  OUT std_logic_vector (6 downto 0);
	 next_button :   IN	std_logic;		
	 load_de		 :   IN	std_logic;	 
	 d_in			 :	  IN std_logic_vector (2 downto 0);
	 d_key		 :	  IN std_logic_vector (2 downto 0); --BOARD DEMO

	 encrypt		 :   IN	std_logic;
    decrypt		 :   IN	std_logic;
	 clk			 :   IN	std_logic;
    reset		 :   IN	std_logic;
    busy        :	  OUT std_logic

    );
END main;

------------------------------------------------------------------
--MODULES
------------------------------------------------------------------

ARCHITECTURE beh OF main IS

COMPONENT control
PORT(
    reset	    	:	IN  std_logic;
    clk		    	:  IN  std_logic;
    encrypt	   	:  IN  std_logic;
	 decrypt	  	:  IN  std_logic;
    data_sel      :  OUT std_logic_vector(1 downto 0);
    load_data     :  OUT std_logic;
    key_sel       :  OUT std_logic;
    round_const	:  OUT std_logic_vector(7 downto 0);
    last_mux_sel  :	OUT std_logic;
    busy          :	OUT std_logic;
    load_key      :	OUT std_logic
   );
END COMPONENT;

COMPONENT round 
PORT(
    d_in				:   IN  std_logic_vector(127 downto 0);
    key				:   IN  std_logic_vector(127 downto 0);
    last_mux_sel	:   IN  std_logic;
    data_out		:   OUT std_logic_vector(127 downto 0)
    );
END COMPONENT;

COMPONENT inv_round 
PORT(
    d_in				:   IN  std_logic_vector(127 downto 0);
    key				:   IN  std_logic_vector(127 downto 0);
    last_mux_sel	:   IN  std_logic;
    data_out		:   OUT std_logic_vector(127 downto 0)
    );
END COMPONENT;

COMPONENT key_schedule 
PORT(
    clk		 	   	:  IN  std_logic;
    reset	   		:  IN  std_logic;
    key_in	   		:  IN  std_logic_vector(127 downto 0);
    key_out	   		:  OUT std_logic_vector(127 downto 0);
    key_sel         	:  IN  std_logic;
    enorde          	:  IN  std_logic;
    round_constant  	:	IN  std_logic_vector(7 downto 0);
    load_key        	:	IN  std_logic
    );
END COMPONENT;

------------------------------------------------------------------
--SIGNAL ASSIGNMENTS
------------------------------------------------------------------

SIGNAL key_sel         		: std_logic;

SIGNAL round0_out_en       : std_logic_vector(127 downto 0);
SIGNAL round0_out_de       : std_logic_vector(127 downto 0);

SIGNAL data_reg_in_en      : std_logic_vector(127 downto 0);
SIGNAL data_reg_in_de      : std_logic_vector(127 downto 0);

SIGNAL data_reg_out_en     : std_logic_vector(127 downto 0);
SIGNAL data_reg_out_de     : std_logic_vector(127 downto 0);

SIGNAL round1_10_out_en    : std_logic_vector(127 downto 0);
SIGNAL round1_10_out_de    : std_logic_vector(127 downto 0);


SIGNAL key              	: std_logic_vector(127 downto 0);
SIGNAL data_out_en         : std_logic_vector(127 downto 0);
SIGNAL data_out_de         : std_logic_vector(127 downto 0);
SIGNAL round_constant   	: std_logic_vector(7 downto 0);
SIGNAL data_sel         	: std_logic_vector(1 downto 0);
SIGNAL load_data        	: std_logic;
SIGNAL load_key         	: std_logic; 
SIGNAL last_mux_sel     	: std_logic;

SIGNAL enorde : std_logic;

------------------------------------------------------------------
--FOR BOARD DEMO
------------------------------------------------------------------
SIGNAL data_in		:std_logic_vector(127 downto 0);
SIGNAL data_out	:std_logic_vector(127 downto 0);
SIGNAL key_m		:std_logic_vector(127 downto 0);
SIGNAL d_out		:std_logic_vector(15 downto 0);
SIGNAL key_de     :std_logic_vector(127 downto 0);
SIGNAL d_in_de    :std_logic_vector(127 downto 0);
SIGNAL i 			:integer:=0;
------------------------------------------------------------------

BEGIN

------------------------------------------------------------------
--FOR BOARD DEMO
------------------------------------------------------------------
data_in 	<= std_logic_vector(resize(unsigned(d_in),128)) when encrypt = '1' else
				d_in_de when decrypt = '1';
key_m 	<= std_logic_vector(resize(unsigned(d_key),128)) when encrypt = '1' else
				key_de when decrypt = '1';
				
load_decryption:
process (reset,load_de)
begin
	if reset = '1' then 
		key_de <= (others=>'0');
		d_in_de <= (others=> '0');
	elsif falling_edge(load_de) and clk = '1' then
		key_de <= key;
		d_in_de <= data_out;	
	end if;
end process;
------------------------------------------------------------------


proc_inter:PROCESS (clk) 
BEGIN

	IF (clk'EVENT AND clk = '1') THEN
	
			IF encrypt = '1' THEN
				enorde <= '0';
			ELSIF decrypt = '1' THEN
				enorde <= '1';
			END IF;

   END IF;
END PROCESS proc_inter;


------------------------------------------------------------------
--CONNECTIONS OF MODULES
------------------------------------------------------------------   


contrl: control
PORT MAP(
	reset				=> reset,   
	clk				=> clk,
	encrypt			=> encrypt,
	decrypt			=> decrypt,
	data_sel       => data_sel,
	load_data      => load_data,
	key_sel	      => key_sel,
	round_const		=> round_constant,
	last_mux_sel	=> last_mux_sel,
	busy	        	=> busy,
	load_key			=> load_key
	);


--key generator for each rounds
key_generator: key_schedule
PORT MAP(
	clk				=>  clk,
	reset				=>  reset,
	key_in			=>  key_m,	
	key_out			=>  key,
	key_sel	      =>  key_sel,
   enorde         =>  enorde,
	round_constant	=>  round_constant,
	load_key			=>  load_key
	);


--MUX
 data_reg_in_en <=  
		round0_out_en 		WHEN data_sel = "00" ELSE 
		round1_10_out_en 	WHEN data_sel = "01" ELSE
			data_in;

 data_reg_in_de <=  
		round0_out_de 		WHEN data_sel = "00" ELSE
		round1_10_out_de 	WHEN data_sel = "01" ELSE
         data_in;

 round0_out_en <= data_reg_out_en XOR key;
 round0_out_de <= data_reg_out_de XOR key;


 layers: round
 PORT MAP(
	d_in	   		 =>  data_reg_out_en, 
	key	   		 =>  key,
	last_mux_sel	 =>  last_mux_sel,
	data_out    	 =>  round1_10_out_en 
	);


 layers1:inv_round
 PORT MAP(
	d_in	    		=>  data_reg_out_de, 
	key	    		=>  key,
	last_mux_sel	=>  last_mux_sel,
	data_out    	=>  round1_10_out_de  
	);


data_register:PROCESS(clk, reset)
BEGIN
    IF(reset='1') THEN
		data_reg_out_en <= (others => '0');
		data_reg_out_de <= (others => '0');
    ELSIF(clk'event AND clk='1') THEN
	    IF(load_data='1') THEN
			data_reg_out_en <= data_reg_in_en;
			data_reg_out_de <= data_reg_in_de;
	    END IF;
    END IF;
	
END PROCESS data_register;

--MUX_out

data_out <= data_reg_out_en when encrypt = '1' else
				data_reg_out_de when decrypt = '1'; 
			 
------------------------------------------------------------------
--FOR BOARD DEMO
------------------------------------------------------------------
display:
process (reset,next_button)
begin
	if reset = '1' then
		i<=0;
	elsif falling_edge(next_button) and clk = '1' then
		if i= 127 then
			i<=0;
		 end if;

	d_out <= data_out(i+15 downto i);
	i <= i+16;	 
	  
	end if;
end process;

------------------------------------------------------------------

		with d_out(3 downto 0) select sseg0 <= 
		
		"1000000" when  "0000",--0
		"1111001" when  "0001",--1
		"0100100" when  "0010",--2
		"0110000" when  "0011",
		"0011001" when  "0100",
		"0010010" when  "0101",
		"0000010" when  "0110",
		"1111000" when  "0111",
		"0000000" when  "1000",
		"0011000" when  "1001",
		"0001000" when  "1010",--A
		"0000011" when  "1011",
		"1000110" when  "1100",
		"0100001" when  "1101",
		"0000110" when  "1110",
		"0001110" when  "1111",--F
		"1000000" when others;
		
		with d_out(7 downto 4) select sseg1 <= 
		
		"1000000" when  "0000",--0
		"1111001" when  "0001",--1
		"0100100" when  "0010",--2
		"0110000" when  "0011",
		"0011001" when  "0100",
		"0010010" when  "0101",
		"0000010" when  "0110",
		"1111000" when  "0111",
		"0000000" when  "1000",
		"0011000" when  "1001",
		"0001000" when  "1010",--A
		"0000011" when  "1011",
		"1000110" when  "1100",
		"0100001" when  "1101",
		"0000110" when  "1110",
		"0001110" when  "1111",--F
		"1000000" when others;
		
		with d_out(11 downto 8) select sseg2 <= 
		
		"1000000" when  "0000",--0
		"1111001" when  "0001",--1
		"0100100" when  "0010",--2
		"0110000" when  "0011",
		"0011001" when  "0100",
		"0010010" when  "0101",
		"0000010" when  "0110",
		"1111000" when  "0111",
		"0000000" when  "1000",
		"0011000" when  "1001",
		"0001000" when  "1010",--A
		"0000011" when  "1011",
		"1000110" when  "1100",
		"0100001" when  "1101",
		"0000110" when  "1110",
		"0001110" when  "1111",--F
		"1000000" when others;
		
		with d_out(15 downto 12) select sseg3 <= 
		
		"1000000" when  "0000",--0
		"1111001" when  "0001",--1
		"0100100" when  "0010",--2
		"0110000" when  "0011",
		"0011001" when  "0100",
		"0010010" when  "0101",
		"0000010" when  "0110",
		"1111000" when  "0111",
		"0000000" when  "1000",
		"0011000" when  "1001",
		"0001000" when  "1010",--A
		"0000011" when  "1011",
		"1000110" when  "1100",
		"0100001" when  "1101",
		"0000110" when  "1110",
		"0001110" when  "1111",--F
		"1000000" when others;
		
			 
END beh;
