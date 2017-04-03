-- Top level UDP/IP offload engine receiver with AXI4-Stream interface.
--
-- Copyright 2017 Patrick Gauvin
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its
-- contributors may be used to endorse or promote products derived from this
-- software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY udp_ip_offload_engine_rx_axis IS
    GENERIC (
        width : POSITIVE := 8
    );
    PORT (
        Clk : IN STD_LOGIC;
        Rstn : IN STD_LOGIC;

        S00_axis_mac_tdata : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        S00_axis_mac_tvalid : IN STD_LOGIC;
        S00_axis_mac_tkeep : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        S00_axis_mac_tlast : IN STD_LOGIC;
        S00_axis_mac_tready : OUT STD_LOGIC;

        M00_axis_app_tdata : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        M00_axis_app_tvalid : OUT STD_LOGIC;
        M00_axis_app_tkeep : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        M00_axis_app_tlast : OUT STD_LOGIC;
        M00_axis_app_tready : IN STD_LOGIC
    );
END ENTITY;

ARCHITECTURE normal OF udp_ip_offload_engine_rx_axis IS
    COMPONENT udp_ip_offload_engine_rx IS
        GENERIC (
            width : POSITIVE := 8
        );
        PORT (
            Clk : IN STD_LOGIC;
            Rst : IN STD_LOGIC;
            Data_mac_in : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Data_mac_in_valid : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Data_mac_in_start : IN STD_LOGIC;
            Data_mac_in_end : IN STD_LOGIC;
            Data_mac_in_err : IN STD_LOGIC;
            Data_app_out : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Data_app_out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Data_app_out_start : OUT STD_LOGIC;
            Data_app_out_end : OUT STD_LOGIC;
            Data_app_out_err : OUT STD_LOGIC
        );
    END COMPONENT;

    CONSTANT no_valid_data : STD_LOGIC_VECTOR(width - 1 DOWNTO 0)
        := (OTHERS => '0');

    SIGNAL data_mac_in : STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
    SIGNAL data_mac_in_valid : STD_LOGIC_VECTOR(width -1 DOWNTO 0);
    SIGNAL data_mac_in_start, data_mac_in_end, data_mac_in_err : STD_LOGIC;
    SIGNAL data_app_out : STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
    SIGNAL data_app_out_valid : STD_LOGIC_VECTOR(width -1 DOWNTO 0);
    SIGNAL data_app_out_start, data_app_out_end, data_app_out_err : STD_LOGIC;

    SIGNAL mac_in_d_data : STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
    SIGNAL mac_in_d_valid : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
    SIGNAL mac_in_d_start, mac_in_d_end, mac_in_d_err : STD_LOGIC;

    SIGNAL rst : STD_LOGIC;
BEGIN
    rst <= NOT Rstn;

    -- Handle application layer input/output signals
    PROCESS(Clk)
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rstn = '0' THEN
                M00_axis_app_tdata <= (OTHERS => '0');
                M00_axis_app_tvalid <= '0';
                M00_axis_app_tkeep <= (OTHERS => '0');
                M00_axis_app_tlast <= '0';
            ELSE
                M00_axis_app_tvalid <= '0';
                M00_axis_app_tkeep <= (OTHERS => '0');
                M00_axis_app_tlast <= '0';
                IF data_app_out_valid /= no_valid_data THEN
                    IF M00_axis_app_tready /= '1' THEN
                        -- FIXME: ERROR CONDITION
                    END IF;
                    M00_axis_app_tkeep <= data_app_out_valid;
                    M00_axis_app_tdata <= data_app_out;
                    M00_axis_app_tvalid <= '1';
                END IF;
                -- NOTE: Transers of only null bytes are legal
                IF data_app_out_end = '1' THEN
                    M00_axis_app_tlast <= '1';
                END IF;
            END IF;
        END IF;
    END PROCESS;

    -- Handle MAC layer input/output signals
    PROCESS(Clk)
        VARIABLE started : BOOLEAN;
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rstn = '0' THEN
                S00_axis_mac_tready <= '1';
                data_mac_in <= (OTHERS => '0');
                data_mac_in_valid <= (OTHERS => '0');
                data_mac_in_start <= '0';
                data_mac_in_end <= '0';
                data_mac_in_err <= '0';
                started := false;
            ELSE
                data_mac_in_start <= '0';
                data_mac_in_end <= '0';
                data_mac_in_valid <= (OTHERS => '0');
                S00_axis_mac_tready <= '1';
                -- TODO: add pushback to the inner core
                IF S00_axis_mac_tvalid = '1' THEN
                    data_mac_in <= S00_axis_mac_tdata;
                    data_mac_in_valid <= S00_axis_mac_tkeep;
                    -- tready acknowledges the data
                    IF NOT started THEN
                        data_mac_in_start <= '1';
                        started := true;
                    END IF;
                    IF S00_axis_mac_tlast = '1' THEN
                        data_mac_in_end <= '1';
                        started := false;
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    c_udp_ip_offload_engine_rx: udp_ip_offload_engine_rx
        GENERIC MAP (
            width => width
        )
        PORT MAP (
            Clk => Clk,
            Rst => rst,
            Data_mac_in => data_mac_in,
            Data_mac_in_valid => data_mac_in_valid,
            Data_mac_in_start => data_mac_in_start,
            Data_mac_in_end => data_mac_in_end,
            Data_mac_in_err => data_mac_in_err,
            Data_app_out => data_app_out,
            Data_app_out_valid => data_app_out_valid,
            Data_app_out_start => data_app_out_start,
            Data_app_out_end => data_app_out_end,
            Data_app_out_err => data_app_out_err
        );
END ARCHITECTURE;
