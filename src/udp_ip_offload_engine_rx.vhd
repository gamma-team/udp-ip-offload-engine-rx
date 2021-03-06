-- Top level of the UDP/IP offload engine receiver with the pure interface.
--
-- Copyright 2017 Patrick Gauvin. All rights reserved.
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

ENTITY udp_ip_offload_engine_rx IS
    GENERIC (
        -- Input and output bus width in bytes, must be a power of 2
        width : POSITIVE := 8
    );
    PORT (
        -- All ports are assumed to be synchronous with Clk
        Clk : IN STD_LOGIC;
        Rst : IN STD_LOGIC;

        Data_mac_in : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        Data_mac_in_valid : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        Data_mac_in_start : IN STD_LOGIC;
        Data_mac_in_end : IN STD_LOGIC;
        Data_mac_in_err : IN STD_LOGIC;
        Data_mac_in_ready : OUT STD_LOGIC;

        Data_app_out : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        Data_app_out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        Data_app_out_start : OUT STD_LOGIC;
        Data_app_out_end : OUT STD_LOGIC;
        Data_app_out_err : OUT STD_LOGIC;
        Data_app_out_ready : IN STD_LOGIC
    );
END ENTITY;

ARCHITECTURE structure OF udp_ip_offload_engine_rx IS
    COMPONENT udp_rx IS
        GENERIC (
            width : POSITIVE := 8
        );
        PORT (
            Clk : IN STD_LOGIC;
            Rst : IN STD_LOGIC;
            Data_in : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Data_in_valid : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Data_in_start : IN STD_LOGIC;
            Data_in_end : IN STD_LOGIC;
            Data_in_err : IN STD_LOGIC;
            Data_in_ready : OUT STD_LOGIC;
            Data_out : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Data_out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Data_out_start : OUT STD_LOGIC;
            Data_out_end : OUT STD_LOGIC;
            Data_out_err : OUT STD_LOGIC;
            Data_out_ready : IN STD_LOGIC
        );
    END COMPONENT;
    COMPONENT ip_rx IS
        GENERIC (
            width : POSITIVE := 8
        );
        PORT (
            Clk : IN STD_LOGIC;
            Rst : IN STD_LOGIC;
            Data_in : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Data_in_valid : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Data_in_start : IN STD_LOGIC;
            Data_in_end : IN STD_LOGIC;
            Data_in_err : IN STD_LOGIC;
            Data_in_ready : OUT STD_LOGIC;
            Data_out : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Data_out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Data_out_start : OUT STD_LOGIC;
            Data_out_end : OUT STD_LOGIC;
            Data_out_err : OUT STD_LOGIC;
            Data_out_ready : IN STD_LOGIC
        );
    END COMPONENT;

    SIGNAL rx_data : STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
    SIGNAL rx_valid : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
    SIGNAL rx_start, rx_end, rx_err, rx_ready : STD_LOGIC;
BEGIN
    c_udp_rx: udp_rx
        GENERIC MAP (
            width => width
        )
        PORT MAP (
            Clk => Clk,
            Rst => Rst,
            Data_in => rx_data,
            Data_in_valid => rx_valid,
            Data_in_start => rx_start,
            Data_in_end => rx_end,
            Data_in_err => rx_err,
            Data_in_ready => rx_ready,
            Data_out => Data_app_out,
            Data_out_valid => Data_app_out_valid,
            Data_out_start => Data_app_out_start,
            Data_out_end => Data_app_out_end,
            Data_out_err => Data_app_out_err,
            Data_out_ready => Data_app_out_ready
        );

    c_ip_rx: ip_rx
        GENERIC MAP (
            width => width
        )
        PORT MAP (
            Clk => Clk,
            Rst => Rst,
            Data_in => Data_mac_in,
            Data_in_valid => Data_mac_in_valid,
            Data_in_start => Data_mac_in_start,
            Data_in_end => Data_mac_in_end,
            Data_in_err => Data_mac_in_err,
            Data_in_ready => Data_mac_in_ready,
            Data_out => rx_data,
            Data_out_valid => rx_valid,
            Data_out_start => rx_start,
            Data_out_end => rx_end,
            Data_out_err => rx_err,
            Data_out_ready => rx_ready
        );
END ARCHITECTURE;
