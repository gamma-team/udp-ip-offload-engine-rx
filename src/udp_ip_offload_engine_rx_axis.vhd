-- Top level UDP/IP offload engine receiver with AXI4-Stream interface.
--
-- The internal module interface is not quite AXI4-Stream compatible, this
-- provides the glue.
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
            Data_mac_in_ready : OUT STD_LOGIC;
            Data_app_out : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Data_app_out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Data_app_out_start : OUT STD_LOGIC;
            Data_app_out_end : OUT STD_LOGIC;
            Data_app_out_err : OUT STD_LOGIC;
            Data_app_out_ready : IN STD_LOGIC
        );
    END COMPONENT;

    COMPONENT stream_packer IS
        GENERIC (
            width : POSITIVE := 8
        );
        PORT (
            Clk : IN STD_LOGIC;
            Rstn : IN STD_LOGIC;
            In_data : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            In_valid : IN STD_LOGIC;
            In_keep : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            In_last : IN STD_LOGIC;
            In_ready : OUT STD_LOGIC;
            Out_data : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Out_valid : OUT STD_LOGIC;
            Out_keep : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Out_last : OUT STD_LOGIC;
            Out_ready : IN STD_LOGIC
        );
    END COMPONENT;

    CONSTANT no_valid_data : STD_LOGIC_VECTOR(width - 1 DOWNTO 0)
        := (OTHERS => '0');

    SIGNAL data_mac_in : STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
    SIGNAL data_mac_in_valid : STD_LOGIC_VECTOR(width -1 DOWNTO 0);
    SIGNAL data_mac_in_start, data_mac_in_end, data_mac_in_err,
        data_mac_in_ready : STD_LOGIC;
    SIGNAL data_app_out : STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
    SIGNAL data_app_out_valid : STD_LOGIC_VECTOR(width -1 DOWNTO 0);
    SIGNAL data_app_out_start, data_app_out_end, data_app_out_err,
        data_app_out_ready : STD_LOGIC;

    SIGNAL mac_in_d_data : STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
    SIGNAL mac_in_d_valid : STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
    SIGNAL mac_in_d_start, mac_in_d_end, mac_in_d_err : STD_LOGIC;

    SIGNAL rst : STD_LOGIC;
    SIGNAL started : BOOLEAN;

    SIGNAL data_app_out_axi_valid : STD_LOGIC;
BEGIN
    rst <= NOT Rstn;


    -- Use the stream packer to keep things Xilinx friendly. They do not
    -- generally support NULL bytes mid-stream, this will group them together
    -- with the last transfer.
    c_stream_packer: stream_packer
        GENERIC MAP (
            width => width
        )
        PORT MAP (
            Clk => clk,
            Rstn => rstn,
            In_data => data_app_out,
            In_valid => data_app_out_axi_valid,
            In_keep => data_app_out_valid,
            In_last => data_app_out_end,
            In_ready => data_app_out_ready,
            Out_data => M00_axis_app_tdata,
            Out_valid => M00_axis_app_tvalid,
            Out_keep => M00_axis_app_tkeep,
            Out_last => M00_axis_app_tlast,
            Out_ready => M00_axis_app_tready
        );
    data_app_out_axi_valid  <= '1' WHEN data_app_out_valid /= no_valid_data
        ELSE '0';

    S00_axis_mac_tready <= data_mac_in_ready;
    data_mac_in <= S00_axis_mac_tdata;
    data_mac_in_valid <= S00_axis_mac_tkeep WHEN '1' = S00_axis_mac_tvalid
        ELSE (OTHERS => '0');
    data_mac_in_end <= S00_axis_mac_tlast AND S00_axis_mac_tvalid;
    data_mac_in_err <= '0';
    data_mac_in_start <= '1' WHEN NOT started AND '1' = S00_axis_mac_tvalid
        ELSE '0';
    -- Handle MAC layer input/output signals
    PROCESS(Clk)
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rstn = '0' THEN
                started <= false;
            ELSE
                IF S00_axis_mac_tlast = '1' THEN
                    started <= false;
                END IF;
                IF NOT started AND '1' = S00_axis_mac_tvalid
                        AND '1' = data_mac_in_ready THEN
                    started <= true;
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
            Data_mac_in_ready => data_mac_in_ready,
            Data_app_out => data_app_out,
            Data_app_out_valid => data_app_out_valid,
            Data_app_out_start => data_app_out_start,
            Data_app_out_end => data_app_out_end,
            Data_app_out_err => data_app_out_err,
            Data_app_out_ready => data_app_out_ready
        );
END ARCHITECTURE;
