	.TITLE	Spare Time Gizmos VT1802 Fonts
	.SBTTL	Bob Armstrong [24-SEP-2024]

;     888     888 88888888888 d888   .d8888b.   .d8888b.   .d8888b.  
;     888     888     888    d8888  d88P  Y88b d88P  Y88b d88P  Y88b 
;     888     888     888      888  Y88b. d88P 888    888        888 
;     Y88b   d88P     888      888   "Y88888"  888    888      .d88P 
;      Y88b d88P      888      888  .d8P""Y8b. 888    888  .od888P"  
;       Y88o88P       888      888  888    888 888    888 d88P"      
;        Y888P        888      888  Y88b  d88P Y88b  d88P 888"       
;         Y8P         888    8888888 "Y8888P"   "Y8888P"  888888888  
;
;          Copyright (C) 2024 By Spare Time Gizmos, Milpitas CA.

;++
;   This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
; for more details.
;
;   You should have received a copy of the GNU General Public License along
; with this program; if not, write to the Free Software Foundation, Inc.,
; 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;--
;0000000001111111111222222222233333333334444444444555555555566666666667777777777
;1234567890123456789012345678901234567890123456789012345678901234567890123456789

	.MSFIRST \ .PAGE \ .CODES

;++
;   This file contains the fonts used by the Spare Time Gizmos VT1802 terminal.
; The hardware supports 7, 8 or 9 pixels per glyph, anywhere from 7 to 16
; scanlines per glyph, and 128 glyphs per font.  Unfortunately the 8275 only
; supports a 7 bit character code so it's only 128 characters per font, not 256.
;
;   Fonts are stored in a 2764 8K byte EPROM which can contain four fonts of
; 128 characters each.  EPROM address A10 is wired to the 8275 LC3 (scanline
; counter bit 3) output, and that means fonts with more than 8 scan lines are
; split into two parts - scanlines 1-8 first, and then scanlines 9-16 second.
; None of the current fonts actually use more than 8 scanlines, HOWEVER some
; video modes program the 8275 to display 9 scanlines so it's important that
; the second half of all these fonts be zero.
;
;   The two EPROM most significant address bits are connected to the 8275
; GPA0 and GPA1 outputs, which allow us to pick one of four fonts stored in
; the EPROM.
;
; EPROM		Font
; -----------	-----------------------------------------------------------
; $0000-$03FF	#1 - Spare Time Gizmos "standard" font
; $0400-$07FF	zero (font 1 scan lines 9-16)
; $0800-$0BFF	#2 - DEC VT52 font
; $0C00-$0FFF	zero (font 2 scan lines 9-16)
; $1000-$13FF	#3 - MICR style computer font
; $1400-$17FF	zero (font 3 scan lines 9-16)
; $1800-$1BFF	#4 - PC code page 437 font
; $1C00-$1FFF	zero (font 4 scan lines 9-16)
;
;   And lastly, this file is intended to be assembled with TASM just as with
; the VT1802 firmware.  It's normally assembled with the 1802 opcode table for
; convenience, but since this file only contains data statements the CPU 
; doesn't matter.
;--

	.SBTTL	Revision History

;++
; 001	-- Shift the VT52 font left one pixel so that it's not clipped in
;	    in the 7 pixels/glyph mode.
;--

	.SBTTL	Spare Time Gizmos Standard Font
	.ORG	$0000

; ASCII character 0x00 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x01 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$1C	;   ***   
	.BYTE	$14	;   * *   
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x02 ...
	.BYTE	$00	;         
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$0A	;  * *    
	.BYTE	$04	;   *     
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x03 ...
	.BYTE	$14	;   * *   
	.BYTE	$14	;   * *   
	.BYTE	$36	;  ** **  
	.BYTE	$00	;         
	.BYTE	$36	;  ** **  
	.BYTE	$14	;   * *   
	.BYTE	$14	;   * *   
	.BYTE	$00	;         

; ASCII character 0x04 ...
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$3E	;  *****  
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x05 ...
	.BYTE	$06	;  **     
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$3E	;  *****  
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x06 ...
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$3E	;  *****  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$06	;  **     
	.BYTE	$00	;         

; ASCII character 0x07 ...
	.BYTE	$08	;    *    
	.BYTE	$14	;   * *   
	.BYTE	$14	;   * *   
	.BYTE	$14	;   * *   
	.BYTE	$22	;  *   *  
	.BYTE	$3E	;  *****  
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x08 ...
	.BYTE	$10	;     *   
	.BYTE	$28	;    * *  
	.BYTE	$14	;   * *   
	.BYTE	$0A	;  * *    
	.BYTE	$14	;   * *   
	.BYTE	$28	;    * *  
	.BYTE	$10	;     *   
	.BYTE	$00	;         

; ASCII character 0x09 ...
	.BYTE	$04	;   *     
	.BYTE	$0A	;  * *    
	.BYTE	$14	;   * *   
	.BYTE	$28	;    * *  
	.BYTE	$14	;   * *   
	.BYTE	$0A	;  * *    
	.BYTE	$04	;   *     
	.BYTE	$00	;         

; ASCII character 0x0A ...
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$06	;  **     
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x0B ...
	.BYTE	$06	;  **     
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$06	;  **     
	.BYTE	$00	;         

; ASCII character 0x0C ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$20	;      *  
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x0D ...
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$0A	;  * *    
	.BYTE	$0A	;  * *    
	.BYTE	$1C	;   ***   
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x0E ...
	.BYTE	$06	;  **     
	.BYTE	$0A	;  * *    
	.BYTE	$12	;  *  *   
	.BYTE	$22	;  *   *  
	.BYTE	$12	;  *  *   
	.BYTE	$0A	;  * *    
	.BYTE	$06	;  **     
	.BYTE	$00	;         

; ASCII character 0x0F ...
	.BYTE	$06	;  **     
	.BYTE	$0E	;  ***    
	.BYTE	$1E	;  ****   
	.BYTE	$3E	;  *****  
	.BYTE	$1E	;  ****   
	.BYTE	$0E	;  ***    
	.BYTE	$06	;  **     
	.BYTE	$00	;         

; ASCII character 0x10 ...
	.BYTE	$30	;     **  
	.BYTE	$28	;    * *  
	.BYTE	$24	;   *  *  
	.BYTE	$22	;  *   *  
	.BYTE	$24	;   *  *  
	.BYTE	$28	;    * *  
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x11 ...
	.BYTE	$30	;     **  
	.BYTE	$38	;    ***  
	.BYTE	$3C	;   ****  
	.BYTE	$3E	;  *****  
	.BYTE	$3C	;   ****  
	.BYTE	$38	;    ***  
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x12 ...
	.BYTE	$00	;         
	.BYTE	$22	;  *   *  
	.BYTE	$36	;  ** **  
	.BYTE	$3E	;  *****  
	.BYTE	$36	;  ** **  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x13 ...
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x14 ...
	.BYTE	$20	;      *  
	.BYTE	$08	;    *    
	.BYTE	$02	;  *      
	.BYTE	$08	;    *    
	.BYTE	$20	;      *  
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x15 ...
	.BYTE	$02	;  *      
	.BYTE	$08	;    *    
	.BYTE	$20	;      *  
	.BYTE	$08	;    *    
	.BYTE	$02	;  *      
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x16 ...
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$3E	;  *****  
	.BYTE	$08	;    *    
	.BYTE	$3E	;  *****  
	.BYTE	$04	;   *     
	.BYTE	$02	;  *      
	.BYTE	$00	;         

; ASCII character 0x17 ...
	.BYTE	$38	;    ***  
	.BYTE	$28	;    * *  
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$0A	;  * *    
	.BYTE	$0E	;  ***    
	.BYTE	$00	;         

; ASCII character 0x18 ...
	.BYTE	$3E	;  *****  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x19 ...
	.BYTE	$3E	;  *****  
	.BYTE	$3E	;  *****  
	.BYTE	$3E	;  *****  
	.BYTE	$3E	;  *****  
	.BYTE	$3E	;  *****  
	.BYTE	$3E	;  *****  
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x1A ...
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$2A	;  * * *  
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1B ...
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$2A	;  * * *  
	.BYTE	$1C	;   ***   
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1C ...
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$3E	;  *****  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1D ...
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$3E	;  *****  
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1E ...
	.BYTE	$18	;    **   
	.BYTE	$24	;   *  *  
	.BYTE	$04	;   *     
	.BYTE	$1E	;  ****   
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x1F ...
	.BYTE	$0C	;   **    
	.BYTE	$12	;  *  *   
	.BYTE	$12	;  *  *   
	.BYTE	$0C	;   **    
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x20 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x21 ...
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x22 ...
	.BYTE	$14	;   * *   
	.BYTE	$14	;   * *   
	.BYTE	$14	;   * *   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x23 ...
	.BYTE	$14	;   * *   
	.BYTE	$14	;   * *   
	.BYTE	$3E	;  *****  
	.BYTE	$14	;   * *   
	.BYTE	$3E	;  *****  
	.BYTE	$14	;   * *   
	.BYTE	$14	;   * *   
	.BYTE	$00	;         

; ASCII character 0x24 ...
	.BYTE	$08	;    *    
	.BYTE	$3C	;   ****  
	.BYTE	$0A	;  * *    
	.BYTE	$1C	;   ***   
	.BYTE	$28	;    * *  
	.BYTE	$1E	;  ****   
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x25 ...
	.BYTE	$06	;  **     
	.BYTE	$26	;  **  *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$32	;  *  **  
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x26 ...
	.BYTE	$04	;   *     
	.BYTE	$0A	;  * *    
	.BYTE	$0A	;  * *    
	.BYTE	$04	;   *     
	.BYTE	$2A	;  * * *  
	.BYTE	$12	;  *  *   
	.BYTE	$2C	;   ** *  
	.BYTE	$00	;         

; ASCII character 0x27 ...
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x28 ...
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x29 ...
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x2A ...
	.BYTE	$08	;    *    
	.BYTE	$2A	;  * * *  
	.BYTE	$1C	;   ***   
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$2A	;  * * *  
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x2B ...
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$3E	;  *****  
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x2C ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     

; ASCII character 0x2D ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x2E ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x2F ...
	.BYTE	$00	;         
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$02	;  *      
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x30 ...
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$32	;  *  **  
	.BYTE	$2A	;  * * *  
	.BYTE	$26	;  **  *  
	.BYTE	$22	;  *   *  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x31 ...
	.BYTE	$08	;    *    
	.BYTE	$0C	;   **    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x32 ...
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$20	;      *  
	.BYTE	$1C	;   ***   
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x33 ...
	.BYTE	$3E	;  *****  
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$18	;    **   
	.BYTE	$20	;      *  
	.BYTE	$22	;  *   *  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x34 ...
	.BYTE	$10	;     *   
	.BYTE	$18	;    **   
	.BYTE	$14	;   * *   
	.BYTE	$12	;  *  *   
	.BYTE	$3E	;  *****  
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$00	;         

; ASCII character 0x35 ...
	.BYTE	$3E	;  *****  
	.BYTE	$02	;  *      
	.BYTE	$1E	;  ****   
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$22	;  *   *  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x36 ...
	.BYTE	$38	;    ***  
	.BYTE	$04	;   *     
	.BYTE	$02	;  *      
	.BYTE	$1E	;  ****   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x37 ...
	.BYTE	$3E	;  *****  
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$00	;         

; ASCII character 0x38 ...
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x39 ...
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3C	;   ****  
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$0E	;  ***    
	.BYTE	$00	;         

; ASCII character 0x3A ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x3B ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     

; ASCII character 0x3C ...
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$02	;  *      
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$00	;         

; ASCII character 0x3D ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x3E ...
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$00	;         

; ASCII character 0x3F ...
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$08	;    *    

; ASCII character 0x40 ...
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$2A	;  * * *  
	.BYTE	$3A	;  * ***  
	.BYTE	$1A	;  * **   
	.BYTE	$02	;  *      
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x41 ...
	.BYTE	$08	;    *    
	.BYTE	$14	;   * *   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3E	;  *****  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x42 ...
	.BYTE	$1E	;  ****   
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$1C	;   ***   
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$1E	;  ****   
	.BYTE	$00	;         

; ASCII character 0x43 ...
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$22	;  *   *  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x44 ...
	.BYTE	$1E	;  ****   
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$1E	;  ****   
	.BYTE	$00	;         

; ASCII character 0x45 ...
	.BYTE	$3E	;  *****  
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$0E	;  ***    
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x46 ...
	.BYTE	$3E	;  *****  
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$0E	;  ***    
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$00	;         

; ASCII character 0x47 ...
	.BYTE	$3C	;   ****  
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$3A	;  * ***  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x48 ...
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3E	;  *****  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x49 ...
	.BYTE	$1C	;   ***   
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x4A ...
	.BYTE	$70	;     *** 
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$22	;  *   *  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x4B ...
	.BYTE	$22	;  *   *  
	.BYTE	$12	;  *  *   
	.BYTE	$0A	;  * *    
	.BYTE	$06	;  **     
	.BYTE	$0A	;  * *    
	.BYTE	$12	;  *  *   
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x4C ...
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x4D ...
	.BYTE	$22	;  *   *  
	.BYTE	$36	;  ** **  
	.BYTE	$2A	;  * * *  
	.BYTE	$2A	;  * * *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x4E ...
	.BYTE	$22	;  *   *  
	.BYTE	$26	;  **  *  
	.BYTE	$2A	;  * * *  
	.BYTE	$32	;  *  **  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x4F ...
	.BYTE	$3E	;  *****  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x50 ...
	.BYTE	$1E	;  ****   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$1E	;  ****   
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$00	;         

; ASCII character 0x51 ...
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$2A	;  * * *  
	.BYTE	$12	;  *  *   
	.BYTE	$2C	;   ** *  
	.BYTE	$00	;         

; ASCII character 0x52 ...
	.BYTE	$1E	;  ****   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$1E	;  ****   
	.BYTE	$0A	;  * *    
	.BYTE	$12	;  *  *   
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x53 ...
	.BYTE	$3C	;   ****  
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$1C	;   ***   
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$1E	;  ****   
	.BYTE	$00	;         

; ASCII character 0x54 ...
	.BYTE	$3E	;  *****  
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x55 ...
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x56 ...
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$14	;   * *   
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x57 ...
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$2A	;  * * *  
	.BYTE	$36	;  ** **  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x58 ...
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$14	;   * *   
	.BYTE	$08	;    *    
	.BYTE	$14	;   * *   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x59 ...
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$14	;   * *   
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x5A ...
	.BYTE	$3E	;  *****  
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$02	;  *      
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x5B ...
	.BYTE	$1C	;   ***   
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x5C ...
	.BYTE	$00	;         
	.BYTE	$02	;  *      
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$20	;      *  
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x5D ...
	.BYTE	$38	;    ***  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$38	;    ***  
	.BYTE	$00	;         

; ASCII character 0x5E ...
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$2A	;  * * *  
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x5F ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 

; ASCII character 0x60 ...
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x61 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3C	;   ****  
	.BYTE	$20	;      *  
	.BYTE	$3C	;   ****  
	.BYTE	$22	;  *   *  
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x62 ...
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$1A	;  * **   
	.BYTE	$26	;  **  *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$1E	;  ****   
	.BYTE	$00	;         

; ASCII character 0x63 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$38	;    ***  
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$38	;    ***  
	.BYTE	$00	;         

; ASCII character 0x64 ...
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$2C	;   ** *  
	.BYTE	$32	;  *  **  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x65 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$3E	;  *****  
	.BYTE	$02	;  *      
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x66 ...
	.BYTE	$18	;    **   
	.BYTE	$24	;   *  *  
	.BYTE	$04	;   *     
	.BYTE	$0E	;  ***    
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$00	;         

; ASCII character 0x67 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3C	;   ****  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3C	;   ****  
	.BYTE	$20	;      *  
	.BYTE	$3C	;   ****  

; ASCII character 0x68 ...
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$1A	;  * **   
	.BYTE	$26	;  **  *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x69 ...
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$00	;         

; ASCII character 0x6A ...
	.BYTE	$20	;      *  
	.BYTE	$00	;         
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$18	;    **   

; ASCII character 0x6B ...
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$22	;  *   *  
	.BYTE	$12	;  *  *   
	.BYTE	$0A	;  * *    
	.BYTE	$16	;  ** *   
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x6C ...
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$00	;         

; ASCII character 0x6D ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$36	;  ** **  
	.BYTE	$2A	;  * * *  
	.BYTE	$2A	;  * * *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x6E ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$1A	;  * **   
	.BYTE	$26	;  **  *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x6F ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x70 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$1E	;  ****   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$1E	;  ****   
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      

; ASCII character 0x71 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$1C	;   ***   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3C	;   ****  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  

; ASCII character 0x72 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$1A	;  * **   
	.BYTE	$26	;  **  *  
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$00	;         

; ASCII character 0x73 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$38	;    ***  
	.BYTE	$04	;   *     
	.BYTE	$18	;    **   
	.BYTE	$20	;      *  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x74 ...
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$00	;         

; ASCII character 0x75 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$32	;  *  **  
	.BYTE	$4C	;   **  * 
	.BYTE	$00	;         

; ASCII character 0x76 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$14	;   * *   
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x77 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$2A	;  * * *  
	.BYTE	$2A	;  * * *  
	.BYTE	$14	;   * *   
	.BYTE	$00	;         

; ASCII character 0x78 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$22	;  *   *  
	.BYTE	$14	;   * *   
	.BYTE	$08	;    *    
	.BYTE	$14	;   * *   
	.BYTE	$22	;  *   *  
	.BYTE	$00	;         

; ASCII character 0x79 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3C	;   ****  
	.BYTE	$20	;      *  
	.BYTE	$38	;    ***  

; ASCII character 0x7A ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x7B ...
	.BYTE	$30	;     **  
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$06	;  **     
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x7C ...
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x7D ...
	.BYTE	$0C	;   **    
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$60	;      ** 
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$0C	;   **    
	.BYTE	$00	;         

; ASCII character 0x7E ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$04	;   *     
	.BYTE	$2A	;  * * *  
	.BYTE	$10	;     *   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x7F ...
	.BYTE	$2A	;  * * *  
	.BYTE	$14	;   * *   
	.BYTE	$2A	;  * * *  
	.BYTE	$14	;   * *   
	.BYTE	$2A	;  * * *  
	.BYTE	$14	;   * *   
	.BYTE	$2A	;  * * *  
	.BYTE	$00	;         

	.SBTTL	Font 1 Scanlines 9-16
	.ORG	$0400

;++
; These are unused, and should always be zero!
;--
	.FILL	$400, 0

	.SBTTL	DEC VT52 Font
	.ORG	$0800

; ASCII character 0x00 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x01 ...
	.BYTE	$00 >> 1;         
	.BYTE	$0C >> 1;   **    
	.BYTE	$02 >> 1;  *      
	.BYTE	$82 >> 1;  *     *
	.BYTE	$8C >> 1;   **   *
	.BYTE	$E0 >> 1;      ***
	.BYTE	$90 >> 1;     *  *
	.BYTE	$E0 >> 1;      ***

; ASCII character 0x02 ...
	.BYTE	$FE >> 1;  *******
	.BYTE	$FE >> 1;  *******
	.BYTE	$FE >> 1;  *******
	.BYTE	$FE >> 1;  *******
	.BYTE	$FE >> 1;  *******
	.BYTE	$FE >> 1;  *******
	.BYTE	$FE >> 1;  *******
	.BYTE	$FE >> 1;  *******

; ASCII character 0x03 ...
	.BYTE	$00 >> 1;         
	.BYTE	$84 >> 1;   *    *
	.BYTE	$86 >> 1;  **    *
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$2E >> 1;  *** *  
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   

; ASCII character 0x04 ...
	.BYTE	$00 >> 1;         
	.BYTE	$8E >> 1;  ***   *
	.BYTE	$90 >> 1;     *  *
	.BYTE	$4C >> 1;   **  * 
	.BYTE	$50 >> 1;     * * 
	.BYTE	$2E >> 1;  *** *  
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   

; ASCII character 0x05 ...
	.BYTE	$00 >> 1;         
	.BYTE	$9E >> 1;  ****  *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$4E >> 1;  ***  * 
	.BYTE	$50 >> 1;     * * 
	.BYTE	$2E >> 1;  *** *  
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   

; ASCII character 0x06 ...
	.BYTE	$00 >> 1;         
	.BYTE	$9E >> 1;  ****  *
	.BYTE	$90 >> 1;     *  *
	.BYTE	$48 >> 1;    *  * 
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$22 >> 1;  *   *  
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   

; ASCII character 0x07 ...
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$24 >> 1;   *  *  
	.BYTE	$18 >> 1;    **   
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x08 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$FE >> 1;  *******
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$FE >> 1;  *******

; ASCII character 0x09 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$20 >> 1;      *  
	.BYTE	$40 >> 1;       * 
	.BYTE	$FE >> 1;  *******
	.BYTE	$40 >> 1;       * 
	.BYTE	$20 >> 1;      *  
	.BYTE	$00 >> 1;         

; ASCII character 0x0A ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$92 >> 1;  *  *  *

; ASCII character 0x0B ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$00 >> 1;         

; ASCII character 0x0C ...
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$54 >> 1;   * * * 
	.BYTE	$38 >> 1;    ***  
	.BYTE	$10 >> 1;     *   
	.BYTE	$00 >> 1;         

; ASCII character 0x0D ...
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x0E ...
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x0F ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x10 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x11 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x12 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x13 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         

; ASCII character 0x14 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******

; ASCII character 0x15 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$0C >> 1;   **    
	.BYTE	$12 >> 1;  *  *   
	.BYTE	$12 >> 1;  *  *   
	.BYTE	$12 >> 1;  *  *   
	.BYTE	$0C >> 1;   **    

; ASCII character 0x16 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$04 >> 1;   *     
	.BYTE	$06 >> 1;  **     
	.BYTE	$04 >> 1;   *     
	.BYTE	$04 >> 1;   *     
	.BYTE	$0E >> 1;  ***    

; ASCII character 0x17 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$0E >> 1;  ***    
	.BYTE	$10 >> 1;     *   
	.BYTE	$0C >> 1;   **    
	.BYTE	$02 >> 1;  *      
	.BYTE	$1E >> 1;  ****   

; ASCII character 0x18 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$0E >> 1;  ***    
	.BYTE	$10 >> 1;     *   
	.BYTE	$0C >> 1;   **    
	.BYTE	$10 >> 1;     *   
	.BYTE	$0E >> 1;  ***    

; ASCII character 0x19 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$08 >> 1;    *    
	.BYTE	$0C >> 1;   **    
	.BYTE	$0A >> 1;  * *    
	.BYTE	$1E >> 1;  ****   
	.BYTE	$08 >> 1;    *    

; ASCII character 0x1A ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$1E >> 1;  ****   
	.BYTE	$02 >> 1;  *      
	.BYTE	$0E >> 1;  ***    
	.BYTE	$10 >> 1;     *   
	.BYTE	$0E >> 1;  ***    

; ASCII character 0x1B ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$1C >> 1;   ***   
	.BYTE	$02 >> 1;  *      
	.BYTE	$0E >> 1;  ***    
	.BYTE	$12 >> 1;  *  *   
	.BYTE	$0C >> 1;   **    

; ASCII character 0x1C ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$1E >> 1;  ****   
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    
	.BYTE	$04 >> 1;   *     
	.BYTE	$02 >> 1;  *      

; ASCII character 0x1D ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$0C >> 1;   **    
	.BYTE	$12 >> 1;  *  *   
	.BYTE	$0C >> 1;   **    
	.BYTE	$12 >> 1;  *  *   
	.BYTE	$0C >> 1;   **    

; ASCII character 0x1E ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$0C >> 1;   **    
	.BYTE	$12 >> 1;  *  *   
	.BYTE	$1C >> 1;   ***   
	.BYTE	$10 >> 1;     *   
	.BYTE	$0E >> 1;  ***    

; ASCII character 0x1F ...
	.BYTE	$00 >> 1;         
	.BYTE	$FC >> 1;   ******
	.BYTE	$5E >> 1;  **** * 
	.BYTE	$5E >> 1;  **** * 
	.BYTE	$5C >> 1;   *** * 
	.BYTE	$50 >> 1;     * * 
	.BYTE	$50 >> 1;     * * 
	.BYTE	$50 >> 1;     * * 

; ASCII character 0x20 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x21 ...
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   

; ASCII character 0x22 ...
	.BYTE	$00 >> 1;         
	.BYTE	$28 >> 1;    * *  
	.BYTE	$28 >> 1;    * *  
	.BYTE	$28 >> 1;    * *  
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x23 ...
	.BYTE	$00 >> 1;         
	.BYTE	$28 >> 1;    * *  
	.BYTE	$28 >> 1;    * *  
	.BYTE	$FE >> 1;  *******
	.BYTE	$28 >> 1;    * *  
	.BYTE	$FE >> 1;  *******
	.BYTE	$28 >> 1;    * *  
	.BYTE	$28 >> 1;    * *  

; ASCII character 0x24 ...
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$12 >> 1;  *  *   
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$90 >> 1;     *  *
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$10 >> 1;     *   

; ASCII character 0x25 ...
	.BYTE	$00 >> 1;         
	.BYTE	$86 >> 1;  **    *
	.BYTE	$46 >> 1;  **   * 
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    
	.BYTE	$C4 >> 1;   *   **
	.BYTE	$C2 >> 1;  *    **

; ASCII character 0x26 ...
	.BYTE	$00 >> 1;         
	.BYTE	$38 >> 1;    ***  
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$28 >> 1;    * *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$A8 >> 1;    * * *
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$B8 >> 1;    *** *

; ASCII character 0x27 ...
	.BYTE	$00 >> 1;         
	.BYTE	$30 >> 1;     **  
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x28 ...
	.BYTE	$00 >> 1;         
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    
	.BYTE	$08 >> 1;    *    
	.BYTE	$08 >> 1;    *    
	.BYTE	$10 >> 1;     *   
	.BYTE	$20 >> 1;      *  

; ASCII character 0x29 ...
	.BYTE	$00 >> 1;         
	.BYTE	$08 >> 1;    *    
	.BYTE	$10 >> 1;     *   
	.BYTE	$20 >> 1;      *  
	.BYTE	$20 >> 1;      *  
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    

; ASCII character 0x2A ...
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$54 >> 1;   * * * 
	.BYTE	$38 >> 1;    ***  
	.BYTE	$54 >> 1;   * * * 
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$10 >> 1;     *   

; ASCII character 0x2B ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$FE >> 1;  *******
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$00 >> 1;         

; ASCII character 0x2C ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$08 >> 1;    *    
	.BYTE	$04 >> 1;   *     

; ASCII character 0x2D ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x2E ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$18 >> 1;    **   

; ASCII character 0x2F ...
	.BYTE	$00 >> 1;         
	.BYTE	$80 >> 1;        *
	.BYTE	$40 >> 1;       * 
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    
	.BYTE	$04 >> 1;   *     
	.BYTE	$02 >> 1;  *      

; ASCII character 0x30 ...
	.BYTE	$00 >> 1;         
	.BYTE	$38 >> 1;    ***  
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$A2 >> 1;  *   * *
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$8A >> 1;  * *   *
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$38 >> 1;    ***  

; ASCII character 0x31 ...
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$18 >> 1;    **   
	.BYTE	$14 >> 1;   * *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x32 ...
	.BYTE	$00 >> 1;         
	.BYTE	$3C >> 1;   ****  
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$80 >> 1;        *
	.BYTE	$70 >> 1;     *** 
	.BYTE	$0C >> 1;   **    
	.BYTE	$02 >> 1;  *      
	.BYTE	$FE >> 1;  *******

; ASCII character 0x33 ...
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$40 >> 1;       * 
	.BYTE	$20 >> 1;      *  
	.BYTE	$70 >> 1;     *** 
	.BYTE	$80 >> 1;        *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x34 ...
	.BYTE	$00 >> 1;         
	.BYTE	$20 >> 1;      *  
	.BYTE	$30 >> 1;     **  
	.BYTE	$28 >> 1;    * *  
	.BYTE	$24 >> 1;   *  *  
	.BYTE	$FE >> 1;  *******
	.BYTE	$20 >> 1;      *  
	.BYTE	$20 >> 1;      *  

; ASCII character 0x35 ...
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$02 >> 1;  *      
	.BYTE	$7A >> 1;  * **** 
	.BYTE	$86 >> 1;  **    *
	.BYTE	$80 >> 1;        *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x36 ...
	.BYTE	$00 >> 1;         
	.BYTE	$78 >> 1;    **** 
	.BYTE	$84 >> 1;   *    *
	.BYTE	$02 >> 1;  *      
	.BYTE	$7A >> 1;  * **** 
	.BYTE	$86 >> 1;  **    *
	.BYTE	$84 >> 1;   *    *
	.BYTE	$78 >> 1;    **** 

; ASCII character 0x37 ...
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$80 >> 1;        *
	.BYTE	$40 >> 1;       * 
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    
	.BYTE	$04 >> 1;   *     

; ASCII character 0x38 ...
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x39 ...
	.BYTE	$00 >> 1;         
	.BYTE	$3C >> 1;   ****  
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$C2 >> 1;  *    **
	.BYTE	$BC >> 1;   **** *
	.BYTE	$80 >> 1;        *
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$3C >> 1;   ****  

; ASCII character 0x3A ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$18 >> 1;    **   
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$18 >> 1;    **   

; ASCII character 0x3B ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$18 >> 1;    **   
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$08 >> 1;    *    
	.BYTE	$04 >> 1;   *     

; ASCII character 0x3C ...
	.BYTE	$00 >> 1;         
	.BYTE	$40 >> 1;       * 
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    
	.BYTE	$10 >> 1;     *   
	.BYTE	$20 >> 1;      *  
	.BYTE	$40 >> 1;       * 

; ASCII character 0x3D ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x3E ...
	.BYTE	$00 >> 1;         
	.BYTE	$04 >> 1;   *     
	.BYTE	$08 >> 1;    *    
	.BYTE	$10 >> 1;     *   
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    
	.BYTE	$04 >> 1;   *     

; ASCII character 0x3F ...
	.BYTE	$00	;         
	.BYTE	$3E	;   ***** 
	.BYTE	$41	;  *     *
	.BYTE	$40	;        *
	.BYTE	$38	;     *** 
	.BYTE	$08	;     *   
	.BYTE	$00	;         
	.BYTE	$08	;     *   

; ASCII character 0x40 ...
	.BYTE	$00	;         
	.BYTE	$3E	;   ***** 
	.BYTE	$41	;  *     *
	.BYTE	$51	;  *   * *
	.BYTE	$49	;  *  *  *
	.BYTE	$39	;  *  *** 
	.BYTE	$01	;  *      
	.BYTE	$3E	;   ***** 

; ASCII character 0x41 ...
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$28 >> 1;    * *  
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$FE >> 1;  *******
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x42 ...
	.BYTE	$00 >> 1;         
	.BYTE	$7E >> 1;  ****** 
	.BYTE	$84 >> 1;   *    *
	.BYTE	$84 >> 1;   *    *
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$84 >> 1;   *    *
	.BYTE	$84 >> 1;   *    *
	.BYTE	$7E >> 1;  ****** 

; ASCII character 0x43 ...
	.BYTE	$00 >> 1;         
	.BYTE	$78 >> 1;    **** 
	.BYTE	$84 >> 1;   *    *
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$84 >> 1;   *    *
	.BYTE	$78 >> 1;    **** 

; ASCII character 0x44 ...
	.BYTE	$00 >> 1;         
	.BYTE	$7E >> 1;  ****** 
	.BYTE	$84 >> 1;   *    *
	.BYTE	$84 >> 1;   *    *
	.BYTE	$84 >> 1;   *    *
	.BYTE	$84 >> 1;   *    *
	.BYTE	$84 >> 1;   *    *
	.BYTE	$7E >> 1;  ****** 

; ASCII character 0x45 ...
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$1E >> 1;  ****   
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$FE >> 1;  *******

; ASCII character 0x46 ...
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$1E >> 1;  ****   
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      

; ASCII character 0x47 ...
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$02 >> 1;  *      
	.BYTE	$E2 >> 1;  *   ***
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x48 ...
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$FE >> 1;  *******
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x49 ...
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x4A ...
	.BYTE	$00 >> 1;         
	.BYTE	$80 >> 1;        *
	.BYTE	$80 >> 1;        *
	.BYTE	$80 >> 1;        *
	.BYTE	$80 >> 1;        *
	.BYTE	$80 >> 1;        *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x4B ...
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$62 >> 1;  *   ** 
	.BYTE	$1A >> 1;  * **   
	.BYTE	$06 >> 1;  **     
	.BYTE	$1A >> 1;  * **   
	.BYTE	$62 >> 1;  *   ** 
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x4C ...
	.BYTE	$00 >> 1;         
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$FE >> 1;  *******

; ASCII character 0x4D ...
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$C6 >> 1;  **   **
	.BYTE	$AA >> 1;  * * * *
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x4E ...
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$86 >> 1;  **    *
	.BYTE	$8A >> 1;  * *   *
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$A2 >> 1;  *   * *
	.BYTE	$C2 >> 1;  *    **
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x4F ...
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x50 ...
	.BYTE	$00 >> 1;         
	.BYTE	$7E >> 1;  ****** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7E >> 1;  ****** 
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      

; ASCII character 0x51 ...
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$A2 >> 1;  *   * *
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$BC >> 1;   **** *

; ASCII character 0x52 ...
	.BYTE	$00 >> 1;         
	.BYTE	$7E >> 1;  ****** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7E >> 1;  ****** 
	.BYTE	$22 >> 1;  *   *  
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x53 ...
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$02 >> 1;  *      
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$80 >> 1;        *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x54 ...
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   

; ASCII character 0x55 ...
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x56 ...
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$28 >> 1;    * *  
	.BYTE	$28 >> 1;    * *  
	.BYTE	$10 >> 1;     *   

; ASCII character 0x57 ...
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$AA >> 1;  * * * *
	.BYTE	$44 >> 1;   *   * 

; ASCII character 0x58 ...
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$28 >> 1;    * *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$28 >> 1;    * *  
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x59 ...
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$28 >> 1;    * *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   

; ASCII character 0x5A ...
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$40 >> 1;       * 
	.BYTE	$20 >> 1;      *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$08 >> 1;    *    
	.BYTE	$04 >> 1;   *     
	.BYTE	$FE >> 1;  *******

; ASCII character 0x5B ...
	.BYTE	$00 >> 1;         
	.BYTE	$3C >> 1;   ****  
	.BYTE	$0C >> 1;   **    
	.BYTE	$0C >> 1;   **    
	.BYTE	$0C >> 1;   **    
	.BYTE	$0C >> 1;   **    
	.BYTE	$0C >> 1;   **    
	.BYTE	$3C >> 1;   ****  

; ASCII character 0x5C ...
	.BYTE	$00 >> 1;         
	.BYTE	$02 >> 1;  *      
	.BYTE	$04 >> 1;   *     
	.BYTE	$08 >> 1;    *    
	.BYTE	$10 >> 1;     *   
	.BYTE	$20 >> 1;      *  
	.BYTE	$40 >> 1;       * 
	.BYTE	$80 >> 1;        *

; ASCII character 0x5D ...
	.BYTE	$00 >> 1;         
	.BYTE	$78 >> 1;    **** 
	.BYTE	$60 >> 1;      ** 
	.BYTE	$60 >> 1;      ** 
	.BYTE	$60 >> 1;      ** 
	.BYTE	$60 >> 1;      ** 
	.BYTE	$60 >> 1;      ** 
	.BYTE	$78 >> 1;    **** 

; ASCII character 0x5E ...
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$28 >> 1;    * *  
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x5F ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******

; ASCII character 0x60 ...
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$10 >> 1;     *   
	.BYTE	$20 >> 1;      *  
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x61 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$80 >> 1;        *
	.BYTE	$FC >> 1;   ******
	.BYTE	$82 >> 1;  *     *
	.BYTE	$FC >> 1;   ******

; ASCII character 0x62 ...
	.BYTE	$00 >> 1;         
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$7A >> 1;  * **** 
	.BYTE	$86 >> 1;  **    *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$86 >> 1;  **    *
	.BYTE	$7A >> 1;  * **** 

; ASCII character 0x63 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$FC >> 1;   ******

; ASCII character 0x64 ...
	.BYTE	$00 >> 1;         
	.BYTE	$80 >> 1;        *
	.BYTE	$80 >> 1;        *
	.BYTE	$BC >> 1;   **** *
	.BYTE	$C2 >> 1;  *    **
	.BYTE	$82 >> 1;  *     *
	.BYTE	$C2 >> 1;  *    **
	.BYTE	$BC >> 1;   **** *

; ASCII character 0x65 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$3C >> 1;   ****  
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$FE >> 1;  *******
	.BYTE	$02 >> 1;  *      
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x66 ...
	.BYTE	$00 >> 1;         
	.BYTE	$70 >> 1;     *** 
	.BYTE	$88 >> 1;    *   *
	.BYTE	$3E >> 1;  *****  
	.BYTE	$08 >> 1;    *    
	.BYTE	$08 >> 1;    *    
	.BYTE	$08 >> 1;    *    
	.BYTE	$08 >> 1;    *    

; ASCII character 0x67 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$B8 >> 1;    *** *
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$78 >> 1;    **** 
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$3C >> 1;   ****  

; ASCII character 0x68 ...
	.BYTE	$00 >> 1;         
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$7E >> 1;  ****** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x69 ...
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x6A ...
	.BYTE	$00 >> 1;         
	.BYTE	$80 >> 1;        *
	.BYTE	$00 >> 1;         
	.BYTE	$80 >> 1;        *
	.BYTE	$80 >> 1;        *
	.BYTE	$80 >> 1;        *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x6B ...
	.BYTE	$00 >> 1;         
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      
	.BYTE	$22 >> 1;  *   *  
	.BYTE	$12 >> 1;  *  *   
	.BYTE	$0A >> 1;  * *    
	.BYTE	$22 >> 1;  *   *  
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x6C ...
	.BYTE	$00 >> 1;         
	.BYTE	$18 >> 1;    **   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$38 >> 1;    ***  

; ASCII character 0x6D ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$6E >> 1;  *** ** 
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$92 >> 1;  *  *  *

; ASCII character 0x6E ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$7A >> 1;  * **** 
	.BYTE	$86 >> 1;  **    *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *

; ASCII character 0x6F ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$7C >> 1;   ***** 

; ASCII character 0x70 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$7A >> 1;  * **** 
	.BYTE	$86 >> 1;  **    *
	.BYTE	$7E >> 1;  ****** 
	.BYTE	$02 >> 1;  *      
	.BYTE	$02 >> 1;  *      

; ASCII character 0x71 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$BC >> 1;   **** *
	.BYTE	$C2 >> 1;  *    **
	.BYTE	$FC >> 1;   ******
	.BYTE	$80 >> 1;        *
	.BYTE	$80 >> 1;        *

; ASCII character 0x72 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$72 >> 1;  *  *** 
	.BYTE	$8C >> 1;   **   *
	.BYTE	$04 >> 1;   *     
	.BYTE	$04 >> 1;   *     
	.BYTE	$04 >> 1;   *     

; ASCII character 0x73 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$02 >> 1;  *      
	.BYTE	$7C >> 1;   ***** 
	.BYTE	$80 >> 1;        *
	.BYTE	$7E >> 1;  ****** 

; ASCII character 0x74 ...
	.BYTE	$00 >> 1;         
	.BYTE	$08 >> 1;    *    
	.BYTE	$08 >> 1;    *    
	.BYTE	$3E >> 1;  *****  
	.BYTE	$08 >> 1;    *    
	.BYTE	$08 >> 1;    *    
	.BYTE	$48 >> 1;    *  * 
	.BYTE	$30 >> 1;     **  

; ASCII character 0x75 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$BC >> 1;   **** *

; ASCII character 0x76 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$28 >> 1;    * *  
	.BYTE	$10 >> 1;     *   

; ASCII character 0x77 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$82 >> 1;  *     *
	.BYTE	$92 >> 1;  *  *  *
	.BYTE	$AA >> 1;  * * * *
	.BYTE	$44 >> 1;   *   * 

; ASCII character 0x78 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$42 >> 1;  *    * 
	.BYTE	$24 >> 1;   *  *  
	.BYTE	$18 >> 1;    **   
	.BYTE	$24 >> 1;   *  *  
	.BYTE	$42 >> 1;  *    * 

; ASCII character 0x79 ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$82 >> 1;  *     *
	.BYTE	$44 >> 1;   *   * 
	.BYTE	$28 >> 1;    * *  
	.BYTE	$10 >> 1;     *   
	.BYTE	$0E >> 1;  ***    

; ASCII character 0x7A ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$FE >> 1;  *******
	.BYTE	$40 >> 1;       * 
	.BYTE	$38 >> 1;    ***  
	.BYTE	$04 >> 1;   *     
	.BYTE	$FE >> 1;  *******

; ASCII character 0x7B ...
	.BYTE	$00 >> 1;         
	.BYTE	$E0 >> 1;      ***
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$0E >> 1;  ***    
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$E0 >> 1;      ***

; ASCII character 0x7C ...
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$00 >> 1;         
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   

; ASCII character 0x7D ...
	.BYTE	$00 >> 1;         
	.BYTE	$0E >> 1;  ***    
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$E0 >> 1;      ***
	.BYTE	$10 >> 1;     *   
	.BYTE	$10 >> 1;     *   
	.BYTE	$0E >> 1;  ***    

; ASCII character 0x7E ...
	.BYTE	$00 >> 1;         
	.BYTE	$88 >> 1;    *   *
	.BYTE	$54 >> 1;   * * * 
	.BYTE	$22 >> 1;  *   *  
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

; ASCII character 0x7F ...
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         
	.BYTE	$00 >> 1;         

	.SBTTL	Font 2 Scanlines 9-16
	.ORG	$0C00

;++
; These are unused, and should always be zero!
;--
	.FILL	$400, 0

	.SBTTL	MICR Style Computer Font
	.ORG	$1000

; ASCII character 0x00 ...
	.BYTE	$54	;   * * * 
	.BYTE	$AA	;  * * * *
	.BYTE	$54	;   * * * 
	.BYTE	$AA	;  * * * *
	.BYTE	$54	;   * * * 
	.BYTE	$AA	;  * * * *
	.BYTE	$54	;   * * * 
	.BYTE	$AA	;  * * * *

; ASCII character 0x01 ...
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 
	.BYTE	$03	; **      
	.BYTE	$03	; **      
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   

; ASCII character 0x02 ...
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$26	;  **  *  
	.BYTE	$0F	; ****    
	.BYTE	$06	;  **     
	.BYTE	$66	;  **  ** 
	.BYTE	$3F	; ******  
	.BYTE	$00	;         

; ASCII character 0x03 ...
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   

; ASCII character 0x04 ...
	.BYTE	$1F	; *****   
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$5F	; ***** * 
	.BYTE	$63	; **   ** 
	.BYTE	$F3	; **  ****
	.BYTE	$63	; **   ** 
	.BYTE	$E3	; **   ***

; ASCII character 0x05 ...
	.BYTE	$70	;     *** 
	.BYTE	$D8	;    ** **
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$1B	; ** **   
	.BYTE	$0E	;  ***    
	.BYTE	$00	;         

; ASCII character 0x06 ...
	.BYTE	$00	;         
	.BYTE	$CC	;   **  **
	.BYTE	$66	;  **  ** 
	.BYTE	$33	; **  **  
	.BYTE	$66	;  **  ** 
	.BYTE	$CC	;   **  **
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x07 ...
	.BYTE	$00	;         
	.BYTE	$33	; **  **  
	.BYTE	$66	;  **  ** 
	.BYTE	$CC	;   **  **
	.BYTE	$66	;  **  ** 
	.BYTE	$33	; **  **  
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x08 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7F	; ******* 
	.BYTE	$36	;  ** **  
	.BYTE	$36	;  ** **  
	.BYTE	$36	;  ** **  
	.BYTE	$36	;  ** **  
	.BYTE	$00	;         

; ASCII character 0x09 ...
	.BYTE	$7F	; ******* 
	.BYTE	$63	; **   ** 
	.BYTE	$06	;  **     
	.BYTE	$0C	;   **    
	.BYTE	$06	;  **     
	.BYTE	$63	; **   ** 
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         

; ASCII character 0x0A ...
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 

; ASCII character 0x0B ...
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$63	; **   ** 
	.BYTE	$7F	; ******* 
	.BYTE	$63	; **   ** 
	.BYTE	$36	;  ** **  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x0C ...
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$36	;  ** **  
	.BYTE	$36	;  ** **  
	.BYTE	$77	; *** *** 
	.BYTE	$00	;         

; ASCII character 0x0D ...
	.BYTE	$70	;     *** 
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$7C	;   ***** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x0E ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$DB	; ** ** **
	.BYTE	$DB	; ** ** **
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x0F ...
	.BYTE	$60	;      ** 
	.BYTE	$30	;     **  
	.BYTE	$7E	;  ****** 
	.BYTE	$DB	; ** ** **
	.BYTE	$DB	; ** ** **
	.BYTE	$7E	;  ****** 
	.BYTE	$06	;  **     
	.BYTE	$03	; **      

; ASCII character 0x10 ...
	.BYTE	$78	;    **** 
	.BYTE	$0C	;   **    
	.BYTE	$06	;  **     
	.BYTE	$7E	;  ****** 
	.BYTE	$06	;  **     
	.BYTE	$0C	;   **    
	.BYTE	$78	;    **** 
	.BYTE	$00	;         

; ASCII character 0x11 ...
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$00	;         

; ASCII character 0x12 ...
	.BYTE	$00	;         
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x13 ...
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x14 ...
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x15 ...
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x16 ...
	.BYTE	$70	;     *** 
	.BYTE	$D8	;    ** **
	.BYTE	$D8	;    ** **
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   

; ASCII character 0x17 ...
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$1B	; ** **   
	.BYTE	$1B	; ** **   
	.BYTE	$0E	;  ***    

; ASCII character 0x18 ...
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x19 ...
	.BYTE	$00	;         
	.BYTE	$6E	;  *** ** 
	.BYTE	$3B	; ** ***  
	.BYTE	$00	;         
	.BYTE	$6E	;  *** ** 
	.BYTE	$3B	; ** ***  
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1A ...
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$36	;  ** **  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1B ...
	.BYTE	$F0	;     ****
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$37	; *** **  
	.BYTE	$36	;  ** **  
	.BYTE	$3C	;   ****  
	.BYTE	$38	;    ***  

; ASCII character 0x1C ...
	.BYTE	$36	;  ** **  
	.BYTE	$6C	;   ** ** 
	.BYTE	$6C	;   ** ** 
	.BYTE	$6C	;   ** ** 
	.BYTE	$6C	;   ** ** 
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1D ...
	.BYTE	$98	;    **  *
	.BYTE	$CC	;   **  **
	.BYTE	$66	;  **  ** 
	.BYTE	$32	;  *  **  
	.BYTE	$98	;    **  *
	.BYTE	$CC	;   **  **
	.BYTE	$66	;  **  ** 
	.BYTE	$32	;  *  **  

; ASCII character 0x1E ...
	.BYTE	$00	;         
	.BYTE	$3C	;   ****  
	.BYTE	$7E	;  ****** 
	.BYTE	$7E	;  ****** 
	.BYTE	$7E	;  ****** 
	.BYTE	$7E	;  ****** 
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x1F ...
	.BYTE	$10	;     *   
	.BYTE	$38	;    ***  
	.BYTE	$7C	;   ***** 
	.BYTE	$FE	;  *******
	.BYTE	$7C	;   ***** 
	.BYTE	$38	;    ***  
	.BYTE	$10	;     *   
	.BYTE	$00	;         

; ASCII character 0x20 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x21 ...
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$00	;         
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x22 ...
	.BYTE	$36	;  ** **  
	.BYTE	$24	;   *  *  
	.BYTE	$36	;  ** **  
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x23 ...
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$7E	;  ****** 
	.BYTE	$24	;   *  *  
	.BYTE	$7E	;  ****** 
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$00	;         

; ASCII character 0x24 ...
	.BYTE	$10	;     *   
	.BYTE	$7C	;   ***** 
	.BYTE	$04	;   *     
	.BYTE	$7C	;   ***** 
	.BYTE	$60	;      ** 
	.BYTE	$7C	;   ***** 
	.BYTE	$10	;     *   
	.BYTE	$00	;         

; ASCII character 0x25 ...
	.BYTE	$00	;         
	.BYTE	$46	;  **   * 
	.BYTE	$26	;  **  *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$64	;   *  ** 
	.BYTE	$62	;  *   ** 
	.BYTE	$00	;         

; ASCII character 0x26 ...
	.BYTE	$3C	;   ****  
	.BYTE	$04	;   *     
	.BYTE	$24	;   *  *  
	.BYTE	$7E	;  ****** 
	.BYTE	$26	;  **  *  
	.BYTE	$26	;  **  *  
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x27 ...
	.BYTE	$38	;    ***  
	.BYTE	$18	;    **   
	.BYTE	$08	;    *    
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x28 ...
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$20	;      *  
	.BYTE	$00	;         

; ASCII character 0x29 ...
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$00	;         

; ASCII character 0x2A ...
	.BYTE	$10	;     *   
	.BYTE	$54	;   * * * 
	.BYTE	$38	;    ***  
	.BYTE	$7C	;   ***** 
	.BYTE	$38	;    ***  
	.BYTE	$54	;   * * * 
	.BYTE	$10	;     *   
	.BYTE	$00	;         

; ASCII character 0x2B ...
	.BYTE	$00	;         
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$7C	;   ***** 
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x2C ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$10	;     *   

; ASCII character 0x2D ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x2E ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x2F ...
	.BYTE	$00	;         
	.BYTE	$40	;       * 
	.BYTE	$20	;      *  
	.BYTE	$10	;     *   
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$02	;  *      
	.BYTE	$00	;         

; ASCII character 0x30 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$46	;  **   * 
	.BYTE	$4A	;  * *  * 
	.BYTE	$52	;  *  * * 
	.BYTE	$62	;  *   ** 
	.BYTE	$62	;  *   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x31 ...
	.BYTE	$18	;    **   
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$58	;    ** * 
	.BYTE	$7C	;   ***** 
	.BYTE	$00	;         

; ASCII character 0x32 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$40	;       * 
	.BYTE	$7E	;  ****** 
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x33 ...
	.BYTE	$3E	;  *****  
	.BYTE	$22	;  *   *  
	.BYTE	$20	;      *  
	.BYTE	$78	;    **** 
	.BYTE	$60	;      ** 
	.BYTE	$62	;  *   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x34 ...
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$7E	;  ****** 
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x35 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$02	;  *      
	.BYTE	$7E	;  ****** 
	.BYTE	$60	;      ** 
	.BYTE	$60	;      ** 
	.BYTE	$62	;  *   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x36 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$02	;  *      
	.BYTE	$7E	;  ****** 
	.BYTE	$62	;  *   ** 
	.BYTE	$62	;  *   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x37 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$40	;       * 
	.BYTE	$40	;       * 
	.BYTE	$60	;      ** 
	.BYTE	$60	;      ** 
	.BYTE	$60	;      ** 
	.BYTE	$60	;      ** 
	.BYTE	$00	;         

; ASCII character 0x38 ...
	.BYTE	$3C	;   ****  
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$7E	;  ****** 
	.BYTE	$62	;  *   ** 
	.BYTE	$62	;  *   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x39 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$7E	;  ****** 
	.BYTE	$60	;      ** 
	.BYTE	$60	;      ** 
	.BYTE	$60	;      ** 
	.BYTE	$00	;         

; ASCII character 0x3A ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x3B ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$10	;     *   

; ASCII character 0x3C ...
	.BYTE	$70	;     *** 
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$06	;  **     
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$70	;     *** 
	.BYTE	$00	;         

; ASCII character 0x3D ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x3E ...
	.BYTE	$0E	;  ***    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$60	;      ** 
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0E	;  ***    
	.BYTE	$00	;         

; ASCII character 0x3F ...
	.BYTE	$7E	;  ****** 
	.BYTE	$40	;       * 
	.BYTE	$40	;       * 
	.BYTE	$7E	;  ****** 
	.BYTE	$06	;  **     
	.BYTE	$00	;         
	.BYTE	$06	;  **     
	.BYTE	$00	;         

; ASCII character 0x40 ...
	.BYTE	$38	;    ***  
	.BYTE	$44	;   *   * 
	.BYTE	$52	;  *  * * 
	.BYTE	$6A	;  * * ** 
	.BYTE	$32	;  *  **  
	.BYTE	$04	;   *     
	.BYTE	$78	;    **** 
	.BYTE	$00	;         

; ASCII character 0x41 ...
	.BYTE	$3C	;   ****  
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$7E	;  ****** 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$00	;         

; ASCII character 0x42 ...
	.BYTE	$1E	;  ****   
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3E	;  *****  
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x43 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$02	;  *      
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x44 ...
	.BYTE	$3E	;  *****  
	.BYTE	$62	;  *   ** 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$66	;  **  ** 
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x45 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$3E	;  *****  
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x46 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$7E	;  ****** 
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$00	;         

; ASCII character 0x47 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$02	;  *      
	.BYTE	$76	;  ** *** 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x48 ...
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$7E	;  ****** 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$00	;         

; ASCII character 0x49 ...
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x4A ...
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$60	;      ** 
	.BYTE	$60	;      ** 
	.BYTE	$62	;  *   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x4B ...
	.BYTE	$42	;  *    * 
	.BYTE	$22	;  *   *  
	.BYTE	$12	;  *  *   
	.BYTE	$3E	;  *****  
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$00	;         

; ASCII character 0x4C ...
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x4D ...
	.BYTE	$7E	;  ****** 
	.BYTE	$52	;  *  * * 
	.BYTE	$52	;  *  * * 
	.BYTE	$56	;  ** * * 
	.BYTE	$56	;  ** * * 
	.BYTE	$56	;  ** * * 
	.BYTE	$56	;  ** * * 
	.BYTE	$00	;         

; ASCII character 0x4E ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$00	;         

; ASCII character 0x4F ...
	.BYTE	$7E	;  ****** 
	.BYTE	$62	;  *   ** 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x50 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$7E	;  ****** 
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$00	;         

; ASCII character 0x51 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$52	;  *  * * 
	.BYTE	$72	;  *  *** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x52 ...
	.BYTE	$3E	;  *****  
	.BYTE	$22	;  *   *  
	.BYTE	$22	;  *   *  
	.BYTE	$3E	;  *****  
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$00	;         

; ASCII character 0x53 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$02	;  *      
	.BYTE	$7E	;  ****** 
	.BYTE	$60	;      ** 
	.BYTE	$62	;  *   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x54 ...
	.BYTE	$7C	;   ***** 
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x55 ...
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x56 ...
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$66	;  **  ** 
	.BYTE	$24	;   *  *  
	.BYTE	$24	;   *  *  
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x57 ...
	.BYTE	$52	;  *  * * 
	.BYTE	$52	;  *  * * 
	.BYTE	$52	;  *  * * 
	.BYTE	$56	;  ** * * 
	.BYTE	$56	;  ** * * 
	.BYTE	$56	;  ** * * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x58 ...
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$66	;  **  ** 
	.BYTE	$18	;    **   
	.BYTE	$66	;  **  ** 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$00	;         

; ASCII character 0x59 ...
	.BYTE	$44	;   *   * 
	.BYTE	$44	;   *   * 
	.BYTE	$44	;   *   * 
	.BYTE	$7C	;   ***** 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x5A ...
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$60	;      ** 
	.BYTE	$18	;    **   
	.BYTE	$06	;  **     
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x5B ...
	.BYTE	$3C	;   ****  
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x5C ...
	.BYTE	$00	;         
	.BYTE	$02	;  *      
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$20	;      *  
	.BYTE	$40	;       * 
	.BYTE	$00	;         

; ASCII character 0x5D ...
	.BYTE	$3C	;   ****  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x5E ...
	.BYTE	$00	;         
	.BYTE	$10	;     *   
	.BYTE	$38	;    ***  
	.BYTE	$54	;   * * * 
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$28	;    * *  
	.BYTE	$28	;    * *  

; ASCII character 0x5F ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$FE	;  *******
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x60 ...
	.BYTE	$38	;    ***  
	.BYTE	$44	;   *   * 
	.BYTE	$52	;  *  * * 
	.BYTE	$6A	;  * * ** 
	.BYTE	$32	;  *  **  
	.BYTE	$04	;   *     
	.BYTE	$78	;    **** 
	.BYTE	$00	;         

; ASCII character 0x61 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3C	;   ****  
	.BYTE	$20	;      *  
	.BYTE	$3E	;  *****  
	.BYTE	$26	;  **  *  
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x62 ...
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x63 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$06	;  **     
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x64 ...
	.BYTE	$40	;       * 
	.BYTE	$40	;       * 
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x65 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$7E	;  ****** 
	.BYTE	$06	;  **     
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x66 ...
	.BYTE	$78	;    **** 
	.BYTE	$48	;    *  * 
	.BYTE	$08	;    *    
	.BYTE	$3E	;  *****  
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x67 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$40	;       * 
	.BYTE	$7E	;  ****** 

; ASCII character 0x68 ...
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$00	;         

; ASCII character 0x69 ...
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x6A ...
	.BYTE	$30	;     **  
	.BYTE	$00	;         
	.BYTE	$10	;     *   
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$22	;  *   *  
	.BYTE	$3E	;  *****  

; ASCII character 0x6B ...
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      
	.BYTE	$22	;  *   *  
	.BYTE	$12	;  *  *   
	.BYTE	$1E	;  ****   
	.BYTE	$26	;  **  *  
	.BYTE	$26	;  **  *  
	.BYTE	$00	;         

; ASCII character 0x6C ...
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x6D ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$FE	;  *******
	.BYTE	$92	;  *  *  *
	.BYTE	$B6	;  ** ** *
	.BYTE	$B6	;  ** ** *
	.BYTE	$B6	;  ** ** *
	.BYTE	$00	;         

; ASCII character 0x6E ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$00	;         

; ASCII character 0x6F ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x70 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$02	;  *      
	.BYTE	$02	;  *      

; ASCII character 0x71 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$42	;  *    * 
	.BYTE	$62	;  *   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$40	;       * 
	.BYTE	$40	;       * 

; ASCII character 0x72 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$02	;  *      
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$00	;         

; ASCII character 0x73 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$02	;  *      
	.BYTE	$7E	;  ****** 
	.BYTE	$60	;      ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x74 ...
	.BYTE	$08	;    *    
	.BYTE	$08	;    *    
	.BYTE	$3E	;  *****  
	.BYTE	$08	;    *    
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x75 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x76 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$66	;  **  ** 
	.BYTE	$24	;   *  *  
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x77 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$92	;  *  *  *
	.BYTE	$92	;  *  *  *
	.BYTE	$B6	;  ** ** *
	.BYTE	$B6	;  ** ** *
	.BYTE	$FE	;  *******
	.BYTE	$00	;         

; ASCII character 0x78 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$3C	;   ****  
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$00	;         

; ASCII character 0x79 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$46	;  **   * 
	.BYTE	$46	;  **   * 
	.BYTE	$42	;  *    * 
	.BYTE	$7E	;  ****** 
	.BYTE	$40	;       * 
	.BYTE	$7E	;  ****** 

; ASCII character 0x7A ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$60	;      ** 
	.BYTE	$18	;    **   
	.BYTE	$06	;  **     
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x7B ...
	.BYTE	$3C	;   ****  
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$04	;   *     
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x7C ...
	.BYTE	$00	;         
	.BYTE	$02	;  *      
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$10	;     *   
	.BYTE	$20	;      *  
	.BYTE	$40	;       * 
	.BYTE	$00	;         

; ASCII character 0x7D ...
	.BYTE	$3C	;   ****  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$20	;      *  
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x7E ...
	.BYTE	$00	;         
	.BYTE	$10	;     *   
	.BYTE	$38	;    ***  
	.BYTE	$54	;   * * * 
	.BYTE	$10	;     *   
	.BYTE	$10	;     *   
	.BYTE	$28	;    * *  
	.BYTE	$28	;    * *  

; ASCII character 0x7F ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$04	;   *     
	.BYTE	$FE	;  *******
	.BYTE	$04	;   *     
	.BYTE	$08	;    *    
	.BYTE	$00	;         

	.SBTTL	Font 3 Scanlines 9-16
	.ORG	$1400

;++
; These are unused, and should always be zero!
;--
	.FILL	$400, 0

	.SBTTL	PC Code Page 437 Font
	.ORG	$1800

; ASCII character 0x00 ...
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$36	;  ** **  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x01 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$81	; *      *
	.BYTE	$A5	; * *  * *
	.BYTE	$81	; *      *
	.BYTE	$BD	; * **** *
	.BYTE	$99	; *  **  *
	.BYTE	$81	; *      *
	.BYTE	$7E	;  ****** 

; ASCII character 0x02 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$FF	; ********
	.BYTE	$DB	; ** ** **
	.BYTE	$FF	; ********
	.BYTE	$C3	; **    **
	.BYTE	$E7	; ***  ***
	.BYTE	$FF	; ********
	.BYTE	$7E	;  ****** 

; ASCII character 0x03 ...
	.BYTE	$36	;  ** **  
	.BYTE	$7F	; ******* 
	.BYTE	$7F	; ******* 
	.BYTE	$7F	; ******* 
	.BYTE	$3E	;  *****  
	.BYTE	$1C	;   ***   
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x04 ...
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$3E	;  *****  
	.BYTE	$7F	; ******* 
	.BYTE	$3E	;  *****  
	.BYTE	$1C	;   ***   
	.BYTE	$08	;    *    
	.BYTE	$00	;         

; ASCII character 0x05 ...
	.BYTE	$1C	;   ***   
	.BYTE	$3E	;  *****  
	.BYTE	$1C	;   ***   
	.BYTE	$7F	; ******* 
	.BYTE	$7F	; ******* 
	.BYTE	$6B	; ** * ** 
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   

; ASCII character 0x06 ...
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$3E	;  *****  
	.BYTE	$7F	; ******* 
	.BYTE	$7F	; ******* 
	.BYTE	$3E	;  *****  
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   

; ASCII character 0x07 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x08 ...
	.BYTE	$FF	; ********
	.BYTE	$FF	; ********
	.BYTE	$E7	; ***  ***
	.BYTE	$C3	; **    **
	.BYTE	$C3	; **    **
	.BYTE	$E7	; ***  ***
	.BYTE	$FF	; ********
	.BYTE	$FF	; ********

; ASCII character 0x09 ...
	.BYTE	$00	;         
	.BYTE	$3C	;   ****  
	.BYTE	$66	;  **  ** 
	.BYTE	$42	;  *    * 
	.BYTE	$42	;  *    * 
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x0A ...
	.BYTE	$FF	; ********
	.BYTE	$C3	; **    **
	.BYTE	$99	; *  **  *
	.BYTE	$BD	; * **** *
	.BYTE	$BD	; * **** *
	.BYTE	$99	; *  **  *
	.BYTE	$C3	; **    **
	.BYTE	$FF	; ********

; ASCII character 0x0B ...
	.BYTE	$F0	;     ****
	.BYTE	$E0	;      ***
	.BYTE	$F0	;     ****
	.BYTE	$BE	;  ***** *
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$1E	;  ****   

; ASCII character 0x0C ...
	.BYTE	$3C	;   ****  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   

; ASCII character 0x0D ...
	.BYTE	$FC	;   ******
	.BYTE	$CC	;   **  **
	.BYTE	$FC	;   ******
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$0E	;  ***    
	.BYTE	$0F	; ****    
	.BYTE	$07	; ***     

; ASCII character 0x0E ...
	.BYTE	$FE	;  *******
	.BYTE	$C6	;  **   **
	.BYTE	$FE	;  *******
	.BYTE	$C6	;  **   **
	.BYTE	$C6	;  **   **
	.BYTE	$E6	;  **  ***
	.BYTE	$67	; ***  ** 
	.BYTE	$03	; **      

; ASCII character 0x0F ...
	.BYTE	$18	;    **   
	.BYTE	$DB	; ** ** **
	.BYTE	$3C	;   ****  
	.BYTE	$E7	; ***  ***
	.BYTE	$E7	; ***  ***
	.BYTE	$3C	;   ****  
	.BYTE	$DB	; ** ** **
	.BYTE	$18	;    **   

; ASCII character 0x10 ...
	.BYTE	$01	; *       
	.BYTE	$07	; ***     
	.BYTE	$1F	; *****   
	.BYTE	$7F	; ******* 
	.BYTE	$1F	; *****   
	.BYTE	$07	; ***     
	.BYTE	$01	; *       
	.BYTE	$00	;         

; ASCII character 0x11 ...
	.BYTE	$40	;       * 
	.BYTE	$70	;     *** 
	.BYTE	$7C	;   ***** 
	.BYTE	$7F	; ******* 
	.BYTE	$7C	;   ***** 
	.BYTE	$70	;     *** 
	.BYTE	$40	;       * 
	.BYTE	$00	;         

; ASCII character 0x12 ...
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   

; ASCII character 0x13 ...
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$00	;         
	.BYTE	$66	;  **  ** 
	.BYTE	$00	;         

; ASCII character 0x14 ...
	.BYTE	$FE	;  *******
	.BYTE	$DB	; ** ** **
	.BYTE	$DB	; ** ** **
	.BYTE	$DE	;  **** **
	.BYTE	$D8	;    ** **
	.BYTE	$D8	;    ** **
	.BYTE	$D8	;    ** **
	.BYTE	$00	;         

; ASCII character 0x15 ...
	.BYTE	$7C	;   ***** 
	.BYTE	$86	;  **    *
	.BYTE	$3C	;   ****  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$61	; *    ** 
	.BYTE	$3E	;  *****  

; ASCII character 0x16 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$7E	;  ****** 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x17 ...
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$FF	; ********

; ASCII character 0x18 ...
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x19 ...
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x1A ...
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$7F	; ******* 
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1B ...
	.BYTE	$00	;         
	.BYTE	$0C	;   **    
	.BYTE	$06	;  **     
	.BYTE	$7F	; ******* 
	.BYTE	$06	;  **     
	.BYTE	$0C	;   **    
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1C ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$03	; **      
	.BYTE	$03	; **      
	.BYTE	$03	; **      
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1D ...
	.BYTE	$00	;         
	.BYTE	$24	;   *  *  
	.BYTE	$66	;  **  ** 
	.BYTE	$FF	; ********
	.BYTE	$66	;  **  ** 
	.BYTE	$24	;   *  *  
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1E ...
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$7E	;  ****** 
	.BYTE	$FF	; ********
	.BYTE	$FF	; ********
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x1F ...
	.BYTE	$00	;         
	.BYTE	$FF	; ********
	.BYTE	$FF	; ********
	.BYTE	$7E	;  ****** 
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x20 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x21 ...
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x22 ...
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$24	;   *  *  
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x23 ...
	.BYTE	$36	;  ** **  
	.BYTE	$36	;  ** **  
	.BYTE	$7F	; ******* 
	.BYTE	$36	;  ** **  
	.BYTE	$7F	; ******* 
	.BYTE	$36	;  ** **  
	.BYTE	$36	;  ** **  
	.BYTE	$00	;         

; ASCII character 0x24 ...
	.BYTE	$18	;    **   
	.BYTE	$7C	;   ***** 
	.BYTE	$06	;  **     
	.BYTE	$3C	;   ****  
	.BYTE	$60	;      ** 
	.BYTE	$3E	;  *****  
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x25 ...
	.BYTE	$00	;         
	.BYTE	$63	; **   ** 
	.BYTE	$33	; **  **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$66	;  **  ** 
	.BYTE	$63	; **   ** 
	.BYTE	$00	;         

; ASCII character 0x26 ...
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$1C	;   ***   
	.BYTE	$6E	;  *** ** 
	.BYTE	$3B	; ** ***  
	.BYTE	$33	; **  **  
	.BYTE	$6E	;  *** ** 
	.BYTE	$00	;         

; ASCII character 0x27 ...
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x28 ...
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$00	;         

; ASCII character 0x29 ...
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$00	;         

; ASCII character 0x2A ...
	.BYTE	$00	;         
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$FF	; ********
	.BYTE	$3C	;   ****  
	.BYTE	$66	;  **  ** 
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x2B ...
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x2C ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    

; ASCII character 0x2D ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x2E ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x2F ...
	.BYTE	$60	;      ** 
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$06	;  **     
	.BYTE	$03	; **      
	.BYTE	$01	; *       
	.BYTE	$00	;         

; ASCII character 0x30 ...
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$63	; **   ** 
	.BYTE	$6B	; ** * ** 
	.BYTE	$63	; **   ** 
	.BYTE	$36	;  ** **  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x31 ...
	.BYTE	$18	;    **   
	.BYTE	$1C	;   ***   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x32 ...
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$60	;      ** 
	.BYTE	$38	;    ***  
	.BYTE	$0C	;   **    
	.BYTE	$66	;  **  ** 
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         

; ASCII character 0x33 ...
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$60	;      ** 
	.BYTE	$3C	;   ****  
	.BYTE	$60	;      ** 
	.BYTE	$63	; **   ** 
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x34 ...
	.BYTE	$38	;    ***  
	.BYTE	$3C	;   ****  
	.BYTE	$36	;  ** **  
	.BYTE	$33	; **  **  
	.BYTE	$7F	; ******* 
	.BYTE	$30	;     **  
	.BYTE	$78	;    **** 
	.BYTE	$00	;         

; ASCII character 0x35 ...
	.BYTE	$7F	; ******* 
	.BYTE	$03	; **      
	.BYTE	$03	; **      
	.BYTE	$3F	; ******  
	.BYTE	$60	;      ** 
	.BYTE	$63	; **   ** 
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x36 ...
	.BYTE	$1C	;   ***   
	.BYTE	$06	;  **     
	.BYTE	$03	; **      
	.BYTE	$3F	; ******  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x37 ...
	.BYTE	$7F	; ******* 
	.BYTE	$63	; **   ** 
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$00	;         

; ASCII character 0x38 ...
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x39 ...
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$60	;      ** 
	.BYTE	$30	;     **  
	.BYTE	$1E	;  ****   
	.BYTE	$00	;         

; ASCII character 0x3A ...
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x3B ...
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    

; ASCII character 0x3C ...
	.BYTE	$60	;      ** 
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$60	;      ** 
	.BYTE	$00	;         

; ASCII character 0x3D ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x3E ...
	.BYTE	$06	;  **     
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$0C	;   **    
	.BYTE	$06	;  **     
	.BYTE	$00	;         

; ASCII character 0x3F ...
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$30	;     **  
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x40 ...
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$7B	; ** **** 
	.BYTE	$7B	; ** **** 
	.BYTE	$7B	; ** **** 
	.BYTE	$03	; **      
	.BYTE	$1E	;  ****   
	.BYTE	$00	;         

; ASCII character 0x41 ...
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$63	; **   ** 
	.BYTE	$7F	; ******* 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$00	;         

; ASCII character 0x42 ...
	.BYTE	$3F	; ******  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3E	;  *****  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3F	; ******  
	.BYTE	$00	;         

; ASCII character 0x43 ...
	.BYTE	$3C	;   ****  
	.BYTE	$66	;  **  ** 
	.BYTE	$03	; **      
	.BYTE	$03	; **      
	.BYTE	$03	; **      
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x44 ...
	.BYTE	$1F	; *****   
	.BYTE	$36	;  ** **  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$36	;  ** **  
	.BYTE	$1F	; *****   
	.BYTE	$00	;         

; ASCII character 0x45 ...
	.BYTE	$7F	; ******* 
	.BYTE	$46	;  **   * 
	.BYTE	$16	;  ** *   
	.BYTE	$1E	;  ****   
	.BYTE	$16	;  ** *   
	.BYTE	$46	;  **   * 
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         

; ASCII character 0x46 ...
	.BYTE	$7F	; ******* 
	.BYTE	$46	;  **   * 
	.BYTE	$16	;  ** *   
	.BYTE	$1E	;  ****   
	.BYTE	$16	;  ** *   
	.BYTE	$06	;  **     
	.BYTE	$0F	; ****    
	.BYTE	$00	;         

; ASCII character 0x47 ...
	.BYTE	$3C	;   ****  
	.BYTE	$66	;  **  ** 
	.BYTE	$03	; **      
	.BYTE	$03	; **      
	.BYTE	$73	; **  *** 
	.BYTE	$66	;  **  ** 
	.BYTE	$5C	;   *** * 
	.BYTE	$00	;         

; ASCII character 0x48 ...
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$7F	; ******* 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$00	;         

; ASCII character 0x49 ...
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x4A ...
	.BYTE	$78	;    **** 
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$1E	;  ****   
	.BYTE	$00	;         

; ASCII character 0x4B ...
	.BYTE	$67	; ***  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$36	;  ** **  
	.BYTE	$1E	;  ****   
	.BYTE	$36	;  ** **  
	.BYTE	$66	;  **  ** 
	.BYTE	$67	; ***  ** 
	.BYTE	$00	;         

; ASCII character 0x4C ...
	.BYTE	$0F	; ****    
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$46	;  **   * 
	.BYTE	$66	;  **  ** 
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         

; ASCII character 0x4D ...
	.BYTE	$63	; **   ** 
	.BYTE	$77	; *** *** 
	.BYTE	$7F	; ******* 
	.BYTE	$7F	; ******* 
	.BYTE	$6B	; ** * ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$00	;         

; ASCII character 0x4E ...
	.BYTE	$63	; **   ** 
	.BYTE	$67	; ***  ** 
	.BYTE	$6F	; **** ** 
	.BYTE	$7B	; ** **** 
	.BYTE	$73	; **  *** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$00	;         

; ASCII character 0x4F ...
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x50 ...
	.BYTE	$3F	; ******  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3E	;  *****  
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$0F	; ****    
	.BYTE	$00	;         

; ASCII character 0x51 ...
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$73	; **  *** 
	.BYTE	$3E	;  *****  
	.BYTE	$70	;     *** 

; ASCII character 0x52 ...
	.BYTE	$3F	; ******  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3E	;  *****  
	.BYTE	$36	;  ** **  
	.BYTE	$66	;  **  ** 
	.BYTE	$67	; ***  ** 
	.BYTE	$00	;         

; ASCII character 0x53 ...
	.BYTE	$3C	;   ****  
	.BYTE	$66	;  **  ** 
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x54 ...
	.BYTE	$7E	;  ****** 
	.BYTE	$7E	;  ****** 
	.BYTE	$5A	;  * ** * 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x55 ...
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x56 ...
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$36	;  ** **  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x57 ...
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$6B	; ** * ** 
	.BYTE	$6B	; ** * ** 
	.BYTE	$7F	; ******* 
	.BYTE	$36	;  ** **  
	.BYTE	$00	;         

; ASCII character 0x58 ...
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$36	;  ** **  
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$00	;         

; ASCII character 0x59 ...
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x5A ...
	.BYTE	$7F	; ******* 
	.BYTE	$63	; **   ** 
	.BYTE	$31	; *   **  
	.BYTE	$18	;    **   
	.BYTE	$4C	;   **  * 
	.BYTE	$66	;  **  ** 
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         

; ASCII character 0x5B ...
	.BYTE	$3C	;   ****  
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x5C ...
	.BYTE	$03	; **      
	.BYTE	$06	;  **     
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$60	;      ** 
	.BYTE	$40	;       * 
	.BYTE	$00	;         

; ASCII character 0x5D ...
	.BYTE	$3C	;   ****  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$30	;     **  
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x5E ...
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$63	; **   ** 
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x5F ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$FF	; ********

; ASCII character 0x60 ...
	.BYTE	$0C	;   **    
	.BYTE	$18	;    **   
	.BYTE	$30	;     **  
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x61 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$1E	;  ****   
	.BYTE	$30	;     **  
	.BYTE	$3E	;  *****  
	.BYTE	$33	; **  **  
	.BYTE	$6E	;  *** ** 
	.BYTE	$00	;         

; ASCII character 0x62 ...
	.BYTE	$07	; ***     
	.BYTE	$06	;  **     
	.BYTE	$3E	;  *****  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3B	; ** ***  
	.BYTE	$00	;         

; ASCII character 0x63 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$03	; **      
	.BYTE	$63	; **   ** 
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x64 ...
	.BYTE	$38	;    ***  
	.BYTE	$30	;     **  
	.BYTE	$3E	;  *****  
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$6E	;  *** ** 
	.BYTE	$00	;         

; ASCII character 0x65 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$7F	; ******* 
	.BYTE	$03	; **      
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x66 ...
	.BYTE	$3C	;   ****  
	.BYTE	$66	;  **  ** 
	.BYTE	$06	;  **     
	.BYTE	$1F	; *****   
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$0F	; ****    
	.BYTE	$00	;         

; ASCII character 0x67 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$6E	;  *** ** 
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$3E	;  *****  
	.BYTE	$30	;     **  
	.BYTE	$1F	; *****   

; ASCII character 0x68 ...
	.BYTE	$07	; ***     
	.BYTE	$06	;  **     
	.BYTE	$36	;  ** **  
	.BYTE	$6E	;  *** ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$67	; ***  ** 
	.BYTE	$00	;         

; ASCII character 0x69 ...
	.BYTE	$18	;    **   
	.BYTE	$00	;         
	.BYTE	$1C	;   ***   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x6A ...
	.BYTE	$60	;      ** 
	.BYTE	$00	;         
	.BYTE	$60	;      ** 
	.BYTE	$60	;      ** 
	.BYTE	$60	;      ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3C	;   ****  

; ASCII character 0x6B ...
	.BYTE	$07	; ***     
	.BYTE	$06	;  **     
	.BYTE	$66	;  **  ** 
	.BYTE	$36	;  ** **  
	.BYTE	$1E	;  ****   
	.BYTE	$36	;  ** **  
	.BYTE	$67	; ***  ** 
	.BYTE	$00	;         

; ASCII character 0x6C ...
	.BYTE	$1C	;   ***   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$3C	;   ****  
	.BYTE	$00	;         

; ASCII character 0x6D ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$37	; *** **  
	.BYTE	$7F	; ******* 
	.BYTE	$6B	; ** * ** 
	.BYTE	$6B	; ** * ** 
	.BYTE	$6B	; ** * ** 
	.BYTE	$00	;         

; ASCII character 0x6E ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3B	; ** ***  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$00	;         

; ASCII character 0x6F ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3E	;  *****  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$3E	;  *****  
	.BYTE	$00	;         

; ASCII character 0x70 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3B	; ** ***  
	.BYTE	$66	;  **  ** 
	.BYTE	$66	;  **  ** 
	.BYTE	$3E	;  *****  
	.BYTE	$06	;  **     
	.BYTE	$0F	; ****    

; ASCII character 0x71 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$6E	;  *** ** 
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$3E	;  *****  
	.BYTE	$30	;     **  
	.BYTE	$78	;    **** 

; ASCII character 0x72 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$3B	; ** ***  
	.BYTE	$6E	;  *** ** 
	.BYTE	$06	;  **     
	.BYTE	$06	;  **     
	.BYTE	$0F	; ****    
	.BYTE	$00	;         

; ASCII character 0x73 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$03	; **      
	.BYTE	$3E	;  *****  
	.BYTE	$60	;      ** 
	.BYTE	$3F	; ******  
	.BYTE	$00	;         

; ASCII character 0x74 ...
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$3F	; ******  
	.BYTE	$0C	;   **    
	.BYTE	$0C	;   **    
	.BYTE	$6C	;   ** ** 
	.BYTE	$38	;    ***  
	.BYTE	$00	;         

; ASCII character 0x75 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$33	; **  **  
	.BYTE	$6E	;  *** ** 
	.BYTE	$00	;         

; ASCII character 0x76 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$36	;  ** **  
	.BYTE	$1C	;   ***   
	.BYTE	$00	;         

; ASCII character 0x77 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$63	; **   ** 
	.BYTE	$6B	; ** * ** 
	.BYTE	$6B	; ** * ** 
	.BYTE	$7F	; ******* 
	.BYTE	$36	;  ** **  
	.BYTE	$00	;         

; ASCII character 0x78 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$63	; **   ** 
	.BYTE	$36	;  ** **  
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$63	; **   ** 
	.BYTE	$00	;         

; ASCII character 0x79 ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$7E	;  ****** 
	.BYTE	$60	;      ** 
	.BYTE	$3F	; ******  

; ASCII character 0x7A ...
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$7E	;  ****** 
	.BYTE	$32	;  *  **  
	.BYTE	$18	;    **   
	.BYTE	$4C	;   **  * 
	.BYTE	$7E	;  ****** 
	.BYTE	$00	;         

; ASCII character 0x7B ...
	.BYTE	$70	;     *** 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$0E	;  ***    
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$70	;     *** 
	.BYTE	$00	;         

; ASCII character 0x7C ...
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$00	;         

; ASCII character 0x7D ...
	.BYTE	$0E	;  ***    
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$70	;     *** 
	.BYTE	$18	;    **   
	.BYTE	$18	;    **   
	.BYTE	$0E	;  ***    
	.BYTE	$00	;         

; ASCII character 0x7E ...
	.BYTE	$6E	;  *** ** 
	.BYTE	$3B	; ** ***  
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         
	.BYTE	$00	;         

; ASCII character 0x7F ...
	.BYTE	$00	;         
	.BYTE	$08	;    *    
	.BYTE	$1C	;   ***   
	.BYTE	$36	;  ** **  
	.BYTE	$63	; **   ** 
	.BYTE	$63	; **   ** 
	.BYTE	$7F	; ******* 
	.BYTE	$00	;         

	.SBTTL	Font 4 Scanlines 9-16
	.ORG	$1C00

;++
; These are unused, and should always be zero!
;--
	.FILL	$400, 0

	.END
