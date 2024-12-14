#++
# Makefile - Makefile for the Spare Time Gizmos VT1802 EPROM
#
# Copyright (C) 2024 by Spare Time Gizmos.  All rights reserved.
#
# This firmware is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 59 Temple
# Place, Suite 330, Boston, MA  02111-1307  USA
#
#DESCRIPTION:
#   This Makefile will build the EPROM image for the Spare Time Gizmos VT1802
# video terminal.  The result is a file called eprom.hex, which is ready to
# burn into a 27C256.
#
#TARGETS:
#  make all	- rebuild eprom.hex
#  make clean	- clean up all generated files except vt1802.hex
#  make distro	- make the vt1802.zip source file distribution
#  make vtfont	- rebuild vtfont.hex (VT1802 font EPROM programming)
#
# REVISION HISTORY:
# dd-mmm-yy	who     description
# 24-Apr-24	RLA	New file.
# 10-May-24	RLA	Add help file.
# 17-JUL-24	RLA	Add BASIC3.
# 26-SEP-24	RLA	Add font file stuff.
# 29-SEP-24	RLA	Add CPUCLOCK and PIXELCLOCK definitions.
# 30-NOV-24	RLA	Use "shell which" to find tools (thanks, David!)
#			Make PIXELCLOCK and CPUCLOCK explicit parameters.
#  2-DEC-24	RLA	Add GLYPHWIDTH parameter.
#--

#    VT1802 assembly options.  CPUCLOCK is in units of 100 Hertz. Note that
# the only supported PIXELCLOCKs are 1431810MHz or 12000000Hz.  GLYPHWIDTH
# may be either 8 or 9, but is only supported for the 14.31818MHz PIXELCLOCK.
# For the 12MHz PIXELCLOCK it is ignored.
CPUCLOCK=40000
PIXELCLOCK=14318180
GLYPHWIDTH=8

# Tool paths and options - you can change these as necessary...
TASM=$(shell which tasm)
export TASMTABS=$(dir $(TASM))
TASMOPTS=-t1802 -x1 -lat -g0 -i -p44 -DTASM -DVT1802 \
	 -DCPUCLOCK=$(CPUCLOCK) -DPIXELCLOCK=$(PIXELCLOCK) -DGLYPHWIDTH=$(GLYPHWIDTH)
A18=$(shell which a18)
ROMMERGE=$(shell which rommerge)
ROMCKSUM=$(shell which romcksum)
ROMTEXT=$(shell which romtext)
ZIP=C:/Program Files/7-Zip/7z.exe
RM=$(shell which rm)
ECHO=$(shell which echo)
DATE=$(shell which date)


# All components of the EPROM image ...
HEXFILES = vt1802.hex basic3.hex help.hex
LISTFILES = $(HEXFILES:.hex=.lst)

# The default target builds everything...
all:	eprom.hex

vt1802.hex:	vt1802.asm sysdat.asm
	@$(ECHO) -e "\n*** BUILDING VT1802 FIRMWARE ***"
	@$(RM) -f sysdat.asm
	@$(DATE) +"SYSDAT:	.TEXT	\"%d-%#b-%Y %H:%M\\000\"" >sysdat.asm
	@$(TASM) $(TASMOPTS) $< $@

basic3.hex:	basic3.asm
	@$(ECHO) -e "\n*** BUILDING BASIC3 ***"
	@$(A18) $< -l $(<:.asm=.lst) -o $@

help.hex:	vt1802.hlp
	@$(ECHO) -e "\n*** COMPILING HELP TEXT ***"
	@$(ROMTEXT) -a0x7800 vt1802.hlp help.hex

merged.hex:	$(HEXFILES)
	@$(ECHO) -e "\n*** MERGING FILES INTO EPROM IMAGE ***"
	@$(ROMMERGE) -s32k -o00000 merged.hex $(HEXFILES)

eprom.hex:	merged.hex
	@$(ECHO) -e "\n*** CALCULATING EPROM CHECKSUM ***"
	@$(ROMCKSUM) merged.hex -s32K -o00000 -c32764 eprom.hex

vtfont:	vtfont.hex

vtfont.hex:	vtfont.asm
	@$(TASM) $(TASMOPTS) $< $@

# The "clean" target does what you'd expect...
clean:
	$(RM) -f $(HEXFILES)
	$(RM) -f $(LISTFILES)
	$(RM) -f *.*\~ \#*.*\#

#   The "distro" target builds a vt1802.zip file which contains all the tools,
# source files, readme files, license files, etc that are usually included in
# the source distribution.  (A binary distribution, of course, only contains
# the VT1802.hex file!)
distro:
	@$(ECHO) Building source distribution ...
	$(RM) -f vt1802.zip
	$(ZIP) a vt1802.zip \
	  vt1802.asm vt1802.hlp basic3.asm Makefile. vtfont.asm \
	  readme.txt license.txt eprom.hex vtfont.hex

