#
# PM (Parallel Models) Programming Language
#
# Released under the MIT License (MIT)
#
# Copyright (c) Tim Bellerby, 2016
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
FC=gfortran
PC=mpifort
FFLAGS= -g  -I../common  #   -fbounds-check #  -fcheck=mem # -Wall
PFLAGS= -g -I../common    -fbounds-check -fcheck=mem

PMCODE= sysdep.o pcomp.o kinds.o memory.o lib.o types.o symbol.o parser.o linker.o vmdefs.o sysdefs.o codegen.o infer.o backend.o array.o parlib.o vm.o main.o


all : pm

pm : $(PMCODE)
	$(PC) -o pm $(PFLAGS) $(PMCODE) 

sysdep.o : ../config/sysdep.f90
	$(PC) $(PFLAGS) -c $<

pcomp.o : ../common/pcomp.f90
	$(PC) $(PFLAGS) -c $<

kinds.o : ../common/kinds.f90 pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

memory.o : ../common/memory.f90 kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

lib.o : ../common/lib.f90 memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

symbol.o : ../common/symbol.f90 lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

parser.o : ../common/parser.f90 symbol.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

types.o : ../common/types.f90 symbol.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

array.o : ../common/array.f90 types.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

parlib.o : ../common/parlib.f90 array.o types.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(PC) $(PFLAGS) -c $<

linker.o : ../common/linker.f90  parser.o symbol.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

vmdefs.o : ../common/vmdefs.f90 parser.o symbol.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<


sysdefs.o : ../common/sysdefs.f90 vmdefs.o parser.o symbol.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

codegen.o : ../common/codegen.f90 parser.o symbol.o sysdefs.o types.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

infer.o : ../common/infer.f90 codegen.o parser.o symbol.o sysdefs.o types.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

backend.o : ../common/backend.f90 array.o infer.o codegen.o parser.o symbol.o sysdefs.o vmdefs.o types.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

vm.o : ../common/vm.f90 parlib.o array.o sysdefs.o types.o symbol.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

main.o : ../common/main.f90 vm.o parlib.o array.o backend.o infer.o codegen.o linker.o parser.o symbol.o sysdefs.o types.o lib.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(PFLAGS) -c $<

