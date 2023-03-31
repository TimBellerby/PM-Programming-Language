#
# PM (Parallel Models) Programming Language
#
# Released under the MIT License (MIT)
#
# Copyright (c) Tim Bellerby, 2019
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
FFLAGS=  -g  -I../src   -fbounds-check   -fcheck=mem # -Wall
PFLAGS=  -g -I../src    -fbounds-check   -fcheck=mem

PMCODE= sysdep.o kinds.o pcomp.o memory.o hash.o opts.o lib.o symbol.o types.o parser.o linker.o vmdefs.o sysdefs.o codegen.o infer.o wcoder.o array.o parlib.o vm.o main.o


all : pm

clean :
	rm *.o *.mod

pm : $(PMCODE)
	$(PC) -o pm $(PFLAGS) $(PMCODE) 

sysdep.o : ../config/sysdep.f90
	$(PC) $(PFLAGS) -c $<

kinds.o : ../src/kinds.f90 sysdep.o
	$(FC) $(FFLAGS) -c $<

pcomp.o : ../src/pcomp.f90 kinds.o
	$(PC) $(PFLAGS) -c $<

memory.o : ../src/memory.f90 kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

hash.o : ../src/hash.f90 memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

opts.o : ../src/opts.f90 hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

symbol.o : ../src/symbol.f90 opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

lib.o : ../src/lib.f90 symbol.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

vmdefs.o : ../src/vmdefs.f90 symbol.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

parser.o : ../src/parser.f90 types.o vmdefs.o symbol.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

types.o : ../src/types.f90 symbol.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

array.o : ../src/array.f90 types.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

parlib.o : ../src/parlib.f90 array.o types.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(PC) $(PFLAGS) -c $<

linker.o : ../src/linker.f90  parser.o symbol.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

sysdefs.o : ../src/sysdefs.f90 vmdefs.o parser.o symbol.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

codegen.o : ../src/codegen.f90 parser.o symbol.o sysdefs.o types.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

infer.o : ../src/infer.f90 codegen.o parser.o symbol.o sysdefs.o types.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

wcoder.o : ../src/wcoder.f90 array.o infer.o codegen.o parser.o symbol.o sysdefs.o vmdefs.o types.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

vm.o : ../src/vm.f90 parlib.o array.o sysdefs.o types.o symbol.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

main.o : ../src/main.f90 vm.o parlib.o array.o wcoder.o infer.o codegen.o linker.o parser.o symbol.o sysdefs.o types.o lib.o opts.o hash.o memory.o kinds.o pcomp.o sysdep.o
	$(FC) $(PFLAGS) -c $<

