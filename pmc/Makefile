#
# PM (Parallel Models) Programming Language
#
# Released under the MIT License (MIT)
#
# Copyright (c) Tim Bellerby, 2020
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
FFLAGS= -I../src/ -g   -fbounds-check -fcheck=mem # -Wall
PFLAGS= -I../src/ -g   -fbounds-check -fcheck=mem

PMCODE= sysdep.o scomp.o kinds.o memory.o hash.o opts.o lib.o types.o symbol.o vmdefs.o parser.o linker.o sysdefs.o codegen.o infer.o wcoder.o optimise.o cfortran.o main.o


all : pmc

clean:
	rm pmc *.o *.mod


pmc : $(PMCODE)
	$(FC) -o pmc $(FFLAGS) $(PMCODE)

sysdep.o : ../config/sysdep.f90
	$(PC) $(PFLAGS) -c $<

scomp.o : ../src/scomp.f90
	$(PC) $(PFLAGS) -c $<

kinds.o : ../src/kinds.f90 scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

memory.o : ../src/memory.f90 kinds.o scomp.o sysdep.o
	$(FC)  -c $<

hash.o : ../src/hash.f90 memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

opts.o : ../src/opts.f90 hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<


symbol.o : ../src/symbol.f90 opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

lib.o : ../src/lib.f90 symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<


vmdefs.o : ../src/vmdefs.f90 lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<


parser.o : ../src/parser.f90 vmdefs.o lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

types.o : ../src/types.f90 lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

linker.o : ../src/linker.f90  parser.o lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

sysdefs.o : ../src/sysdefs.f90 vmdefs.o parser.o lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

codegen.o : ../src/codegen.f90 parser.o symbol.o sysdefs.o types.o lib.o symbol.o opts.o hash.o  memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

infer.o : ../src/infer.f90 codegen.o parser.o symbol.o sysdefs.o types.o lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

wcoder.o : ../src/wcoder.f90 infer.o codegen.o parser.o sysdefs.o types.o lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

optimise.o : ../src/optimise.f90 ../src/rtime.inc wcoder.o infer.o codegen.o linker.o parser.o sysdefs.o types.o lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<

../src/rtime.inc : ../src/runtime.f90
	./make_runtime ../src/runtime.f90 > ../src/rtime.inc

cfortran.o : ../src/cfortran.f90 optimise.o ../src/rtime.inc wcoder.o infer.o codegen.o linker.o parser.o sysdefs.o types.o lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(FFLAGS) -c $<


main.o : ../src/main.f90 cfortran.o wcoder.o infer.o codegen.o linker.o parser.o sysdefs.o types.o opts.o lib.o symbol.o opts.o hash.o memory.o kinds.o scomp.o sysdep.o
	$(FC) $(PFLAGS)  -c $<

