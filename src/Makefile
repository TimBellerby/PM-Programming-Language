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
FFLAGS=-g #   -fbounds-check -fcheck=mem # -Wall
PFLAGS=-g  #  -fbounds-check -fcheck=mem
PMCODE= sysdep.o kinds.o memory.o lib.o types.o parser.o linker.o sysdefs.o codegen.o infer.o backend.o array.o parlib.o vm.o main.o

all : pm

pm : $(PMCODE)
	$(PC) -o pm $(PFLAGS) $(PMCODE) 

sysdep.o : sysdep.f90
	$(PC) $(PFLAGS) -c $<

kinds.o : kinds.f90 sysdep.o
	$(FC) $(FFLAGS) -c $<

memory.o : memory.f90 kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

lib.o : lib.f90 memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

parser.o : parser.f90 lib.o memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

types.o : types.f90 parser.o lib.o memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

array.o : array.f90 types.o lib.o memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

parlib.o : parlib.f90 array.f90 types.o lib.o memory.o kinds.o sysdep.o
	$(PC) $(PFLAGS) -c $<

linker.o : linker.f90  parser.o lib.o memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

sysdefs.o : sysdefs.f90 parser.o lib.o memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

codegen.o : codegen.f90 parser.o sysdefs.o types.o lib.o memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

infer.o : infer.f90 codegen.o parser.o sysdefs.o types.o lib.o memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

backend.o : backend.f90 array.o infer.o codegen.o parser.o sysdefs.o types.o lib.o memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

vm.o : vm.f90 parlib.o array.o sysdefs.o types.o lib.o memory.o kinds.o sysdep.o
	$(FC) $(FFLAGS) -c $<

main.o : main.f90 vm.o parlib.o array.o backend.o infer.o codegen.o linker.o parser.o sysdefs.o types.o lib.o memory.o kinds.o sysdep.o
	$(FC) $(PFLAGS) -c $<

