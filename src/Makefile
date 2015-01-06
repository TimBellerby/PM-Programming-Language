#
#PM (Parallel Models) Programming Language
#
#Released under the MIT License (MIT)
#
#Copyright (c) Tim Bellerby, 2015
#
#Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included in
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
FFLAGS=-g   # -fbounds-check
PMCODE= kinds.o memory.o lib.o types.o parser.o linker.o sysdefs.o codegen.o infer.o backend.o vm.o main.o

all : pm.exe

pm.exe : $(PMCODE)
	$(FC) -o pm $(FFLAGS) $(PMCODE) 

kinds.o : kinds.f90
	$(FC) $(FFLAGS) -c $<

memory.o : memory.f90 kinds.o
	$(FC) $(FFLAGS) -c $<

lib.o : lib.f90 kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

types.o : types.f90 lib.o kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

parser.o : parser.f90 lib.o kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

linker.o : linker.f90  parser.o lib.o kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

sysdefs.o : sysdefs.f90 lib.o kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

codegen.o : codegen.f90 parser.o sysdefs.o types.o lib.o kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

infer.o : infer.f90 codegen.o parser.o sysdefs.o types.o lib.o kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

backend.o : backend.f90 infer.o codegen.o parser.o sysdefs.o types.o lib.o kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

vm.o : vm.f90 sysdefs.o types.o lib.o kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

main.o : main.f90 vm.o backend.o infer.o codegen.o linker.o parser.o sysdefs.o types.o lib.o kinds.o memory.o
	$(FC) $(FFLAGS) -c $<

