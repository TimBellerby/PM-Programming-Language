![ PM logo ](./PM-logo.png)
# PM Programming Language 
## [www.pm-lang.org](http://www.pm-lang.org)

## Overview

The PM Programming Language is designed to facilitate the creation
of numerical models on parallel systems. It combines concepts of
parallelisation and vectorisation into a unified model using a 'strand'
as a basic unit of parallelisation. A strand is a very lightweight
entity that does not require its own stack and can map to a single execution 
of a loop body. Strands are used in both data and task parallelism and can communicate
with other strands, including those running on other nodes. PM uses a modified form of
the Partitioned Global Address Space approach which builds synchronisation into basic operations 
and excludes race conditions.

The PM compiler currently cross-compiles to Fortran+MPI. More target language/library
combinations are planned, including Fortran-MPI-OpenMP and accelerator support (initially
via either OpenMP or OpenACC)

The PM interpreter uses a parallel virtual machine and is designed for debugging PM code.

The language specification (available in /doc) while incomplete in places, will give
a good view of the features of the version 0.4 of the language.
    

## Installation and use

This is an initial (not stable) pr-release of version 0.4 of the language.
The interpreter is currently more stable than the compiler.

To compile PM:

Make sure you have MPI installed and loaded and can run mpifort and mpirun

The current setup is developed on Fedora with gfortran and openmp. You
can edit pm/Makefile and pmc/Makefile to change compilers.

The code should be fairly portable. Portability issues may be addressed by
editing config/sysdep.f90 (read the comments)

To compile the interpreter:

  cd pm

  make clean

  make

You can then run the interpreter (./pm --help for options)

mpirun the interpreter for distributed execution.

To compile PM-to-Fortran compiler:

  cd pmc

  make clean

  make

To compile a PM program to Fortran use ./pmc (./pmc --help for options)

The pmc compiler does not use MPI.

The compiled code will appear as PMOUT.F90 and will need to be compiled using mpifort. 

You can then mpirun the resulting executable.

Both compiler and interpreter expect PM source files to have a .pmm extension.


## Watch out for continuing updates.
