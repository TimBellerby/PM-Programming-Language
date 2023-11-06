![ PM logo ](/PM-LOGO.png)
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

## Status

This is a pre-release version of the PM language. While syntax and semantics are now well
developed, there may be further changes to both in response to feedback.

The implementation is not yet fully free of bugs or unimplemented features. If you encounter
a problem, then please check the latest code on GitHub. If this does not work, then please
raise an issue.

## Contribution

This is an open source project and outside contributions are entirely welcome.

At this stage the most effective contributions are in the are of testing and
feedback on the language design and implementation.

A refactoring of compiler/interpreter source code is underway for version 0.5. 
One goal of this will be to make the code more accessible to outside contributors.
In the meantime, bug-fix contributions to the source code by the brave are certainly welcome.


## Installation and use

This is an initial release of version 0.4 of the language. At the moment
the code is designed to be compiled and installed on a Linux system with
MPI. It should be possible to compile on other systems with MPI and Fortan.

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
