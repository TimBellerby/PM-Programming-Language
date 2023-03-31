The PM Programming Language is designed to facilitate the creation

of numerical models on parallel systems. It combines concepts of

parallelisation and vectorisation into a unified model.


For a more complete project description see www.pm-lang.org

The language specification (available in /doc) while incomplete in places, will give

a good view of the features of the version 0.4 of the language.
    

This is an initial (not stable) pr-release of version 0.4 of the language.

The interpreter is currently more stable than the compiler.

To compile PM:

Make sure you have MPI installed and loaded and can run mpifort and mpirun

To compile interpreter:
  cd pm
  make clean
  make

To compile PM-to-Fortran compiler:
  cd pmc
  make clean
  make

If there are issues, you can look in the config directory and edit the source file there (follow the comments)


Watch out for continuing updates.
