The PM Programming Language is designed to facilitate the creation

of numerical models on parallel systems. It combines concepts of

parallelisation and vectorisation into a unified model.


For a more complete project description see www.pm-lang.org

The language specification (available in /doc) while incomplete in places, will give

a good view of the features of the version 0.1 of the language.
    

Most of the single-node part of the language - including concurrent loops and communicating

operators (apart from reductions) is now working. Types are currently restricted to int, long and bool, 

with some support for strings. 


There is a floating bug in the code generator that causes crashes in some compilations. I am chasing

this down.


Look out for updates over the next month or so.