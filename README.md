# Schelme 

Schelme is a scheme-inspired scripting language for Elm.  

Schelme is written with incremental execution in mind.  Running a schelme program consists of creating an EvalBodyStart state instance, then calling evalBodyStateStep on that instance again and again until either an EvalBodyFinal or EvalBodyError is returned.  This way you are free to execute as many evals as you have time for, then move on to other tasks.  With this method running multiple schelme programs simultaneously is no problem - you just have to store the latest state for each program. 

The Run module contains functions to compile and execute schelme code.  Also check the examples to see how to get schelme to call your own custom functions.

You can try out the basic language [here](https://bburdette.github.io/schelmeex3.html).  The prelude functions are listed in the initial namespace, and there's a minimal language reference.

Here's a [robot battle demo](https://bots.practica.site).  This shows off the incremental execution feature.  Its in a [separate repo](https://github.com/bburdette/schelme-bots) now.

And there's [spreadsheet and markdown demos](https://www.github.com/bburdette/cellme) too.
