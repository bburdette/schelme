# Schelme 

Schelme is a scheme-inspired scripting language for Elm.  

Schelme is written with incremental execution in mind.  Running a schelme program consists of creating an EvalBodyStart state instance, then calling evalBodyStateStep on that instance again and again until either an EvalBodyFinal or EvalBodyError is returned.  This way you are free to execute as many evals as you have time for, then move on to other tasks.  With this method running multiple schelme programs simultaneously is no problem - you just have to store the latest state for each program. 

The Run module contains functions to compile and execute schelme code.  Also check the examples to see how to get schelme to call your own custom functions.  Sorry about the disarray in the bots example!  Hopefully that will be tidied up sometime soon. 

You can try out the basic language [here](https://bburdette.github.io/schelmeex2.html).  The prelude functions are listed in the initial namespace.

Here's the fancy [robot battle demo](https://bburdette.github.io/schelmebots1.html).  This shows off the incremental execution feature.  Add bots to see some demo code.

Sorry, there's no language reference yet!  Hopefully soon though - I'll try to integrate that into namespace creation with an eye towards repl based help in the future.


