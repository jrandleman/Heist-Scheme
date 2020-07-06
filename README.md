# Heist-Scheme
Souped-Up Scheme Interpreter Written in C++!
---------------------
# Using Heist Scheme:
Compiling the Interpreter: `$ clang++ -std=c++17 -O3 -o heist_main heist_main.cpp`</br></br>
REPL: `$ ./heist_main` (exit REPL via `(exit)` command)</br>
Interpret Script: `$ ./heist_main -script <your-scripts-filename-here>`</br>
Compile Script to C++: `$ ./heist_main -compile <your-scripts-filename-here> <optional-target-name>`</br>
Embed Heist in C++: `#include` the `heist_cpp_interop.hpp` header into your code (read it for more details)!</br>
* See `embedded_heist_demo.cpp` for an example of embedding Heist in action!

----------
# Features:
0) Hygienic Macros
1) Tail-Call Optimization
2) Opt-In Dynamic Scoping (see the `call/ce` & `inline` application primitives)
3) Opt-In Continuations & `call/cc`
4) Native Even Streams (Lists w/ Delayed Car & Cdr)
5) Generic Algorithms (Polymorphic Algorithm Primitives)
6) SFRI Primitives (List, Vector, String, etc.)
7) Eval (Evaluate Symbolic Data as Code)
8) String I/O (Read/Write Compatibility w/ Strings as Ports)
9) Recursive Depth Control
10) See heist_main.cpp for more!
