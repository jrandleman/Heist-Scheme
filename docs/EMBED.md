# Embedding Heist Scheme in C++17

## Why Embed?

Whereas [`EXTEND.md`](./EXTEND.md) discusses extending Heist Scheme with C++17, embedding enables 
extending C++17 with Heist Scheme!<br>
Refer to [`examples/embedded_heist_demo.cpp`](../examples/embedded_heist_demo.cpp) to see Heist 
Scheme being embedded in action!




## How Embed?

### Prerequisite Knowledge for Embedding Heist Scheme in C++17

Before getting into embedding Heist Scheme in C++ though, you'll first want to learn 
about the underlying C++ type system of Heist Scheme objects. You can learn about this 
type system in [`TYPES.md`](./TYPES.md). 

Should you want more information about working with Heist objects of a particular type, 
I'd recommend perusing [`lib/primitives/stdlib/data`](../lib/primitives/stdlib/data) 
for extensive examples of C++ functions interoperating with the types described in 
[`TYPES.md`](./TYPES.md).


### Actually Embedding

Programs embedding Heist Scheme must `#include` the [`interop.hpp`](../interop.hpp) header.<br>
This defines 3 functions (designed for use in single-threaded environments) in the `heist` namespace:

1. `heist::data eval(std::string exp)`
   * Evaluate `exp` as Heist Scheme code.
     - Returns the result of the evaluation.
     - Returns `*exit-success*` or `*exit-failure*` as a _symbol_ if Heist's `exit` gets triggered as a result of evaluating `exp`
     - Note this can be called on string literals by using the `_heist` suffix, IE:
       `auto thricePi = "(* 3 (acos -1))"_heist;`

2. `heist::data apply(std::string procedure_name, std::vector<heist::data> args)`
   * Apply `args` to `procedure_name` & return the result
   * Alternatively, can pass a `heist::data` callable instead of a `std::string` procedure name

3. `void define(std::string variable_name, heist::data variable_value)`
   * Bind `variable_name` to `variable_value` in Heist's global environment
   * Alternatively, can pass a `heist::data(*)(std::vector<heist::data>&&)` C++ primitive function pointer<br>
     instead of a `heist::data` variable value name
     - See [`EXTEND.md`](./EXTEND.md) for more info on extending Heist Scheme with your own C++ primitives!
