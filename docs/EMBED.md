# Embedding Heist Scheme in C++17:

## Why Embed?

Whereas `EXTEND.md` discusses extending Heist Scheme with C++17, embedding enables 
extending C++17 with Heist Scheme! Refer to `examples/embedded_heist_demo.cpp` to see
Heist Scheme being embedded in action!




## How Embed?

### Prerequisite Knowledge for Embedding Heist Scheme in C++17

Before getting into embedding Heist Scheme in C++ though, you'll first want to learn 
about the underlying C++ type system of Heist Scheme objects. You can learn about this 
type system in `TYPES.md`. 

Should you want more information about working with Heist objects of a particular type, 
I'd recommend perusing `lib/primitives/stdlib/data` for extensive examples of C++ 
functions interoperating with the types described in `TYPES.md`.


### Actually Embedding

Programs embedding Heist Scheme must `#include` the `interop.hpp` header.<br>
This defines 3 functions (designed for use in single-threaded environments) in the `heist` namespace:

1. `heist::data eval(std::string exp)`
   * Evaluate `exp` as Heist Scheme code.
     - Returns the result of the evaluation.
     - Returns `*exit-success*` or `*exit-failure*` as a _symbol_ if Heist's `exit` gets
       triggered as a result of evaluating `exp`
     - Note this can be called on string literals by using the `_heist` suffix, IE:
       `auto thricePi = "(* 3 (acos -1))"_heist;`

2. `heist::data apply(std::string heist_procedure_name, std::vector<heist::data> args)`
   * Apply `args` to `heist_procedure_name` & return the result
   * Alternatively, can pass a `heist::data` callable instead of a `std::string` procedure name

3. `define`
   * Comes in 2 flavors:
     1. `void define(std::string heist_variable_name, heist::data variable_value)`
        * Binds `heist_variable_name` to `variable_value` in Heist's global environment
     2. `void define(std::string primitive_name, heist::prm_ptr_t heist_primitive_function_ptr)`
        * Binds `heist_primitive_function_ptr` as a C++ primitive to `primitive_name` in Heist's
          global environment
          - Note that `heist::prm_ptr_t` is equivalent to `heist::data(*)(std::vector<heist::data>&&)`
            * See `EXTEND.md` for more info on extending Heist Scheme with your own C++ primitives!
        * Pass a an extra `true` variable to the end of this function to have the calling environment's pointer<br>
          appended the end of `heist_primitive_function_ptr`'s arg-list when applied by Heist
          - This enables a form of dynamic scoping, as the environment pointer can in turn be passed
            to `primitive_toolkit::apply_callable`
          - Again, see `EXTEND.md` for info on implementing your own C++ primitives for Heist Scheme!
