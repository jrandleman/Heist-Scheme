# Understanding [`lib/core/type_system`](../lib/core/type_system)
## The C++ Types Underlying "Heist Scheme Objects"

--------------------------------
## [`dependancies`](../lib/core/type_system/dependancies)

Directory containing functionality support for Heist Scheme `struct data` objects.
  * `struct data` is defined in [`lib/core/type_system/scheme_types/data`](../lib/core/type_system/scheme_types/data) (see below).

Includes the [garbage collector](../lib/core/type_system/dependancies/garbage_collector.hpp), 
[error-handling macro definitions](../lib/core/type_system/dependancies/error_handling.hpp), 
[most global variables](../lib/core/type_system/dependancies/process_independent_global_variables.hpp), 
and [predefined symbolic constants](../lib/core/type_system/dependancies/symbolic_constants.hpp).<br>
However, the latter two are only used internally, and hence can be largely ignored 
by those extending the Heist Scheme interpreter. 
  * The only thing to keep in mind here is that the `heist::GLOBALS` namespace is
    defined in [`dependancies/process_independent_global_variables.hpp`](../lib/core/type_system/dependancies/process_independent_global_variables.hpp), 
    should you ever encounter such and wonder where it's defined.

The garbage collector is simply a smart-pointer class that effectively provides 
cycle-safe shared pointers.<br>
As such, this is less of a true "garbage collector" and more of a souped-up
reference-counting mechanism.
* The pointer class is called `tgc_ptr` (typed garbage collection pointer), and is 
  used as follows:
  - Allocate and initialize a cycle-safe pointer: `auto p = tgc_ptr<typename>(value)` 
  - Allocate and initialize a cycle-unsafe pointer: `auto p = tgc_ptr<typename,0>(value)`
    * If not given a `value`, `tgc_ptr`s are equivalent to `nullptr` by default.
    * Cycle-unsafe pointers are effectively `std::shared_ptr`.<br>
      Forsaking cycle-safety creates faster/"thinner" pointers, so such is simply an 
      optimization technique.
      - For example, Heist strings are cycle-unsafe pointers under the hood, since they 
        never to produce cyclical references.
      - However, environment pointers are cycle-safe, since closures can cause cyclical 
        environment references.
      - Heist uses these smart pointers to create objects with "reference semantics".

Error handling is done by macros to get access to C++'s `__FILE__`, `__LINE__`, and 
`__func__` for clearer error messages.<br>
This is principally done by `HEIST_THROW_ERR(err_message)`.
* The `err_message` arg is passed to `std::cerr`, hence `HEIST_THROW_ERR("not a string: " << 42.0)` 
  is valid.<br>As implied, this macro does `throw` a C++ exception, and hence should ***NEVER*** 
  be used in a `noexcept` context.
  - Note that `HEIST_THROW_ERR` automatically applies ANSI colors to the printed error message, 
    so long as `-nansi`'s inactive.
  - These errors cancel individual REPL expression evaluations, and terminate scripts.




--------------------------------
## [`scheme_types`](../lib/core/type_system/scheme_types)

Directory containing the definition and implementation of the Heist `struct data` object, 
as well as those of the underlying types associated with `struct data`'s composite values.

`struct data` is defined in `scheme_types/data/data.hpp`, and uses a `union` under the hood 
as an efficient means of storing a wide set of value types, the aliases of which can be found 
in [`lib/core/type_system/types.hpp`](../lib/core/type_system/types.hpp) (discussed below). 

Type headers found directly in the [`scheme_types`](../lib/core/type_system/scheme_types) 
directory are simple enough such that their definition and implementation could be trivially 
fit into a single file. Larger types, such as `data` and `function_object`, are put in their 
own subdirectories. Each of these subdirectories will contain a definition file (usually named 
after the type it defines) and an implementation file (`implementation.hpp`). Occasionally, a 
type may also define another subdirectory as well in order to hold supplemental library code. 

While the perusal of _all_ these type definition files would certainly enrich one's capacity 
to understand and interoperate with Heist Scheme directly from C++, such is a bit overkill 
-- for example -- should you only want to extend Heist Scheme with custom C++ primitives for
a subset of Heist's type system. 

Luckily, no such redundancy is required -- so long as one carefully reviews the definition 
of `struct data` in [`/scheme_types/data/data.hpp`](../lib/core/type_system/scheme_types/data/data.hpp) 
(the core Heist object) in conjunction with the definitions of their chosen subset of types, 
they'll be able to quickly become productive in writing C++ to extend Heist Scheme's features.


Note: In addition to reviewing type definitions, type implementation files also serve as 
good mechanisms by which to gain exposure to Heist Scheme's underlying facilities.

Note: More information on extending Heist with C++ primitives can be found in [`EXTEND.md`](./EXTEND.md)!
  * See [`EMBED.md`](./EMBED.md) for information on embedding Heist Scheme as a scripting language in C++17!


### Exploring `struct data`:

Heist's `data` object is designed such that types are represented by an enum, with the alias 
of each enum "type" having a corresponding value member name in the `data` object. As such, 
`data_obj.is_type(heist::types::str)` denotes that the `str` member is active in `data_obj`'s
internal union. Accessing a different member name in `data_obj` than that which is designated 
by `data_obj.type` causes undefined behavior. The `heist::types` enumeration may be found in 
`/scheme_types/data/data.hpp`.

Additional properties of `struct data` include:

  0. `.is_type(const heist::types&)`:<br>
     Referenced above, this methods serves as a readable means by which to validate a `data`
     object's current type. Note that the current type may be accessed via the `.type` member.
  1. `.copy()` & `.shallow_copy()`:<br>
      Deep & shallow copying. Shallow copying is sometimes refered to as "structural" copying 
      since its approach relies on reallocating the base data-structure representing any given 
      `data` object, then copying all of said object's member values. "Deep-copying" simply 
      recursively reallocates data structures while copying atomic values. 
  2. `.write()`, `.display()`, `.pprint()`:<br>
      Stringify a `data` object into a machine-readable, human-readable, or machine-readable
      (& auto-indented) `std::string`.
      * Note that these will also automatically invoke `defclass` object polymorphism as needed!
        - I.E. if an object has a `self->string` methods defined, such will be invoked if a `data`
          object containing the object's pointer invokes any of the above methods.
  3. `.eq()`, `.eqv()`, `.equal()`:<br>
     Shallow, structural, and deep equality. Ideally, `.eq()` is a pointer comparison, whereas
     `.eqv()` will recurse through a structure once, only to compare each of the members of the 
     structure with `.eq()`. `.equal()`, however, continuously recursively parses data structures 
     until it only reaches atomics to compare against using `.eq()`.
     * Note that, as with the above stringification methods, these will also automatically invoke 
       `defclass` object polymorphism for their associated method names (as well as `self=`).
     * Note that these methods are why the `data` object doesn't support `operator==`: as since 
       scheme doesn't have only one notion of equality (and by extension identity), it doesn't 
       make sense to impose such on the type system while interpreting the language.
  4. `.is_falsey()`, `.is_truthy()`:<br>
     Determine the falsey & truthy semantics of any given `data` object.
     * By default, only boolean false (`#f`) is false, whereas everything else is true. 
       - However, Heist allows users to "falsify" values in order to register them as a "fail" in 
         conditional branch statements. 
       - See the `set-falsey!`, `set-truthy!`, `falsey-values` primitives for more information!
       - Note that boolean false `#f` can **never** be registered as truthy, nor may boolean true 
         `#t` **ever** be registered as falsey.

Remember to check out any file in the `/lib/primitives/stdlib` directory for numerous examples of 
`data` objects being used!




--------------------------------
## [`types.hpp`](../lib/core/type_system/types.hpp)

The type system's wrapper header file. Contains the vast majority of Heist's standard library 
inclusions, all of its `dependancies` and `type_system` inclusions, as well as the type aliases 
referenced by `struct data` in `/scheme_types/data/data.hpp`.

Also contains functionality to easily create smart pointers (wrappers around `tgc_ptr`'s 
constructors) and performs platform-identification at preprocessor-time.


### Type Aliases:

#### ___Atomics:___

  0. `heist::num_type`: 
     * Numeric alias, see `scm_numeric::Snum` in `lib/core/type_system/scheme_types/numerics/complex.hpp`
  1. `heist::chr_type`:
     * Character alias, equivalent to `int` to represent `EOF` as a character
  2. `heist::sym_type`:
     * Symbol alias, equivalent to `std::string`
  3. `heist::bol_type`
     * Boolean alias, see `struct boolean` in `lib/core/type_system/scheme_types/boolean.hpp`
  4. `heist::syn_type`
     * Syntax-rule-object alias, see `struct syntax_rules_object` in 
       `lib/core/type_system/scheme_types/syntax_rules_objects/syntax_rules_object.hpp`
  5. `heist::fip_type`:
     * File input port alias, see `struct iport` in `lib/core/type_system/scheme_types/ports/port.hpp`
  6. `heist::fop_type`:
     * File output port alias, see `struct oport` in `lib/core/type_system/scheme_types/ports/port.hpp`


#### ___Containers:___

  0. `heist::str_type`:
     * String alias, a `tgc_ptr` to `std::string`
  1. `heist::par_type`:
     * Pair alias, a `tgc_ptr` to `std::pair<struct data,struct data>`
  2. `heist::vec_type`:
     * Vector alias, a `tgc_ptr` to `std::vector<struct data>`
  3. `heist::map_type`:
     * Hash-map (hmap) alias, a `tgc_ptr` to `struct map_object`
       - See `lib/core/type_system/scheme_types/map_object.hpp`!


#### ___OO:___

  0. `heist::cls_type`:
     * Class prototype alias, `tgc_ptr` to `struct class_prototype`
       - See `lib/core/type_system/scheme_types/class_prototype.hpp`!
  1. `heist::obj_type`:
     * Object alias, `tgc_ptr` to `struct object_type`
       - See `lib/core/type_system/scheme_types/objects/object_type.hpp`!


#### ___Code Wrappers:___

  0. `heist::del_type`:
     * Delayed expression alias, `tgc_ptr` to `struct delay_object`
       - See `lib/core/type_system/scheme_types/delay_object.hpp`!
  1. `heist::fcn_type`:
     * Function object alias, equivalent to `struct function_object`
       - See `lib/core/type_system/scheme_types/functions/function_object.hpp`!


#### ___Special Case: Valueless Types!___

These are type enums that have no associated value member in `struct data`'s internal `union`,
rather they denote a single, unique, & immutable value designed for special treatment:

  0. `heist::types::undefined`:
     * `struct data`'s default type, denoting an indeterminate value
     * Create an undefined datum via the following: `heist::data()`
  1. `heist::types::dne`:
     * Denotes the `void` value, with "dne" having been chosen to stand for "does not exist"
     * Create a void datum via the following: `heist::data(heist::types::dne)`


#### ___Special Case: Internal Types!___

These are type aliases designed mostly for internal use, though understanding them is important
when implementing serious extensions to Heist's "kernel" in `lib/core/evaluator/evaluator.hpp`:

  0. `heist::env_type`:
     * Environment pointer alias. Passable after `args` to `primitive_toolkit::apply_callable`.
       - `tgc_ptr` to `struct environment`, who's definition can be found in
         `lib/core/type_system/scheme_types/environments/environment.hpp`
  1. `heist::exp_type`:
     * Syntax expression alias, equivalent to `std::vector<struct data>`
       - Used to denote "list literals" read in by the reader prior evaluation
  2. `heist::prc_type`:
     * Process invariant set: used internally by `universe` objects.
       - `tgc_ptr` to `struct process_invariants_t`, who's definition can be found in
         `lib/core/type_system/scheme_types/process.hpp`


### Common Container `tgc_ptr` Constructor Wrappers:

  0. Strings: `str_type make_str(const std::string&)`
  1. Pairs: `par_type make_par()`
  2. Vectors: `vec_type make_vec(const std::vector<struct data>&)`
  3. Hmaps: `map_type make_map(const struct map_object&)`
