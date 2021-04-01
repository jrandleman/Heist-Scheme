# Extending Heist Scheme with C++ & Scheme Primitives

## Why Extend?

No matter how fast Heist Scheme is or gets, there's no escaping that it's a dynamically-typed
interpreted language. While this is hardly an issue for scripting, it does mean that Heist
Scheme -- as with JavaScript and Python -- can chug when it comes to CPU-intensive programs.

As such, it can be fruitful to implement performance-sensitive functions as C++ primitives
that Heist Scheme interoperates with (similar to TensorFlow's use in the Python community).

In addition to C++ primitives though, Heist can also be extended with _Scheme primitive files!_
Scheme primitive files differ from regular Scheme files in that they are automatically loaded
by Heist when initializing a global environment -- such as when starting a new Heist session,
evaluating code in a `*null-environment*`, or creating a new `universe` object!

The only caveat to Scheme primitive files is that they must swear a solemn oath to _never_
trigger a C++ exception ___while being loaded___ (this includes reader errors, malformed core syntax 
invocations, and uncaught `jump!` statements). A C++ exception in a Scheme primitive file 
means that the global environment can't be initialized -- and without a global environment, 
you don't have a runtime -- hence Heist will yell at you and cease interpretation.




## How Extend?

The good news when it comes to extending Heist Scheme is that it's relatively simple to do so -- 
in fact, _every standard library primitive is implemented as if it were an extension to Heist!_


### Prerequisite Knowledge for C++ Primitives

Before getting into extending Heist Scheme with C++ primitives though, you'll first want to 
learn about the underlying C++ type system of Heist Scheme objects. You can learn about this
type system in [`TYPES.md`](https://github.com/jrandleman/Heist-Scheme/blob/master/docs/TYPES.md). 

Should you want more information about working with Heist objects of a particular type, I'd
recommend perusing [`lib/primitives/stdlib/data`](https://github.com/jrandleman/Heist-Scheme/tree/master/lib/primitives/stdlib/data) 
for extensive examples of C++ primitives interoperating with the types described in [`TYPES.md`](https://github.com/jrandleman/Heist-Scheme/blob/master/docs/TYPES.md).


### Writing C++ Primitives

C++ primitives must have the following type signature: `heist::data(std::vector<heist::data>&&)`.<br>
If those types don't make sense to you, read [`TYPES.md`](https://github.com/jrandleman/Heist-Scheme/blob/master/docs/TYPES.md). 

The vector represents the primitive's "argument list" -- all of Heist's C++ primitves are N-ary, 
meaning that the number (and type!) of arguments being passed has to be validated by you.

C++ primitives extending Heist automatically have access to [`lib/core/type_system/types.hpp`](https://github.com/jrandleman/Heist-Scheme/blob/master/lib/core/type_system/types.hpp), 
which is extensively explored by [`TYPES.md`](https://github.com/jrandleman/Heist-Scheme/blob/master/docs/TYPES.md). 
Additionally, C++ primitives also automatically have access to the [`lib/primitives/primitive_toolkit.hpp`](https://github.com/jrandleman/Heist-Scheme/blob/master/lib/primitives/primitive_toolkit.hpp) 
library, which is designed to aid in the development of C++ extensions to Heist Scheme (it's 
hardly 200 lines of code and well worth the read).

A quick note on C++ macros: while there are macros from the interpreter that can and will leak
into C++ primitive extension files by way of `#include`, they are all prefixed by `HEIST_` and 
hence can easily be avoided when it comes to managing macro name collisions.
  * These "leaked" macros can be found in [`lib/core/type_system/dependancies/error_handling.hpp`](https://github.com/jrandleman/Heist-Scheme/blob/master/lib/core/type_system/dependancies/error_handling.hpp)

A quick note on C++ namespacing: while all of the C++ primitives in [`lib/primitives/stdlib`](https://github.com/jrandleman/Heist-Scheme/blob/master/lib/primitives/stdlib) are 
implemented in the `heist` namespace, this isn't strictly needed, and in fact isn't generally 
recommended for 3rd-party primitives. Rather, you should either always explicitly write out the 
`heist::` prefix, or selectively alias elements of the `heist` namespace (ie `using heist::data;`).


### Registering Primitives

Primitive files are registered in [`lib/primitives/primitives.json`](https://github.com/jrandleman/Heist-Scheme/blob/master/lib/primitives/primitives.json). Take a look at it now!

You'll see that the file contains a single JSON object. This object is a set of primitive
file associations, with filename-string keys and primitive-object values. 

An empty object or `null` value for a filename key indicates that the filename is that of a 
Scheme primitive file. C++ primitive files have another object as their value, this time of 
C++ name keys & Scheme name values -- with the C++ name indicating the name of the primitive 
that can be found in the filename, and the Scheme name denoting how such should be registered 
in Heist's global environment.

Simply add you own 3rd-party C++/Scheme primitive file at the end of the JSON object as a new key, 
and adjust its value accordingly. Note that your primitive filename string should be the full 
path to said file on your machine -- [`stdlib.scm`](https://github.com/jrandleman/Heist-Scheme/blob/master/lib/primitives/stdlib/lang/stdlib.scm) 
is a special case handled by the interpreter with some extra logic (since there's no way to know 
the full path to Heist ahead of time on every user's machine!).
  * Note that you want to include your bonus primitives at the end of the JSON object to preserve 
    registration order and not befuddle dependancies -- C++ files are always `#include`d in the
    order they appear in [`primitives.json`](https://github.com/jrandleman/Heist-Scheme/blob/master/lib/primitives/primitives.json), 
    and Scheme files are loaded in similar fashion.

Finally, follow [`INSTALL.md`](https://github.com/jrandleman/Heist-Scheme/blob/master/docs/INSTALL.md) 
to re-install Heist Scheme along with your new primitives, et voila! You've officially extended Heist Scheme!

A quick note on dynamic scope: it can be useful for C++ primitives to have access to the calling 
environment to emulate dynamic scoping (think of `eval`'s `*local-environment*` flag). You can request 
such for your own C++ primitives by prefixing their key in the JSON object with an asterisk `*` (see 
`primitive_EVAL` in [`primitives.json`](https://github.com/jrandleman/Heist-Scheme/blob/master/lib/primitives/primitives.json) 
for an example). This tells Heist to push the calling `env_type` 
environment pointer to the end of the arguments-list when applying your primitive. This environment 
pointer can in turn be passed as an optional 3rd argument to `primitive_toolkit::apply_callable`.
