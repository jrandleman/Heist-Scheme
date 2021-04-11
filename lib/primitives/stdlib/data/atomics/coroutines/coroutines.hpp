// Author: Jordan Randleman -- jordanran199@gmail.com -- coroutines.hpp
// => Defines the primitive "cycle-coroutines!" function written in 
//    C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_COROUTINES_HPP_
#define HEIST_SCHEME_CORE_STDLIB_COROUTINES_HPP_

#include "implementation.hpp"

/***
 * NOTE:
 *   Similar to "universe"s, coroutines as constructs are actually entirely defined 
 *   directly in Heist Scheme, and as such, the interpreter only ever sees them as just 
 *   being "any other object".
 *
 *   The only reason we define "cycle-coroutines!" in C++ is due to Heist Scheme's design
 *   philosophy, namely that as many primitives as possible should be implemented in C++ 
 *   in order to reduce run-time overhead.
 *
 *   Indeed, implementing "cycle-coroutines!" in Heist Scheme directly is trivial:
 *
 *      (define (cycle-coroutines! . coroutines)
 *        (define (cycle coroutines)
 *          (cycle (map (lambda (c) (if (coroutine? c) (c.next) (jump! c)))
 *                      coroutines)))
 *        (catch-jump cycle coroutines))
 *
 */

namespace heist {

  // WARNING: If none of the cycled coroutines terminate, neither will this procedure!

  data primitive_CYCLE_COROUTINES_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (cycle-coroutines! <coroutine-1> <coroutine-2> ...)";
    auto coro_proto = stdlib_coroutines::get_coroutine_class_prototype(args,format);
    size_type i = 0, n = args.size();
    for(;;) {
      for(i = 0; i < n; ++i) {
        if(!stdlib_coroutines::datum_is_a_coroutine(args[i],coro_proto)) return args[i];
        args[i] = stdlib_coroutines::invoke_coroutine_NEXT_method(args[i],format);
      }
    }
    return data(); // never triggered
  }

} // End of namespace heist

#endif