// Author: Jordan Randleman -- jordanran199@gmail.com -- copying.hpp
// => Defines the primitive copying functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_COPYING_HPP_
#define HEIST_SCHEME_CORE_STDLIB_COPYING_HPP_

namespace heist {

  // primitive "copy" procedure:
  data primitive_COPY(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'copy received incorrect # of args!\n     (copy <obj>)" 
        "\n     -> Deep-Copy: Vector | String | List/Dotted/Circular | Hmap | Object"
        << HEIST_FCN_ERR("copy", args));
    return args[0].copy();
  }


  // primitive "shallow-copy" procedure:
  data primitive_SHALLOW_COPY(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'shallow-copy received incorrect # of args!\n     (shallow-copy <obj>)" 
        "\n     -> Shallow-Copy: Vector | String | List/Dotted/Circular | Hmap | Object"
        << HEIST_FCN_ERR("shallow-copy", args));
    return args[0].shallow_copy();
  }

} // End of namespace heist

#endif