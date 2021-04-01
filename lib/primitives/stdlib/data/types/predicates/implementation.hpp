// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for predicates.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_TYPE_PREDICATES_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_TYPE_PREDICATES_IMPLEMENTATION_HPP_

namespace heist::stdlib_type_predicates {

  void confirm_given_one_arg(const data_vector& args, const char* name){
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of arguments:"
        "\n     ("<<name<<" <obj>)"<<HEIST_FCN_ERR(name,args));
  }

} // End of namespace heist::stdlib_type_predicates

#endif