// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for characters.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_CHARACTERS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_CHARACTERS_IMPLEMENTATION_HPP_

namespace heist::stdlib_characters {

  void confirm_only_chars_and_at_least_one_arg(const data_vector& args, const char* name) {
    if(args.empty())
      HEIST_THROW_ERR('\'' << name << " received no arguments!\n     (" 
        << name << " <char> ...)" << HEIST_FCN_ERR(name,args));
    for(const auto& arg : args)
      if(!arg.is_type(types::chr))
        HEIST_THROW_ERR('\'' << name << " received non-char argument: "
        << HEIST_PROFILE(arg) << "!\n     (" << name << " <char> ...)" << HEIST_FCN_ERR(name,args));      
  }


  void confirm_given_one_char_arg(const data_vector& args, const char* name) {
    if(args.size() != 1 || !args[0].is_type(types::chr))
      HEIST_THROW_ERR('\'' << name << " didn't receive exactly 1 character arg!"
        << "\n     (" << name << " <char>)" << HEIST_FCN_ERR(name,args));
  }

} // End of namespace heist::stdlib_characters

#endif