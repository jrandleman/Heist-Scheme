// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for filesystem.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_FILESYSTEM_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_FILESYSTEM_IMPLEMENTATION_HPP_

namespace heist::stdlib_filesystem {

  void confirm_given_one_string_arg(const data_vector& args, const char* name, const char* format) {
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" didn't receive exactly 1 string!" << format << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[0])<<" isn't a string!" << format << HEIST_FCN_ERR(name,args));
  }


  bool is_path(const string& s)noexcept{
    try {
      return std::filesystem::exists(s);
    } catch(...) {
      return false;
    }
  }


  bool is_directory(const string& s)noexcept{
    try {
      return std::filesystem::is_directory(s);
    } catch(...) {
      return false;
    }
  }


  bool is_file(const string& s)noexcept{
    return is_path(s) && !is_directory(s);
  }

} // End of namespace heist::stdlib_filesystem

#endif