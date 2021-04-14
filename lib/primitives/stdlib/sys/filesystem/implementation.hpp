// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
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


  void confirm_given_two_string_args(const data_vector& args, const char* name, const char* format) {
    if(args.size() != 2)
      HEIST_THROW_ERR('\''<<name<<" didn't receive exactly 2 strings!" << format << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string!" << format << HEIST_FCN_ERR(name,args));
    if(!args[1].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a string!" << format << HEIST_FCN_ERR(name,args));
  }


  // Converts the given path to a std::string. Since Heist only supports ASCII 
  //   encoding for strings, file paths are always assumed to only have ASCII
  //   characters as well (don't get too sendy there Windows, I see you.)
  string coerce_path_to_string(const std::filesystem::path::string_type& s)noexcept{
    return string(s.begin(),s.end());
  }


  auto coerce_string_to_path(const string& s)noexcept{
    return std::filesystem::path::string_type(s.begin(),s.end());
  }


  bool is_path(const string& s)noexcept{
    try {
      return std::filesystem::exists(coerce_string_to_path(s));
    } catch(...) {
      return false;
    }
  }


  bool is_directory(const string& s)noexcept{
    try {
      return std::filesystem::is_directory(coerce_string_to_path(s));
    } catch(...) {
      return false;
    }
  }


  bool is_file(const string& s)noexcept{
    return is_path(s) && !is_directory(s);
  }

} // End of namespace heist::stdlib_filesystem

#endif