// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for numerics.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_NUMERICS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_NUMERICS_IMPLEMENTATION_HPP_

namespace heist::stdlib_numerics {

  void confirm_only_numbers_and_at_least_one_arg(const data_vector& args, const char* name, const char* format) {
    if(args.empty())
      HEIST_THROW_ERR('\'' << name << " received no arguments!\n     "
        << format << HEIST_FCN_ERR(name,args));
    for(const auto& arg : args)
      if(!arg.is_type(types::num))
        HEIST_THROW_ERR('\'' << name << " received non-numeric argument: "
        << HEIST_PROFILE(arg) << "!\n     " << format << HEIST_FCN_ERR(name,args));      
  }


  void confirm_only_reals_and_at_least_one_arg(const data_vector& args, const char* name, const char* format) {
    if(args.empty())
      HEIST_THROW_ERR('\'' << name << " received no arguments!\n     "
        << format << HEIST_FCN_ERR(name,args));
    for(const auto& arg : args)
      if(!arg.is_type(types::num) || !arg.num.is_real())
        HEIST_THROW_ERR('\'' << name << " received non-real-numeric argument: "
          << HEIST_PROFILE(arg) << "!\n     " << format << HEIST_FCN_ERR(name,args));
  }


  void confirm_2_args(const data_vector& args, const char* name, const char* format){
    if(args.size() != 2)
      HEIST_THROW_ERR('\'' << name << " didn't receive two arguments (given "
        << args.size() << ")!\n     " << format << HEIST_FCN_ERR(name,args));
  }


  void confirm_unary_numeric(const data_vector& args, const char* name, const char* format){
    if(args.size() != 1 || !args[0].is_type(types::num)) 
      HEIST_THROW_ERR('\'' << name << " didn't receive exactly 1 numeric argument!"
        "\n     " << format << HEIST_FCN_ERR(name,args));
  }


  void confirm_unary_real_numeric(const data_vector& args, const char* name, const char* format){
    if(args.size() != 1) 
      HEIST_THROW_ERR('\'' << name << " didn't receive 1 argument (given "
        << args.size() << ")!\n     " << format << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::num) || !args[0].num.is_real())
      HEIST_THROW_ERR('\'' << name << " received non-real-numeric argument: "
        << HEIST_PROFILE(args[0]) << "!\n     " << format << HEIST_FCN_ERR(name,args));
  }


  num_type factorial(num_type&& n, num_type&& p)noexcept{
    if(n < 2) return std::move(p);
    return factorial(n-1,n*p);
  }


  void confirm_valid_rounding_args(const data_vector& args, const char* name, const char* format){
    if(args.size() != 1 && args.size() != 2) 
      HEIST_THROW_ERR('\'' << name << " didn't receive 1 or 2 arguments (given "
        << args.size() << ")!\n     " << format << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::num) || !args[0].num.is_real())
      HEIST_THROW_ERR('\'' << name << " received non-real-numeric argument: "
        << HEIST_PROFILE(args[0]) << "!\n     " << format << HEIST_FCN_ERR(name,args));
    if(args.size() == 2 && (!args[1].is_type(types::num) || !args[1].num.is_real() || args[1].num.is_neg()))
      HEIST_THROW_ERR('\'' << name << " 2nd argument (precision) isn't a non-negative real number: "
        << HEIST_PROFILE(args[1]) << "!\n     " << format << HEIST_FCN_ERR(name,args));
  }

} // End of namespace heist::stdlib_numerics

#endif