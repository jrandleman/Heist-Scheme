// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for pairs.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_PAIRS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_PAIRS_IMPLEMENTATION_HPP_

namespace heist::stdlib_pairs {

  void confirm_given_a_pair_arg(data_vector& args, const char* name){
    if(args.size() != 1 || !args[0].is_type(types::par))
      HEIST_THROW_ERR('\''<<name<<" didn't receive exactly 1 pair!"
        "\n     ("<<name<<" <pair>)"<<HEIST_FCN_ERR(name,args));
  }


  void confirm_nth_car_is_pair(const data& nth_arg,const char* name,
                               const char* nth,    const data_vector& args){
    if(!nth_arg.par->first.is_type(types::par))
      HEIST_THROW_ERR('\''<<name<<' '<<nth<<" 'car "<<HEIST_PROFILE(nth_arg.par->first)
        <<" isn't a pair!"<<HEIST_FCN_ERR(name,args));
  }


  void confirm_nth_cdr_is_pair(const data& nth_arg,const char* name,
                               const char* nth,    const data_vector& args){
    if(!nth_arg.par->second.is_type(types::par))
      HEIST_THROW_ERR('\''<<name<<' '<<nth<<" 'cdr "<<HEIST_PROFILE(nth_arg.par->second)
        <<" isn't a pair!"<<HEIST_FCN_ERR(name,args));
  }

} // End of namespace heist::stdlib_pairs

#endif