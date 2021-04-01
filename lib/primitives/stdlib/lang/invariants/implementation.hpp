// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for invariants.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_INVARIANTS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_INVARIANTS_IMPLEMENTATION_HPP_

namespace heist::stdlib_invariants {

  /******************************************************************************
  * VALIDATION
  ******************************************************************************/

  void confirm_no_args_given(const data_vector& args, const char* name) {
    if(!args.empty())
      HEIST_THROW_ERR('\''<<name<<" doesn't accept any args!\n     ("<<name<<')'<<HEIST_FCN_ERR(name,args));
  }

  /******************************************************************************
  * SETTINGS TOGGLING
  ******************************************************************************/

  data primitive_TOGGLE_BOOLEAN_SETTING(data_vector& args, const char* name, bool& setting){
    if(args.size() > 1)
      HEIST_THROW_ERR('\'' << name << " received incorrect # of args:\n     ("
        << name << " <optional-bool>)" << HEIST_FCN_ERR(name,args));
    bool original_setting_status = setting;
    setting = true;
    if(!args.empty()) setting = args[0].is_truthy();
    return boolean(original_setting_status);
  }


  data primitive_TOGGLE_NUMERIC_SETTING(data_vector& args, const char* name, size_type& SETTING){
    if(args.size() != 1 || !args[0].is_type(types::num) || 
       !args[0].num.is_integer() || !args[0].num.is_pos())
      HEIST_THROW_ERR('\''<<name<<" didn't receive a positive integer arg:"
        "\n     ("<<name<<" <positive-integer>)"
        "\n     <positive-integer>: (0, " << GLOBALS::MAX_SIZE_TYPE << ']'
        << HEIST_FCN_ERR(name,args));
    auto float_num = args[0].num.to_inexact();
    if(float_num < 0 || float_num > GLOBALS::MAX_SIZE_TYPE)
      HEIST_THROW_ERR('\''<<name<<" integer arg is out of bounds!"
        "\n     ("<<name<<" <positive-integer>)"
        "\n     <positive-integer>: (0, " << GLOBALS::MAX_SIZE_TYPE << ']'
        << HEIST_FCN_ERR(name,args));
    // Set the new setting value, & return the original
    auto original = SETTING;
    SETTING = (size_type)float_num.extract_inexact();
    return num_type(original);
  }

} // End of namespace heist::stdlib_invariants

#endif