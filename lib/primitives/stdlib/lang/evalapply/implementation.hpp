// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for evalapply.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_EVALAPPLY_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_EVALAPPLY_IMPLEMENTATION_HPP_

/******************************************************************************
* HELPER PROTOTYPE TO CONVERT DATA TO CPS-STYLE
******************************************************************************/

namespace heist {
  // Convert <data> into CPS-syntax syntax.
  // From "lib/core/evaluator/dependancies/cps/expander.hpp"
  data_vector generate_fundamental_form_cps(const data& code,const bool topmost_call=true,const bool core_unexpanded=true);
}

/******************************************************************************
* VALIDATION
******************************************************************************/

namespace heist::stdlib_eval_apply {
  
  void eval_confirm_correct_number_of_args(data_vector& args, bool& must_reset_global_env, 
                                           env_type& env, const char* name, const char* format){
    if(args.size()==2 && args[1].is_type(types::sym)) {
      if(args[1].sym == symconst::null_env) {
        must_reset_global_env = true, args.pop_back();
      } else if(args[1].sym == symconst::global_env) {
        env = G.GLOBAL_ENVIRONMENT_POINTER, args.pop_back();
      } else if(args[1].sym == symconst::local_env) {
        args.pop_back(); // *local-environment* is default
      } else { 
        HEIST_THROW_ERR('\''<<name<<" \""<<args[1].sym<<"\" isn't an evaluation environment:" << format << HEIST_FCN_ERR(name, args));
      }
    }
    // confirm the correct # of arguments were passed
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of arguments:" << format << HEIST_FCN_ERR(name, args));
  }

} // End of namespace heist::stdlib_eval_apply

#endif