// Author: Jordan Randleman -- jrandleman@scu.edu -- delay_object.hpp
// => Contains "delay_object" data structure for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_DELAY_OBJECT_HPP_
#define HEIST_SCHEME_CORE_DELAY_OBJECT_HPP_

namespace heist {
  struct delay_object {
    data datum;
    env_type env;
    bool already_forced, in_cps;
    delay_object(const delay_object& d) = default;
    delay_object(delay_object&& d)      = default;
    delay_object(const data& delayed_datum = data(), env_type delay_env = nullptr, bool cps = false) noexcept
      : datum(delayed_datum), env(delay_env), already_forced(false), in_cps(cps) {}
    data force() { // Memoize delays, "call by need" evaluation
      exe_fcn_t scm_analyze(data&& datum,const bool tail_call=false,const bool cps_block=false);
      if(!already_forced) {
        already_forced = true;
        datum = scm_analyze(std::move(datum),false,in_cps)(env);
      }
      return datum;
    }
  };
}

#endif