// Author: Jordan Randleman -- jrandleman@scu.edu -- types.hpp
// => Contains type aliases & structures for the C++ Heist Scheme Interpreter

#ifndef TYPES_HPP_
#define TYPES_HPP_

/******************************************************************************
* STANDARD LIBRARY DEPENDANCIES (MORE IN "object_types/numerics/complex.hpp")
******************************************************************************/

#include <algorithm>
#include <climits>
#include <ctime>
#include <filesystem>
#include <functional>
#include <map>
#include <ratio>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

/******************************************************************************
* INTERPRETER-WIDE COMMON TYPE ALIASES
******************************************************************************/

namespace heist {
  using data_vector = std::vector<struct data>;
  using string      = std::string;
  using str_vector  = std::vector<string>;
  using size_type   = std::size_t;
}

/******************************************************************************
* CUSTOM DEPENDANCIES
******************************************************************************/

#include "type_dependancies/symbolic_constants.hpp" // namespace symconst
#include "type_dependancies/garbage_collector.hpp"  // struct tgc_ptr
#include "scheme_types/numerics/complex.hpp"        // struct scm_numeric::Snum

/******************************************************************************
* HEIST-SCHEME-OBJECT INTERNAL TYPE ALIASES
******************************************************************************/

namespace heist {
  using exp_type = data_vector;                                 // expression
  using par_type = tgc_ptr<std::pair<struct data,struct data>>; // pair
  using num_type = scm_numeric::Snum;                           // number (float/int/frac)
  using str_type = tgc_ptr<string>;                             // string
  using chr_type = int;                                         // character ("int" allows EOF to be a char)
  using sym_type = string;                                      // symbol
  using vec_type = tgc_ptr<data_vector>;                        // vector
  using bol_type = struct boolean;                              // boolean
  using env_type = tgc_ptr<struct environment>;                 // evironment
  using del_type = tgc_ptr<struct delay_object>;                // delay
  using fcn_type = struct function_object;                      // procedure (compound & primitive)
  using fip_type = struct iport;                                // file input port
  using fop_type = struct oport;                                // file output port
  using syn_type = struct syntax_rules_object;                  // syntax-rules object
  using map_type = tgc_ptr<struct map_object>;                  // hash-map
  using cls_type = tgc_ptr<struct class_prototype>;             // class-prototype
  using obj_type = tgc_ptr<struct object_type>;                 // object
  using prc_type = tgc_ptr<struct process_invariants_t>;        // process invariants
}

/******************************************************************************
* SCHEME PROCEDURE INTERNAL TYPE ALIASES
******************************************************************************/

namespace heist {
  using prm_ptr_t = struct data(*)(data_vector&);          // primitive procedure ptr
  using exe_fcn_t = std::function<struct data(env_type&)>; // fcn execution procedure
}

/******************************************************************************
* HEIST-SCHEME-OBJECT TYPE DEFINITIONS
******************************************************************************/

#include "scheme_types/boolean.hpp"                                  // struct boolean
#include "scheme_types/port.hpp"                                     // struct iport, struct oport
#include "scheme_types/syntax_rules_objects/syntax_rules_object.hpp" // struct syntax_rules_object
#include "scheme_types/functions/function_object.hpp"                // struct function_object
#include "scheme_types/data/data.hpp"                                // enum class types, struct data
#include "scheme_types/environments/environment.hpp"                 // function create_frame, struct environment
#include "scheme_types/delay_object.hpp"                             // struct delay_object
#include "scheme_types/map_object.hpp"                               // struct map_object
#include "scheme_types/class_prototype.hpp"                          // struct class_prototype
#include "scheme_types/object_type.hpp"                              // struct object_type
#include "scheme_types/process.hpp"                                  // variable G, struct process_invariants_t

/******************************************************************************
* HEIST-SCHEME-OBJECT GC "FACTORY FUNCTIONS"
******************************************************************************/

namespace heist {
  str_type make_str(const string& o)                             noexcept{return str_type(o);}
  str_type make_str(string&& o)                                  noexcept{return str_type(std::move(o));}
  vec_type make_vec(const data_vector& o)                        noexcept{return vec_type(o);}
  vec_type make_vec(data_vector&& o)                             noexcept{return vec_type(std::move(o));}
  del_type make_del(const data& d,const env_type& e, bool in_cps)noexcept{return del_type(delay_object(d,e,in_cps));}
  par_type make_par()                                            noexcept{return par_type(std::pair<struct data,struct data>());}
  map_type make_map(const map_object& m)                         noexcept{return map_type(m);}
  map_type make_map(map_object&& m)                              noexcept{return map_type(std::move(m));}
  env_type make_env()                                            noexcept{return env_type(environment());}
  cls_type make_cls(const class_prototype& c)                    noexcept{return cls_type(c);}
  obj_type make_obj(const object_type& o)                        noexcept{return obj_type(o);}
  obj_type make_obj(object_type&& o)                             noexcept{return obj_type(std::move(o));}
}

/******************************************************************************
* PROCESS-INDEPENDENT GLOBAL VARIABLES
******************************************************************************/

#include "type_dependancies/process_indpendent_global_variables.hpp"

/******************************************************************************
* ERROR-HANDLING MACROS
******************************************************************************/

#include "type_dependancies/error_handling.hpp"

/******************************************************************************
* HEIST-SCHEME-OBJECT TYPE IMPLEMENTATIONS
******************************************************************************/

#include "scheme_types/syntax_rules_objects/implementation.hpp"
#include "scheme_types/functions/implementation.hpp"
#include "scheme_types/environments/implementation.hpp"
#include "scheme_types/data/implementation.hpp"

/******************************************************************************
* HEIST COMMAND-LINE FLAG SET
******************************************************************************/

#define HEIST_COMMAND_LINE_ARGS\
    "> Interpret Script:    -script <script-filename> <argv1> <argv2> ..."\
  "\n> Compile Script:      -compile <script-filename> <optional-compiled-filename>"\
  "\n> Load Script:         -l <script-filename>"\
  "\n> Infix Operators:     -infix"\
  "\n> With CPS Evaluation: -cps"\
  "\n> Disable ANSI Colors: -nansi"\
  "\n> Case Insensitivity:  -ci"\
  "\n> Dynamic Call Trace:  -dynamic-call-trace"\
  "\n> Trace Call Args:     -trace-args"\
  "\n> Stack Trace Size:    -trace-limit <non-negative-integer>"\
  "\n> Interpreter Version: --version"\
  "\n> Show This Message:   --help"

/******************************************************************************
* HEIST PLATFORM IDENTIFICATION
******************************************************************************/

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__) || defined(_WIN64)
  #define HEIST_PLATFORM "windows"
  #ifdef _WIN64
    #define HEIST_EXACT_PLATFORM "windows-64"
  #else
    #define HEIST_EXACT_PLATFORM "windows-32"
  #endif
#elif __APPLE__
  #define HEIST_PLATFORM "apple"
  #include <TargetConditionals.h>
  #if TARGET_IPHONE_SIMULATOR
    #define HEIST_EXACT_PLATFORM "apple-ios-simulator"
  #elif TARGET_OS_IPHONE
    #define HEIST_EXACT_PLATFORM "apple-ios"
  #elif TARGET_OS_MAC
    #define HEIST_EXACT_PLATFORM "apple-osx"
  #else
    #define HEIST_EXACT_PLATFORM "apple"
  #endif
#elif __linux__
  #define HEIST_PLATFORM       "linux"
  #define HEIST_EXACT_PLATFORM "linux"
#elif __unix__
  #define HEIST_PLATFORM       "unix"
  #define HEIST_EXACT_PLATFORM "unix"
#elif defined(_POSIX_VERSION)
  #define HEIST_PLATFORM       "posix"
  #define HEIST_EXACT_PLATFORM "posix"
#else
  #define HEIST_PLATFORM       "unknown"
  #define HEIST_EXACT_PLATFORM "unknown"
#endif
  
#endif