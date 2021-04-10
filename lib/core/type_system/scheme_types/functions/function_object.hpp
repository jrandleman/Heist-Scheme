// Author: Jordan Randleman -- jrandleman@scu.edu -- function_object.hpp
// => Contains "function_object" & "param_stats" data structures for the C++ 
//    Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_FUNCTION_OBJECT_HPP_
#define HEIST_SCHEME_CORE_FUNCTION_OBJECT_HPP_

namespace heist {

  /******************************************************************************
  * HELPER PROCEDURES
  ******************************************************************************/

  // From "lib/core/type_system/scheme_types/process.hpp"
  bool symbol_is_dot_operator(const string&)noexcept;

  // From "lib/core/reader/parser.hpp"
  bool string_begins_with(const string&, const char*, size_type)noexcept;

  /******************************************************************************
  * FUNCTION PARAMETER STATISTICS OBJECT
  ******************************************************************************/

  struct param_stats {
    bool non_nullary_params() const noexcept{return flags & 1;} // requires 1+ args
    bool nullary_params()     const noexcept{return flags & 2;} // CAN accept 0 args
    bool cps_variadic_params()const noexcept{return flags & 4;}
    bool variadic_params()    const noexcept{return flags & 8;}

    size_type mandatory_number_of_args()const noexcept{return mandatory_arg_number;}

    param_stats& operator=(const param_stats& p) = default;
    param_stats& operator=(param_stats&& p)      = default;

    param_stats()                     = default;
    param_stats(const param_stats& p) = default;
    param_stats(param_stats&& p)      = default;
    param_stats(const str_vector& params)noexcept{
      set_non_nullary_params(!params.empty() && !(params.size()==2 && symbol_is_dot_operator(params[0])));
      set_nullary_params(params.empty());
      set_cps_variadic_params(params.size() > 2 && string_begins_with(params[params.size()-1],symconst::continuation,0) && symbol_is_dot_operator(params[params.size()-3]));
      set_variadic_params((params.size() > 1 && symbol_is_dot_operator(params[params.size()-2])) || cps_variadic_params());
      mandatory_arg_number = params.size() > 1 ? params.size() - 2 - cps_variadic_params() : 0;
    }

  private:
    size_type mandatory_arg_number = 0;
    unsigned char flags = 0;
    void set_non_nullary_params(const bool status) noexcept{if(status) flags |= 1; else flags &= ~1;}
    void set_nullary_params(const bool status)     noexcept{if(status) flags |= 2; else flags &= ~2;}
    void set_cps_variadic_params(const bool status)noexcept{if(status) flags |= 4; else flags &= ~4;}
    void set_variadic_params(const bool status)    noexcept{if(status) flags |= 8; else flags &= ~8;}
  };

  /******************************************************************************
  * FUNCTION OBJECT
  ******************************************************************************/

  struct function_object {

    // Type alias for the current recursive depth
    using depth_type = tgc_ptr<size_type,0>;

    
    // Type alias for params/stats pair
    using params_type = std::pair<data_vector,param_stats>;


    // General invariants (applies to both primitives & compound procedures)
    string name; // name == "" denotes an anonymous procedure
    std::vector<params_type> param_instances;


    // Primitive function pointer invariant
    prm_ptr_t prm = nullptr;


    // Compound function invariants
    std::vector<exe_fcn_t> bodies;
    env_type env = nullptr ;
    obj_type self = nullptr;
    depth_type rec_depth = nullptr;
    unsigned char flags = 1; // is_lambda (as opposed to 'fn) | using_dynamic_scope | is_cps_procedure [only lambda by default]


    // Constructors
    function_object() = default;
    // primitive ctors
    function_object(const data_vector& a, const prm_ptr_t& p)noexcept:prm(p) { // partial primitive
      param_instances.push_back(std::make_pair(a,param_stats()));
    }
    function_object(const string& n, const prm_ptr_t& p)noexcept:name(n),prm(p) {} // primitive
    // tail call wrapper ctor (gets returned up)
    function_object(env_type& e,const exe_fcn_t& b)noexcept:env(e){bodies.push_back(b);}
    // lambda ctor
    function_object(const params_type& p,const exe_fcn_t& b,env_type& e,const string& n)noexcept:
      name(n),param_instances(1,p),bodies(1,b),env(e),rec_depth(depth_type(size_type(0))){}
    // fn ctor
    function_object(const std::vector<params_type>& ps,const std::vector<exe_fcn_t>& bs,env_type& e,const string& n)noexcept:
      name(n),param_instances(ps),bodies(bs),env(e),rec_depth(depth_type(size_type(0))),flags(0){}
    function_object(const function_object& f)noexcept{*this = f;}
    function_object(const function_object&& f)noexcept{*this = std::move(f);}


    // Assignment
    void operator=(const function_object& f)noexcept;
    void operator=(function_object&& f)noexcept;


    // Equality
    bool operator==(const function_object& f)const noexcept;


    // Primitive/Compound Predicates
    bool is_primitive()const noexcept{return prm;};
    bool is_compound() const noexcept{return !prm;};


    // Method "self" extension
    function_object& bind_self(obj_type& self)noexcept{this->self = self; return *this;}


    // Stringification
    string str()const noexcept{if(name.empty()) return "#<procedure>"; return "#<procedure " + name + '>';}
    string printable_procedure_name()const noexcept{if(name.empty()) return "#<procedure>"; return name;}


    // Compound procedure invariant manipulation
    bool is_lambda()const noexcept{return flags & 1;}
    void set_lambda(const bool status)noexcept{if(status) flags |= 1; else flags &= ~1;}
    bool is_using_dynamic_scope()const noexcept{return flags & 2;}
    void set_using_dynamic_scope(const bool status)noexcept{if(status) flags |= 2; else flags &= ~2;}
    bool is_cps_procedure()const noexcept{return flags & 4;}
    void set_cps_procedure(const bool status)noexcept{if(status) flags |= 4; else flags &= ~4;}
    size_type& recursive_depth()noexcept{return *rec_depth;} // PRECONDITION: rec_depth
    size_type recursive_depth()const noexcept{return *rec_depth;} // PRECONDITION: rec_depth


    // Environmental extension for compound procedures
    str_vector lambda_parameters()const;
    env_type get_extended_environment(data_vector& arguments,exe_fcn_t& body,const bool applying_in_cps);
  };
}

#endif