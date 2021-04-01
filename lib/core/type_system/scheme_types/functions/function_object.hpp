// Author: Jordan Randleman -- jrandleman@scu.edu -- function_object.hpp
// => Contains "function_object" data structure for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_FUNCTION_OBJECT_HPP_
#define HEIST_SCHEME_CORE_FUNCTION_OBJECT_HPP_

namespace heist {
  struct function_object {

    // Type alias for the current recursive depth
    using depth_t = tgc_ptr<size_type,0>;


    // General invariants (applies to both primitives & compound procedures)
    string name; // name == "" denotes an anonymous procedure
    std::vector<data_vector> param_instances;


    // Primitive function pointer invariant
    prm_ptr_t prm = nullptr;


    // Compound function invariants
    std::vector<exe_fcn_t> bodies;
    env_type env = nullptr ;
    obj_type self = nullptr;
    depth_t rec_depth = nullptr;
    unsigned char flags = 1; // is_lambda (as opposed to 'fn) | using_dynamic_scope | is_cps_procedure [only lambda by default]


    // Constructors
    function_object() = default;
    // primitive ctors
    function_object(const data_vector& a, const prm_ptr_t& p)noexcept:prm(p) {param_instances.push_back(a);} // partial primitive
    function_object(const string& n, const prm_ptr_t& p)noexcept:name(n),prm(p) {}                           // primitive
    // tail call wrapper ctor (gets returned up)
    function_object(env_type& e,const exe_fcn_t& b)noexcept:env(e){bodies.push_back(b);}
    // lambda ctor
    function_object(const data_vector& p,const exe_fcn_t& b,env_type& e,const string& n)noexcept:name(n),env(e),rec_depth(depth_t(size_type(0))){
      param_instances.push_back(p), bodies.push_back(b);
    }
    // fn ctor
    function_object(const std::vector<data_vector>& ps,const std::vector<exe_fcn_t>& bs,env_type& e,const string& n)noexcept:
      name(n),param_instances(ps),bodies(bs),env(e),rec_depth(depth_t(size_type(0))),flags(0){}
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
    void set_lambda(bool status)noexcept{if(status) flags |= 1; else flags &= ~1;}
    bool is_using_dynamic_scope()const noexcept{return flags & 2;}
    void set_using_dynamic_scope(bool status)noexcept{if(status) flags |= 2; else flags &= ~2;}
    bool is_cps_procedure()const noexcept{return flags & 4;}
    void set_cps_procedure(bool status)noexcept{if(status) flags |= 4; else flags &= ~4;}
    size_type& recursive_depth()noexcept{return *rec_depth;} // PRECONDITION: rec_depth
    size_type recursive_depth()const noexcept{return *rec_depth;} // PRECONDITION: rec_depth


    // Environmental extension for compound procedures
    str_vector lambda_parameters()const noexcept;
    env_type get_extended_environment(data_vector& arguments,exe_fcn_t& body,const bool applying_in_cps);
  };
}

#endif