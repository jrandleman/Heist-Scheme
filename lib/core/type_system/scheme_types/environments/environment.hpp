// Author: Jordan Randleman -- jrandleman@scu.edu -- environment.hpp
// => Contains "create_frame" procedure & "environment" data structure 
//    for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_ENVIRONMENT_HPP_
#define HEIST_SCHEME_CORE_ENVIRONMENT_HPP_

namespace heist {

  /******************************************************************************
  * ENVIRONMENT FRAME TYPE ALIASES
  ******************************************************************************/

  using frame_var  = std::string;
  using frame_val  = struct data;
  using frame_mac  = struct data;
  using frame_objs = std::unordered_map<frame_var,frame_val>;
  using frame_macs = std::vector<frame_mac>;
  using frame_type = std::pair<frame_objs,frame_macs>;

  /******************************************************************************
  * ENVIRONMENT DATA STRUCTURE
  ******************************************************************************/

  // Environment Data Type
  struct environment {
    // Invariants
    tgc_ptr<environment> parent = nullptr; // enclosing environment pointer
    frame_type frame;                      // environment's bindings

    // Getters
    frame_objs& objects()noexcept{return frame.first;}
    const frame_objs& objects()const noexcept{return frame.first;}
    frame_macs& macros()noexcept{return frame.second;}
    const frame_macs& macros()const noexcept{return frame.second;}

    // Environmental Traversal & Access
    frame_val lookup_variable_value(const frame_var& var, bool& found)const noexcept;
    bool set_variable_value(const frame_var& var, frame_val&& val)noexcept;
    void define_variable(const frame_var& var, frame_val val)noexcept;
    void define_macro(const frame_mac& mac)noexcept;
    string getenv(const frame_var& var, bool& found)const;
    bool has_macro(const string& label)const noexcept;
    bool has_variable(const frame_var& var)const noexcept;
    bool erase_variable(const frame_var& var)noexcept;
    bool erase_macro(const string& label)noexcept;

  private:
    bool macro_has_label(const frame_mac& mac, const string& label)const noexcept;
  };

  /******************************************************************************
  * ENVIRONMENT FRAME CREATION
  ******************************************************************************/

  // Frame Generation
  frame_type create_frame(const str_vector& vars, data_vector& vals) {
    const size_type n = vars.size();
    frame_objs objects;
    objects.reserve(n);
    for(size_type i = 0; i < n; ++i)
      objects[vars[i]] = vals[i];
    return frame_type(std::move(objects),frame_macs());
  }
}

#endif