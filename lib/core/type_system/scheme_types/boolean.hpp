// Author: Jordan Randleman -- jordanran199@gmail.com -- boolean.hpp
// => Contains "boolean" data structure for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_BOOLEAN_HPP_
#define HEIST_SCHEME_CORE_BOOLEAN_HPP_

namespace heist {
  struct boolean {
    // core logic
    bool val; 
    boolean(const bool b=false)          noexcept : val(b) {}
    boolean(const boolean& b)            noexcept = default;
    boolean(boolean&& b)                 noexcept = default;
    void operator=(const bool b)         noexcept {val=b;}
    boolean& operator=(const boolean& b) noexcept = default;
    boolean& operator=(boolean&& b)      noexcept = default;
    // stringification & equality
    string str() const noexcept {if(val) return "#t"; return "#f";}
    bool operator==(const boolean& b) const noexcept {return b.val == val;}
  };
}

#endif