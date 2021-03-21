// Author: Jordan Randleman -- jrandleman@scu.edu -- port.hpp
// => Contains "iport" & "oport" data structures for the C++ Heist Scheme Interpreter

#ifndef HEIST_PORT_HPP_
#define HEIST_PORT_HPP_

namespace heist {

  // Port getter aliases (implementation in "implementation.hpp")
  FILE* noexcept_get_current_output_port()noexcept;
  FILE* noexcept_get_current_input_port()noexcept;


  // Input port struct (differentiates from 'oport')
  struct iport {
    // Internal port pointer
    using port_ptr_t = tgc_ptr<FILE*,0>;
    port_ptr_t fp = nullptr;
    // Constructors
    iport(FILE* inport = nullptr) {fp = port_ptr_t(inport);}
    iport(const iport& o) = default;
    iport(iport&& o)      = default;
    // Assignment
    iport& operator=(const iport& p) = default;
    iport& operator=(iport&& p)      = default;
    // Equality
    bool operator==(const iport& p)const noexcept{return fp == p.fp;}
    bool operator==(iport&& p)const noexcept{return fp == p.fp;}
    bool operator==(const FILE*& fp)const noexcept{return this->fp && *(this->fp) == fp;}
    // Open Predicate & Close Action
    bool is_open()const noexcept{return fp && *fp;}
    void close()noexcept;
    // Stringification
    string str()const noexcept;
    // Destructor
    ~iport()noexcept;
  };


  // Output port struct (differentiates from 'iport')
  struct oport {
    // Internal port pointer
    using port_ptr_t = tgc_ptr<FILE*,0>;
    port_ptr_t fp = nullptr;
    // Constructors
    oport(FILE* outport = nullptr) {fp = port_ptr_t(outport);}
    oport(const oport& o) = default;
    oport(oport&& o)      = default;
    // Assignment
    oport& operator=(const oport& p) = default;
    oport& operator=(oport&& p)      = default;
    // Equality
    bool operator==(const oport& p)const noexcept{return fp == p.fp;}
    bool operator==(oport&& p)const noexcept{return fp == p.fp;}
    bool operator==(const FILE*& fp)const noexcept{return this->fp && *(this->fp) == fp;}
    // Open Predicate & Close Action
    bool is_open()const noexcept{return fp && *fp;}
    void close()noexcept;
    // Stringification
    string str()const noexcept;
    // Destructor
    ~oport()noexcept;
  };
}

#endif