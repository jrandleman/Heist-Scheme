// Author: Jordan Randleman -- jrandleman@scu.edu -- port.hpp
// => Contains "iport" & "oport" data structures for the C++ Heist Scheme Interpreter

#ifndef HEIST_PORT_HPP_
#define HEIST_PORT_HPP_

namespace heist {
  // Global port registry
  namespace GLOBALS { std::vector<FILE*> PORT_REGISTRY({stdin,stdout}); }


  // Input port struct (differentiates from 'oport')
  struct iport {
    // core logic
    size_type port_idx;
    iport(const size_type& idx = 0) noexcept : port_idx(idx) {}
    iport(const iport& ip) = default;
    iport(iport&& ip)      = default;
    bool is_open() const noexcept {return GLOBALS::PORT_REGISTRY[port_idx] != nullptr;}
    FILE*& port()  const noexcept {return GLOBALS::PORT_REGISTRY[port_idx];}
    // assignment
    iport& operator=(const iport& ip) = default;
    iport& operator=(iport&& ip)      = default;
    // stringification & equality
    string str() const noexcept {return "#<input-port>";}
    bool operator==(const iport& p) const noexcept {return p.port_idx == port_idx;}
  };


  // Output port struct (differentiates from 'iport')
  struct oport {
    // core logic
    size_type port_idx;
    oport(const size_type& idx = 1) noexcept : port_idx(idx) {}
    oport(const oport& op) = default;
    oport(oport&& op)      = default;
    bool is_open() const noexcept {return GLOBALS::PORT_REGISTRY[port_idx] != nullptr;}
    FILE*& port()  const noexcept {return GLOBALS::PORT_REGISTRY[port_idx];}
    // assignment
    oport& operator=(const oport& op) = default;
    oport& operator=(oport&& op)      = default;
    // stringification & equality
    string str() const noexcept {return "#<output-port>";}
    bool operator==(const oport& p) const noexcept {return p.port_idx == port_idx;}
  };
}

#endif