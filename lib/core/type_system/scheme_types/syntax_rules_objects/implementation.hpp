// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Contains implementation of "syntax_rules_object"'s equality for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_SYNTAX_RULES_OBJECT_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_SYNTAX_RULES_OBJECT_IMPLEMENTATION_HPP_

namespace heist {
  bool syntax_rules_object::operator==(const syntax_rules_object& s) const noexcept {
    // Confirm basic parameters match
    if(s.label != label || s.keywords.size() != keywords.size() || 
       s.hashed_template_ids.size() != hashed_template_ids.size() || 
       s.patterns.size() != patterns.size() || s.templates.size() != templates.size()) {
      return false;
    }
    // Confirm keyword set match
    for(size_type i = 0, n = keywords.size(); i < n; ++i) 
      if(s.keywords[i] != keywords[i]) return false;
    // Confirm syntax-hash variable sets match
    for(size_type i = 0, n = hashed_template_ids.size(); i < n; ++i) {
      if(s.hashed_template_ids[i].size() != hashed_template_ids[i].size())
        return false;
      for(size_type j = 0, m = hashed_template_ids[i].size(); j < m; ++j)
        if(s.hashed_template_ids[i][j] != hashed_template_ids[i][j])
          return false;
    }
    // Confirm patterns match
    for(size_type i = 0, n = patterns.size(); i < n; ++i) {
      if(s.patterns[i].size() != patterns[i].size())
        return false;
      for(size_type j = 0, m = patterns[i].size(); j < m; ++j)
        if(!s.patterns[i][j].noexcept_equal(patterns[i][j]))
          return false;
    }
    // Confirm templates match
    for(size_type i = 0, n = templates.size(); i < n; ++i) {
      if(s.templates[i].size() != templates[i].size())
        return false;
      for(size_type j = 0, m = templates[i].size(); j < m; ++j)
        if(!s.templates[i][j].noexcept_equal(templates[i][j]))
          return false;
    }
    return true;
  }
}

#endif