// Author: Jordan Randleman -- jrandleman@scu.edu -- syntax_rules_object.hpp
// => Contains "syntax_rules_object" data structure for the C++ Heist Scheme Interpreter

#ifndef HEIST_SYNTAX_RULES_OBJECT_HPP_
#define HEIST_SYNTAX_RULES_OBJECT_HPP_

namespace heist {
  struct syntax_rules_object {
    // core logic
    string label;
    str_vector keywords;
    std::vector<str_vector> hashed_template_ids; // hashed_template_ids[i] = syntax-hashed vars of templates[i]
    std::vector<data_vector> patterns, templates;
    syntax_rules_object(string u_label = "") noexcept : label(u_label) {}
    syntax_rules_object(const syntax_rules_object& s) = default;
    syntax_rules_object(syntax_rules_object&& s)      = default;
    // assignment
    syntax_rules_object& operator=(const syntax_rules_object& s) = default;
    syntax_rules_object& operator=(syntax_rules_object&& s)      = default;
    // stringification & equality
    string str() const noexcept {return "#<syntax-rules-object>";}
    bool operator==(const syntax_rules_object& s) const noexcept;
  };
}

#endif