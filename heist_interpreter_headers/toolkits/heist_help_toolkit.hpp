// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_help_toolkit.hpp
// => Defines logic for the Heist Scheme Interpreter's <help> primitive

////////////////////////////////////////////////////////////
// PROVIDES 2 PROCEDURES FOR USE IN heist_primitives.hpp:
//   0. help::launch_interactive_menu()
//   1. help::query_datum(const scm_string& query)
////////////////////////////////////////////////////////////

#ifndef HEIST_HELP_TOOLKIT_HPP_
#define HEIST_HELP_TOOLKIT_HPP_

/******************************************************************************
* HELP DB DATA MATRIX
******************************************************************************/

namespace G {
  // defines menu options lists
  // defines <HELP_ENTRIES> matrix of entries:
  //    entry ::= {name, classification, signatures, description}
  #include "heist_help_toolkit_db.hpp" 
}

/******************************************************************************
* HELP DB QUERYING HELPER FUNCTIONS
******************************************************************************/

namespace help::logic {
  scm_string indent_phrasing(const char* s)noexcept{
    scm_string str;
    while(*s) {
      str += *s;
      if(*s == '\n' && *(s+1)) str += ' ', str += ' ';
      ++s;
    }
    return str;
  }


  bool is_scar_scdr_composition(const sym_type& query)noexcept{
    if(query.size() <= 4 || query[0] != 's' || query[1] != 'c') return false;
    for(unsigned i = 2; i < 6; ++i) {
      if(query[i] != 'a' && query[i] != 'd') return false;
      if(query[i+1] == 'r' && !query[i+2]) return true;
    }
    return false;
  }


  bool is_car_cdr_composition(const sym_type& query)noexcept{
    if(query.size() <= 3 || query[0] != 'c') return false;
    for(unsigned i = 1; i < 5; ++i) {
      if(query[i] != 'a' && query[i] != 'd') return false;
      if(query[i+1] == 'r' && !query[i+2]) return true;
    }
    return false;
  }


  void strip_whitespace(sym_type& query)noexcept{
    if(query.empty() || (!isspace(*query.begin()) && !isspace(*query.rbegin()))) return;
    const auto n = query.size();
    size_type begin = 0, end = n-1;
    while(begin < n && isspace(query[begin])) ++begin;
    while(end && isspace(query[end])) --end;
    if(!end && isspace(query[end])) 
      query = "";
    else
      query = query.substr(begin,end-begin+1);
  }


  void prepare_query(sym_type& query)noexcept{
    // mk query lower-case
    for(auto& ch : query) ch = scm_numeric::mklower(ch);
    // strip whitespace at either end
    strip_whitespace(query);
    // replace aliases with their official entry
    if(query.empty() || query == "null" || query == "empty-list" || query == "()" || query == "'()" || query == "nihil") 
      query = "nil";
    else if(query == "numeric" || query == "num") 
      query = "number";
    else if(query == "character") 
      query = "char";
    else if(query == "bool") 
      query = "boolean";
    else if(query == "&&") 
      query = "and";
    else if(query == "||") 
      query = "or";
    else if(query == "class" || query == "prototype" || query == "class-proto" || query == "proto") 
      query = "class-prototype";
    else if(query == "function" || query == "fcn")
      query = "procedure";
    else if(query == "flags" || query == "command-line-flags" || query == "cmd-line" || query == "cmd-line-flags" || query == "cmd") 
      query = "command-line";
    else if(query == "-cps" || query == "continuation-passing-style" || query == "continuation" || query == "continuations") 
      query = "cps";
    else if(query == "comment" || query == "commenting" || query == ";" || query == "#||#" || query == "#|" || query == "|#") 
      query = "comments";
    else if(query == "unquote" || query == "unquote-splicing")
      query = "quasiquote";
    else if(query == "named-let" || query == "nameless-let") 
      query = "let";
    else if(query == "macro" || query == "define-macro" || query == "let-syntax" || query == "letrec-syntax") 
      query = "define-syntax";
    else if(query == "reader-alias" || query == "alias")
      query = "define-reader-alias";
    else if(query == "define-class" || query == "define-prototype" || query == "define-class-prototype" || query == "self" || query == "super") 
      query = "defclass";
    else if(query == "anonymous-object" || query == "anon-object" || query == "anonymous-obj" || query == "anon-obj") 
      query = "new";
    else if(query == "make-coroutine")
      query = "define-coroutine";
    else if(query == "overload" || query == "polymorphism" || query == "polymorphic") 
      query = "define-overload";
    else if(query == "infixr!" || query == "infix" || query == "infixr" || query == "operator" || query == "infix-operator" || query == "infixr-operator")
      query = "infix!";
    else if(query == "true" || query == "t") 
      query = "#t";
    else if(query == "false") 
      query = "#f";
    else if(query == "min-infix-precedence" || query == "min-precedence") 
      query = "*min-infix-precedence*";
    else if(query == "max-infix-precedence" || query == "max-precedence") 
      query = "*max-infix-precedence*";
    else if(query == "null-environment" || query == "null-env") 
      query = "*null-environment*";
    else if(query == "local-environment" || query == "local-env") 
      query = "*local-environment*";
    else if(query == "global-environment" || query == "global-env") 
      query = "*global-environment*";
    else if(query == "argc") 
      query = "*argc*";
    else if(query == "argv") 
      query = "*argv*";
    else if(query == "heist-platform" || query == "platform") 
      query = "*heist-platform*";
    else if(query == "heist-exact-platform" || query == "exact-platform") 
      query = "*heist-exact-platform*";
    else if(query == "heist-dirname") 
      query = "*heist-dirname*";
    else if(query == "exit-success") 
      query = "*exit-success*";
    else if(query == "exit-failure") 
      query = "*exit-failure*";
    else if(query == "o") 
      query = "compose";
    else if(query == "e") 
      query = "fl-e";
    else if(query == "pi") 
      query = "fl-pi";
    else if(query == "phi") 
      query = "fl-phi";
    else if(query == "euler") 
      query = "fl-euler";
    else if(query == "+inf" || query == "inf" || query == "inf.0") 
      query = "+inf.0";
    else if(query == "-inf") 
      query = "-inf.0";
    else if(query == "-nan.0" || query == "nan.0" || query == "nan" || query == "+nan" || query == "-nan") 
      query = "+nan.0";
    else if(query == "seq") 
      query = "sequence";
    else if(query == "coro") 
      query = "coroutine";
    else if(query == "expr") 
      query = "expression";
    else if(query == "sym") 
      query = "symbol";
    else if(query == "str") 
      query = "string";
    else if(query == "hash-map" || query == "hashmap") 
      query = "hmap";
    else if(query == "heist-interop" || query == "heist-cpp-interop" || query == "heist-c++-interop" || query == "cpp-interop" || 
            query == "c++-interop" || query == "heist-cpp" || query == "heist-c++" || query == "heist_cpp_interop" || query == "interop")
      query = "heist_cpp_interop.hpp";
    else if(query == "readme") 
      query = "readme.md";
    else if(query == "install") 
      query = "install.md";
    else if(query == "license.md") 
      query = "license";
    else if(query == "associative-list" || query == "association-list") 
      query = "alist";
    else if(is_scar_scdr_composition(query) || query == "scaar...scddddr")
      query = "scaar ... scddddr";
    else if(is_car_cdr_composition(query) || query == "caar...cddddr")
      query = "caar ... cddddr";
  }


  size_type longestCommonSubstring(const scm_string& s1, const char* s2, const size_type& n)noexcept{
    const auto m = s1.size(); // Traditional DP Soln: solve the "longest common suffix" prob instead
    std::vector<std::vector<size_type>> longestCommonSuffix(m+1,std::vector<size_type>(n+1,0));
    size_type longest = 0;
    for(size_type i = 0; i <= m; i++) {
      for(size_type j = 0; j <= n; j++) {
        if(i && j && s1[i-1] == s2[j-1]) {
            longestCommonSuffix[i][j] = longestCommonSuffix[i-1][j-1] + 1;
            if(longestCommonSuffix[i][j] > longest)
              longest = longestCommonSuffix[i][j];
        }
      }
    }
    return longest;
  }


  void print_possibly_intended_queries(const sym_type& query, std::vector<scm_string>& possible_matches)noexcept{
    size_type minimum_substring_length_match = query.size() < 4 ? query.size() : 4;
    static constexpr const char* ALTERNATIVE_HELP_QUERY_NAMES[] = {
      "","null","empty-list","()","'()","nihil","numeric","num","character","bool","&&","||","class","prototype","class-proto","proto",
      "function","fcn","flags","command-line-flags","cmd-line","cmd-line-flags","cmd","-cps","continuation-passing-style","continuation",
      "continuations","comment","commenting",";","#||#","#|","|#","unquote","unquote-splicing","named-let","nameless-let","macro","define-macro",
      "let-syntax","letrec-syntax","reader-alias","alias","define-class","define-prototype","define-class-prototype","self","super","anonymous-object",
      "anon-object","anonymous-obj","anon-obj","make-coroutine","overload","polymorphism","polymorphic","infixr!","infix","infixr","operator",
      "infix-operator","infixr-operator","true","t","false","min-infix-precedence","min-precedence","max-infix-precedence","max-precedence",
      "null-environment","null-env","local-environment","local-env","global-environment","global-env","argc","argv","heist-platform","platform",
      "heist-exact-platform","exact-platform","heist-dirname","exit-success","exit-failure","o","e","pi","phi","euler","+inf","inf","inf.0",
      "-inf","-nan.0","nan.0","nan","+nan","-nan","seq","coro","expr","sym","str","hash-map","hashmap","heist-interop","heist-cpp-interop",
      "heist-c++-interop","cpp-interop","c++-interop","heist-cpp","heist-c++","heist_cpp_interop","interop","readme","install","license.md",
      "associative-list","association-list","scaar","scadr","scdar","scddr","scaaar","scaadr","scadar","scaddr","scdaar","scdadr","scddar","scdddr",
      "scaaaar","scaaadr","scaadar","scaaddr","scadaar","scadadr","scaddar","scadddr","scdaaar","scdaadr","scdadar","scdaddr","scddaar","scddadr",
      "scdddar","scddddr","scaar...scddddr","caar","cadr","cdar","cddr","caaar","caadr","cadar","caddr","cdaar","cdadr","cddar","cdddr","caaaar","caaadr",
      "caadar","caaddr","cadaar","cadadr","caddar","cadddr","cdaaar","cdaadr","cdadar","cdaddr","cddaar","cddadr","cdddar","cddddr","caar...cddddr"
    };
    // Continuously try finding possible mismatches of decreasing minimum substring match lengths (until a match is found)
    for(; minimum_substring_length_match > 0; --minimum_substring_length_match) {
      // Match against official entry names
      for(const auto& entry : G::HELP_ENTRIES)
        if(longestCommonSubstring(query, entry[0], strlen(entry[0])) >= minimum_substring_length_match)
          possible_matches.push_back(entry[0]);
      // Match against entry name aliases from <prepare_entry>
      for(const auto& alias : ALTERNATIVE_HELP_QUERY_NAMES) {
        if(longestCommonSubstring(query, alias, strlen(alias)) >= minimum_substring_length_match) {
          // get alias's offical entry name
          scm_string s(alias);
          prepare_query(s);
          // only add alias if not already in the list of possible mismatches
          bool found = false;
          for(const auto& pm : possible_matches)
            if(pm == s) { found = true; break; }
          if(!found) possible_matches.push_back(std::move(s));
        }
      }
      // break loop if found possible mismatches
      if(!possible_matches.empty()) break;
    }
    // print results & output prompt to get a new entry
    if(possible_matches.empty()) {
      printf("\nNo matches found, enter a new query!\n  => NOTE: Type \"quit\" to quit!\n\n");
    } else {
      printf("\nNo matches found! Did you mean:\n");
      for(size_type i = 0, n = possible_matches.size(); i < n; ++i) {
        if(i < 10) printf("   %zu) %s\n", i, possible_matches[i].c_str());
        else        printf("  %zu) %s\n", i, possible_matches[i].c_str());
      }
      printf("\nEnter the number of the desired entry above, OR enter a new query!"
             "\n  => NOTE: Type \"quit\" to quit!\n\n");
    }
  }


  scm_string get_new_help_query(const std::vector<scm_string>& possible_matches)noexcept{
    printf("help> ");
    fflush(stdout);
    // get new query
    scm_string new_query;
    int ch;
    while((ch = fgetc(stdin)) != '\n') new_query += ch;
    // clean new query & determine if given a number
    strip_whitespace(new_query);
    if(new_query.empty()) return get_new_help_query(possible_matches);
    bool only_digits = true;
    for(auto& ch : new_query)
      if(!isdigit(ch))
        only_digits = false, ch = scm_numeric::mklower(ch);
    // check if new query is an index access for 1 of the suggested matches
    int suggested_match_idx = -1;
    if(only_digits) {
      try {
        suggested_match_idx = std::stoi(new_query);
      } catch(...) {
        suggested_match_idx = -1;
      }
    }
    // return new query value
    if(suggested_match_idx >= 0 && (size_type)suggested_match_idx < possible_matches.size())
      return possible_matches[suggested_match_idx];
    return new_query;
  }


  // Returns G::EMPTY_ENTRY if no match
  auto get_entry(sym_type query)noexcept{
    prepare_query(query);
    for(const auto& entry : G::HELP_ENTRIES)
      if(entry[0] == query) 
        return entry;
    return G::EMPTY_ENTRY;
  }


  // Print the single entry instance
  // PRECONDITION: entry != G::EMPTY_ENTRY
  void print_entry(const char *const *const entry)noexcept{
    puts("\n==================================================================================");
    printf("Name: %s\n", entry[0]);
    for(size_type i = 0, n = strlen(entry[0]) + 6; i < n; ++i) putchar('*');
    printf("\nClassification: %s\n", entry[1]);
    if(entry[2][0])
      printf("\nSignatures:%s", indent_phrasing(entry[2]).c_str());
    printf("\nDescription:%s", indent_phrasing(entry[3]).c_str());
    puts("==================================================================================\n");
  }


  // Prints potential intended matches & returns new help query
  scm_string retry_help_query(const sym_type& query)noexcept{
    std::vector<scm_string> possible_matches;
    print_possibly_intended_queries(query,possible_matches);
    return get_new_help_query(possible_matches);
  }


  // Determines whether entered a valid quit string
  bool is_quit_string(const scm_string& new_query)noexcept{
    return new_query == "q" || new_query == "quit"  || new_query == ":q";
  }
} // End of namespace help::logic

/******************************************************************************
* HELP DB SINGLE QUERYING MAIN FUNCTION
******************************************************************************/

namespace help {
  // Returns whether read in "quit" flag
  bool query_datum(const sym_type& query)noexcept{
    const auto entry = logic::get_entry(query);
    if(entry != G::EMPTY_ENTRY) {
      logic::print_entry(entry);
    } else {
      auto new_query = logic::retry_help_query(query);
      if(!logic::is_quit_string(new_query)) 
        return query_datum(new_query);
      return true;
    }
    return false;
  }
}

/******************************************************************************
* HELP DB QUERYING DYNAMIC MENU HELPER FUNCTIONS
******************************************************************************/

namespace help::menu {
  scm_string get_input()noexcept{
    printf("help> ");
    fflush(stdout);
    scm_string new_query;
    int ch;
    while((ch = fgetc(stdin)) != '\n') 
      new_query += (ch >= 'A' && ch <= 'Z' ? ch+32 : ch); // mk lowercase
    logic::strip_whitespace(new_query);
    return new_query;
  }


  int get_row_max_width(const char *const *const HELP_MENU_SUBMENU, const size_type n, const int column)noexcept{
    int max_row_width = 0;
    for(size_type i = column; i < n; i += 4) {
      auto length = strlen(HELP_MENU_SUBMENU[i]);
      if(length > (size_type)max_row_width) max_row_width = length;
    }
    return max_row_width;
  }


  void print_submenu_item(const char* col_style, const char* ENTRY)noexcept{
    scm_string str(ENTRY);
    for(auto& ch : str) if(ch >= 'a' && ch <= 'z') ch -= 32; // to uppercase
    printf(col_style, str.c_str());
  }


  void display_submenu(const char *const *const HELP_MENU_SUBMENU, const size_type n)noexcept{
    puts("");
    const scm_string col0_style = "%-" + std::to_string(get_row_max_width(HELP_MENU_SUBMENU,n,0)) + "s    ";
    const scm_string col1_style = "%-" + std::to_string(get_row_max_width(HELP_MENU_SUBMENU,n,1)) + "s    ";
    const scm_string col2_style = "%-" + std::to_string(get_row_max_width(HELP_MENU_SUBMENU,n,2)) + "s    ";
    const scm_string col3_style = "%-" + std::to_string(get_row_max_width(HELP_MENU_SUBMENU,n,3)) + "s\n";
    size_type i = 0;
    for(; i < n; ++i) {
      switch(i % 4) {
        case 0: print_submenu_item(col0_style.c_str(), HELP_MENU_SUBMENU[i]); break;
        case 1: print_submenu_item(col1_style.c_str(), HELP_MENU_SUBMENU[i]); break;
        case 2: print_submenu_item(col2_style.c_str(), HELP_MENU_SUBMENU[i]); break;
        case 3: print_submenu_item(col3_style.c_str(), HELP_MENU_SUBMENU[i]); break;
      }
    }
    if(i % 4 != 0) 
      puts("\n"); // didn't print an additional #\newline yet
    else
      puts("");
  }


  void menu_driver_loop()noexcept{
    auto input = menu::get_input();
    if(logic::is_quit_string(input)) return;
    for(size_type i = 0, n = sizeof(G::HELP_MENU)/sizeof(G::HELP_MENU[0]); i < n; ++i) {
      if(input == G::HELP_MENU[i]) {
        menu::display_submenu(G::HELP_MENU_SUBMENUS[i],G::HELP_MENU_SUBMENUS_LENGTH[i]);
        menu_driver_loop();
        return;
      }
    }
    for(size_type i = 1, m = sizeof(G::HELP_MENU_PROCEDURES)/sizeof(G::HELP_MENU_PROCEDURES[0]); i < m; ++i) { // i=1 to skip "help"
      if(input == G::HELP_MENU_PROCEDURES[i]) {
        menu::display_submenu(G::HELP_MENU_PROCEDURES_SUBMENU[i],G::HELP_MENU_PROCEDURES_SUBMENU_LENGTH[i]);
        menu_driver_loop();
        return;
      }
    }
    if(help::query_datum(input)) return; // recieved "quit"
    menu_driver_loop();
  }
} // End of namespace help::menu

/******************************************************************************
* HELP DB QUERYING DYNAMIC MENU MAIN FUNCTION
******************************************************************************/

namespace help {
  void launch_interactive_menu() {
    puts("\nWelcome to Heist Scheme's help menu!\n\n"
         "Enter a query/language-feature in order to get more details about it.\n"
         "Enter \"quit\" to quit.\n\n"
         "To get a list of topics, documentation, types, primitive variables,\n"
         "constants, special forms, or primitive procedures, type \"topics\",\n"
         "\"types\", \"variables\", \"constants\", \"specials\", or \"procedures\".\n");
    menu::menu_driver_loop();
  }
} // End of namespace help
#endif