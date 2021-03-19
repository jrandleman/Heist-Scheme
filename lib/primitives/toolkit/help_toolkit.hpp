// Author: Jordan Randleman -- jrandleman@scu.edu -- help_toolkit.hpp
// => Defines logic for the Heist Scheme Interpreter's <help> primitive

////////////////////////////////////////////////////////////
// PROVIDES 2 PROCEDURES FOR USE IN primitives.hpp:
//   0. help::launch_interactive_menu()
//   1. help::query_datum(const string& query)
////////////////////////////////////////////////////////////

#ifndef HEIST_HELP_TOOLKIT_HPP_
#define HEIST_HELP_TOOLKIT_HPP_

/******************************************************************************
* HELP DB DATA MATRIX
******************************************************************************/

namespace GLOBALS {
  // defines menu options lists
  // defines <HELP_ENTRIES> matrix of entries:
  //    entry ::= {name, classification, signatures, description}
  #include "help_toolkit_db.hpp" 
}

/******************************************************************************
* HELP EOF SIGNAL HANDLER
******************************************************************************/

string handle_help_EOF_signal() {
  clearerr(stdin);
  puts("");
  fflush(stdout);
  return "quit";
}

/******************************************************************************
* HELP DB QUERYING HELPER FUNCTIONS
******************************************************************************/

namespace help::logic {
  // Stop indenting if at "CPS Transformation" section!
  bool at_CPS_Transformation_section(const char* s)noexcept{
    static constexpr const char * const CPS_Transformation = "CPS Transformation";
    const char* p = CPS_Transformation;
    while(*p && *s) {
      if(*p != *s) return false;
      ++p, ++s;
    }
    return !*p;
  }


  string indent_phrasing(const char* s)noexcept{
    string str;
    while(*s) {
      str += *s;
      if(*s == '\n' && *(s+1)) {
        if(at_CPS_Transformation_section(s+1))
          return str + (s+1);
        str += ' ', str += ' ';
      }
      ++s;
    }
    return str;
  }


  void strip_whitespace(string& query)noexcept{
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


  bool is_scar_scdr_composition(const string& query)noexcept{
    if(query.size() <= 4 || query[0] != 's' || query[1] != 'c') return false;
    for(unsigned i = 2; i < 6; ++i) {
      if(query[i] != 'a' && query[i] != 'd') return false;
      if(query[i+1] == 'r' && !query[i+2]) return true;
    }
    return false;
  }


  bool is_car_cdr_composition(const string& query)noexcept{
    if(query.size() <= 3 || query[0] != 'c') return false;
    for(unsigned i = 1; i < 5; ++i) {
      if(query[i] != 'a' && query[i] != 'd') return false;
      if(query[i+1] == 'r' && !query[i+2]) return true;
    }
    return false;
  }


  void prepare_query(string& query)noexcept{
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
    else if(query == "call-with-current-continuation") 
      query = "call/cc";
    else if(query == "call-with-current-environment") 
      query = "call/ce";
    else if(query == "comment" || query == "commenting" || query == ";" || query == "#||#" || query == "#|" || query == "|#") 
      query = "comments";
    else if(query == "unquote" || query == "unquote-splicing")
      query = "quasiquote";
    else if(query == "named-let" || query == "nameless-let") 
      query = "let";
    else if(query == "macro" || query == "let-syntax" || query == "letrec-syntax") 
      query = "define-syntax";
    else if(query == "syntax-transform" || query == "syntax-transformers") 
      query = "syntax-transformer";
    else if(query == "syntax-obj" || query == "syntax-object") 
      query = "syntax-rules";
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
            query == "c++-interop" || query == "heist-cpp" || query == "heist-c++" || query == "heist_cpp_interop" || query == "cpp_interop" || query == "interop")
      query = "cpp_interop.hpp";
    else if(query == "readme") 
      query = "readme.md";
    else if(query == "install") 
      query = "install.md";
    else if(query == "license.md") 
      query = "license";
    else if(query == "associative-list" || query == "association-list") 
      query = "alist";
    else if(query == "universes" || query == "new-universe" || query == "universe?" || query == "universe-eval") 
      query = "universe";
    else if(query == "\\") 
      query = "lambda";
    else if(query == "*dot" || query == "dot*") 
      query = "*dot*";
    else if(query == "truthiness") 
      query = "falsiness";
    else if(is_scar_scdr_composition(query) || query == "scaar...scddddr")
      query = "scaar ... scddddr";
    else if(is_car_cdr_composition(query) || query == "caar...cddddr")
      query = "caar ... cddddr";
  }


  size_type longestCommonSubstring(const string& s1, const char* s2, const size_type& n)noexcept{
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


  void print_possibly_intended_queries(const string& query, str_vector& possible_matches)noexcept{
    size_type minimum_substring_length_match = query.size() < 4 ? query.size() : 4;
    static constexpr const char* ALTERNATIVE_HELP_QUERY_NAMES[] = {
      "","null","empty-list","()","'()","nihil","numeric","num","character","bool","&&","||","class","prototype","class-proto","proto",
      "function","fcn","flags","command-line-flags","cmd-line","cmd-line-flags","cmd","-cps","continuation-passing-style","continuation",
      "continuations","call-with-current-continuation","call-with-current-environment","comment","commenting",";","#||#","#|","|#","unquote",
      "unquote-splicing","named-let","nameless-let","macro","syntax-transform","syntax-transformers","let-syntax","letrec-syntax","syntax-obj","syntax-object",
      "reader-alias","alias","define-class","define-prototype","define-class-prototype","self","super","anonymous-object","anon-object","anonymous-obj","anon-obj",
      "make-coroutine","overload","polymorphism","polymorphic","infixr!","infix","infixr","operator","infix-operator","infixr-operator","true","t","false",
      "min-infix-precedence","min-precedence","max-infix-precedence","max-precedence","null-environment","null-env","local-environment","local-env",
      "global-environment","global-env","argc","argv","heist-platform","platform","heist-exact-platform","exact-platform","heist-dirname","exit-success",
      "exit-failure","o","e","pi","phi","euler","+inf","inf","inf.0","-inf","-nan.0","nan.0","nan","+nan","-nan","seq","coro","expr","sym","str","hash-map",
      "hashmap","heist-interop","heist-cpp-interop","heist-c++-interop","cpp-interop","c++-interop","heist-cpp","heist-c++","heist_cpp_interop","cpp_interop",
      "interop","readme","install","license.md","associative-list","association-list","universes","new-universe","universe?","universe-eval","\\","*dot","dot*",
      "truthiness","scaar","scadr","scdar","scddr","scaaar","scaadr","scadar","scaddr","scdaar","scdadr","scddar","scdddr","scaaaar","scaaadr","scaadar","scaaddr",
      "scadaar","scadadr","scaddar","scadddr","scdaaar","scdaadr","scdadar","scdaddr","scddaar","scddadr","scdddar","scddddr","scaar...scddddr","caar",
      "cadr","cdar","cddr","caaar","caadr","cadar","caddr","cdaar","cdadr","cddar","cdddr","caaaar","caaadr","caadar","caaddr","cadaar","cadadr","caddar",
      "cadddr","cdaaar","cdaadr","cdadar","cdaddr","cddaar","cddadr","cdddar","cddddr","caar...cddddr"
    };
    // Store possible matches by decreasing substring match length
    std::vector<std::pair<size_type,string>> match_map;
    // Continuously try finding possible mismatches of decreasing minimum substring match lengths (until a match is found)
    for(; minimum_substring_length_match > 0; --minimum_substring_length_match) {
      // Match against official entry names
      for(const auto& entry : GLOBALS::HELP_ENTRIES) {
        auto match_length = longestCommonSubstring(query, entry[0], strlen(entry[0]));
        if(match_length >= minimum_substring_length_match)
          match_map.push_back(std::make_pair(match_length,entry[0]));
      }
      // Match against entry name aliases from <prepare_entry>
      for(const auto& alias : ALTERNATIVE_HELP_QUERY_NAMES) {
        auto match_length = longestCommonSubstring(query, alias, strlen(alias));
        if(match_length >= minimum_substring_length_match) {
          // get alias's offical entry name
          string s(alias);
          prepare_query(s);
          // only keep unique matches
          bool found = false;
          for(size_type i = 0, n = match_map.size(); i < n; ++i) {
            if(match_map[i].second == s) {
              found = true;
              if(match_length > match_map[i].first) match_map[i].first = match_length; // better match length
              break;
            }
          }
          if(!found) match_map.push_back(std::make_pair(match_length,std::move(s)));
        }
      }
      // break loop if found possible mismatches
      if(!match_map.empty()) break;
    }
    // print results & output prompt to get a new entry
    if(match_map.empty()) {
      printf("\nNo matches found, enter a new query!\n  => NOTE: Type \"quit\" to quit!\n\n");
    } else {
      // sort possible matches by decreasing substring match length
      std::sort(match_map.rbegin(),match_map.rend());
      for(auto& p : match_map) possible_matches.push_back(std::move(p.second));
      // output possible matches
      printf("\nNo matches found! Did you mean:\n");
      for(size_type i = 0, n = possible_matches.size(); i < n; ++i) {
        if(i < 10) printf("   %zu) %s\n", i, possible_matches[i].c_str());
        else        printf("  %zu) %s\n", i, possible_matches[i].c_str());
      }
      printf("\nEnter the number of the desired entry above, OR enter a new query!"
             "\n  => NOTE: Type \"quit\" to quit!\n\n");
    }
  }


  string get_new_help_query(const str_vector& possible_matches)noexcept{
    printf("help> ");
    fflush(stdout);
    // get new query
    string new_query;
    int ch;
    while((ch = fgetc(stdin)) != '\n' && ch != EOF) new_query += ch;
    // Quit if given EOF signal
    if(ch == EOF) return handle_help_EOF_signal();
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


  // Returns GLOBALS::EMPTY_ENTRY if no match
  auto get_entry(string& query)noexcept{
    prepare_query(query);
    for(const auto& entry : GLOBALS::HELP_ENTRIES)
      if(entry[0] == query) 
        return entry;
    return GLOBALS::EMPTY_ENTRY;
  }


  // Print the single entry instance
  // PRECONDITION: entry != GLOBALS::EMPTY_ENTRY
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
  string retry_help_query(const string& query)noexcept{
    str_vector possible_matches;
    print_possibly_intended_queries(query,possible_matches);
    return get_new_help_query(possible_matches);
  }


  // Determines whether entered a valid quit string
  bool is_quit_string(const string& new_query)noexcept{
    return new_query == "q" || new_query == "quit"  || new_query == ":q";
  }
} // End of namespace help::logic

/******************************************************************************
* HELP DB QUERYING DYNAMIC MENU HELPER FUNCTIONS
******************************************************************************/

namespace help::menu {
  string get_input()noexcept{
    printf("help> ");
    fflush(stdout);
    string new_query;
    int ch;
    while((ch = fgetc(stdin)) != '\n' && ch != EOF)
      new_query += (ch >= 'A' && ch <= 'Z' ? ch+32 : ch); // mk lowercase
    if(ch == EOF) return handle_help_EOF_signal();
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
    string str(ENTRY);
    for(auto& ch : str) if(ch >= 'a' && ch <= 'z') ch -= 32; // to uppercase
    printf(col_style, str.c_str());
  }


  void display_submenu(const char *const *const HELP_MENU_SUBMENU, const size_type n)noexcept{
    puts("");
    const string col0_style = "%-" + std::to_string(get_row_max_width(HELP_MENU_SUBMENU,n,0)) + "s    ";
    const string col1_style = "%-" + std::to_string(get_row_max_width(HELP_MENU_SUBMENU,n,1)) + "s    ";
    const string col2_style = "%-" + std::to_string(get_row_max_width(HELP_MENU_SUBMENU,n,2)) + "s    ";
    const string col3_style = "%-" + std::to_string(get_row_max_width(HELP_MENU_SUBMENU,n,3)) + "s\n";
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


  bool driver_loop_query_datum(string&& query)noexcept{
    const auto entry = logic::get_entry(query); // also prepares "query" for any comparison
    if(entry != GLOBALS::EMPTY_ENTRY) {
      logic::print_entry(entry);
    } else {
      for(size_type i = 0, n = sizeof(GLOBALS::HELP_MENU)/sizeof(GLOBALS::HELP_MENU[0]); i < n; ++i) {
        if(query == GLOBALS::HELP_MENU[i]) {
          menu::display_submenu(GLOBALS::HELP_MENU_SUBMENUS[i],GLOBALS::HELP_MENU_SUBMENUS_LENGTH[i]);
          return false;
        }
      }
      for(size_type i = 0, m = sizeof(GLOBALS::HELP_MENU_PROCEDURES)/sizeof(GLOBALS::HELP_MENU_PROCEDURES[0]); i < m; ++i) { // i=1 to skip "help"
        bool at_direct_link = false; // skip past direct procedure links (no submenu to print)
        for(const auto& link : GLOBALS::HELP_MENU_PROCEDURES_DIRECT_LINKS) {
          if(link == GLOBALS::HELP_MENU_PROCEDURES_SUBMENU[i]) {
            at_direct_link = true;
            break;
          }
        }
        if(!at_direct_link && query == GLOBALS::HELP_MENU_PROCEDURES[i]) {
          menu::display_submenu(GLOBALS::HELP_MENU_PROCEDURES_SUBMENU[i],GLOBALS::HELP_MENU_PROCEDURES_SUBMENU_LENGTH[i]);
          return false;
        }
      }
      auto new_query = logic::retry_help_query(query);
      if(!logic::is_quit_string(new_query)) 
        return driver_loop_query_datum(std::move(new_query));
      return true;
    }
    return false;
  }


  void menu_driver_loop()noexcept{
    for(;;) {
      auto input = menu::get_input();
      if(logic::is_quit_string(input) || driver_loop_query_datum(std::move(input))) return;
    }
  }
} // End of namespace help::menu

/******************************************************************************
* HELP DB QUERYING MAIN FUNCTIONS
******************************************************************************/

namespace help {
  void query_datum(string query)noexcept{
    const auto entry = logic::get_entry(query);
    if(entry != GLOBALS::EMPTY_ENTRY) {
      logic::print_entry(entry);
    } else {
      auto new_query = logic::retry_help_query(query);
      if(!logic::is_quit_string(new_query)) query_datum(new_query);
    }
  }

  void launch_interactive_menu()noexcept{
    puts("\nWelcome to Heist Scheme's help menu!\n\n"
         "Enter a query/language-feature in order to get more details about it.\n"
         "Enter \"quit\" to quit.\n\n"
         "To get a list of topics, documentation, types, math constants, special forms,\n"
         "primitive variables, or primitive procedures, type \"topics\", \"documentation\",\n"
         "\"types\", \"constants\", \"specials\", \"variables\", or \"procedures\".\n");
    menu::menu_driver_loop();
  }
} // End of namespace help
#endif