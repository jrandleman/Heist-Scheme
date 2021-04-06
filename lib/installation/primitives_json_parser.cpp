// Author: Jordan Randleman -- jrandleman@scu.edu -- primitives_json_parser.cpp
// => Parses "../primitives/primitives.json" in order to initialize "../core/primitives.hpp"
//    for the C++ Heist Scheme Interpreter

// NOTE: if defined(HEIST_SCHEME_CORE_INSTALLER_CPP_): running as library
//       else: running as stand-alone program

#ifndef HEIST_SCHEME_CORE_PRIMITIVES_JSON_PARSER_CPP_
#define HEIST_SCHEME_CORE_PRIMITIVES_JSON_PARSER_CPP_

/******************************************************************************
* GET THE JSON PARSER
******************************************************************************/

#include "json.hpp"

/******************************************************************************
* PRIMITIVE REGISTRY FILEPATH
******************************************************************************/

#ifndef HEIST_SCHEME_CORE_INSTALLER_CPP_
  #define PRIMITIVES_REGISTRY_FILEPATH "../core/primitives.hpp"
#else
  #define PRIMITIVES_REGISTRY_FILEPATH "../lib/core/primitives.hpp"
#endif

/******************************************************************************
* JSON FILEPATH
******************************************************************************/

#ifndef HEIST_SCHEME_CORE_INSTALLER_CPP_
  #define PRIMITIVES_JSON_FILEPATH "../primitives/primitives.json"
#else
  #define PRIMITIVES_JSON_FILEPATH "../lib/primitives/primitives.json"
#endif

/******************************************************************************
* ERROR SIGNALING
******************************************************************************/

std::string signal_error_throw() {
  throw 1;
  return ""; // never triggered
}

#define signal_error(signal_error_message) std::cerr <<\
  "\n>> INSTALLER ERROR WHILE PARSING \"/lib/primitives/primitives.json\":\n   " << signal_error_message <<\
  "\n   @FILE: " << __FILE__ << "\n   @FUNC: " << __func__ << "\n   @LINE: " << __LINE__ <<\
  "\n   >> SEE \"docs/EXTEND.md\" FOR HELP EXTENDING HEIST WITH C++ PRIMITIVES!\n   " <<\
  primitives_json_parser::stringify_PRIMITIVE_FILES(3) + "\n\n";

/******************************************************************************
* PRIMITIVE JSON PARSING & POPULATION LOGIC
******************************************************************************/

namespace primitives_json_parser {

  /******************************************************************************
  * PRIMITIVES.JSON PARSING DATA STRUCTURE
  ******************************************************************************/

  struct primitive_node {
    std::string cpp_name;
    std::string scm_name;
    bool uses_dynamic_scope = false;
  };

  struct file_node {
    std::string filename;
    std::vector<primitive_node> primitives;
    bool is_cpp_primitive_file()const noexcept{return !primitives.empty();}
    bool is_scm_primitive_file()const noexcept{return primitives.empty();}
  };

  std::vector<file_node> PRIMITIVE_FILES;

  /******************************************************************************
  * STRINGIFY & PRINT <PRIMITIVE_FILES> (FOR ERROR MESSAGING)
  ******************************************************************************/

  std::string stringify_bool(const bool b)noexcept{
    if(b) return "true";
    return "false";
  }


  std::string stringify_PRIMITIVE_FILES(const std::size_t tab_offset = 0, const std::string& prefix = ">> ")noexcept{
    if(PRIMITIVE_FILES.empty()) return prefix + "CURRENT FILE-PRIMITIVES JSON AST = {}\n";
    const auto newline = '\n' + std::string(prefix.size() + tab_offset, ' ');
    std::string buffer = prefix + "CURRENT FILE-PRIMITIVES JSON AST =" + newline + '{';
    for(const auto& file : PRIMITIVE_FILES) {
      if(file.is_cpp_primitive_file()) {
        buffer += newline + "  \"" + file.filename + "\": {filetype: C++," + newline + "      primitives: [";
        for(const auto& primitive : file.primitives) {
          buffer += newline + "        "
            "{cpp_name: \"" + primitive.cpp_name + 
            "\", scm_name: \"" + primitive.scm_name + 
            "\", dynamic_scope: " + stringify_bool(primitive.uses_dynamic_scope) + "},";
        }
        buffer += newline + "      ]},";
      } else {
        buffer += newline + "  \"" + file.filename + "\": {filetype: SCM},";
      }
    }
    return buffer + newline + "}\n";
  }


  void print_PRIMITIVE_FILES()noexcept{
    printf("%s", stringify_PRIMITIVE_FILES().c_str());
    fflush(stdout);
  }

  /******************************************************************************
  * JSON FILE READER
  ******************************************************************************/

  std::string read_json() {
    FILE* ins = fopen(PRIMITIVES_JSON_FILEPATH, "r");
    if(ins == nullptr) signal_error("\"PRIMITIVES_JSON_FILEPATH\" MACRO POINTS TO A NON-EXISTENT FILE: \"" PRIMITIVES_JSON_FILEPATH "\"!");
    std::string file_contents;
    char buffer[1001];
    while(fgets(buffer,1000,ins)) file_contents += buffer;
    fclose(ins);
    return file_contents;
  }

  /******************************************************************************
  * JSON FILE PARSER
  ******************************************************************************/

  bool cpp_primitive_uses_dynamic_scope(const std::string& str)noexcept{
    return !str.empty() && str[0] == '*';
  }


  // Removes a prefixing "*" if present (denotes dynamically-scoped primitives)
  std::string extract_cppname(const std::string& s)noexcept{
    if(!s.empty() && s[0] == '*') return std::string(s.begin()+1,s.end());
    return s;
  }


  bool is_scm_paren(const char c)noexcept{
    return c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}';
  }


  // NOTE: This makes no assumptions about what reader syntax may be active or not (including "'")
  void validate_scm_variable_name(const std::string& str, const JSON::Datum& ast, const std::size_t property_No, const std::size_t val_No) {
    std::string local_err_message;
    for(const auto& ch : str) {
      if(!isprint(ch)) {
        local_err_message = "BE ONLY HAVE PRINTABLE CHARACTERS";
        break;
      } else if(isspace(ch)) {
        local_err_message = "NOT CONTAIN WHITESPACE";
        break;
      } else if(is_scm_paren(ch)) {
        local_err_message = "NOT CONTAIN PARENS/BRACKETS/BRACES";
        break;
      } else if(ch >= 'A' && ch <= 'Z') {
        local_err_message = "NOT CONTAIN ANY UPPERCASE LETTERS";
        break;
      }
    }
    if(!local_err_message.empty())
      signal_error("JSON OBJECT PROPERTY #" + std::to_string(property_No) + 
        ", PRIMITIVE OBJECT VALUE #" + std::to_string(val_No) + ", SCM NAME \"" + str + 
        "\", MUST " + local_err_message + "!\n   JSON:\n" + JSON::stringify(ast,2,3));
  }


  void extract_JSON_primitive_names(file_node& fnode, const JSON::Datum& ast, const JSON::Obj_type& obj, const std::size_t property_No) {
    for(std::size_t i = 0, n = obj.size(); i < n; ++i) {
      if(obj[i].second.type != JSON::Types::Str)
        signal_error("JSON OBJECT PROPERTY #" + std::to_string(property_No) + 
          ", PRIMITIVE OBJECT VALUE #" + std::to_string(i+1) + ", " + JSON::stringify(obj[i].second) + 
          ", ISN'T A SCM NAME STRING!\n   JSON:\n" + JSON::stringify(ast,2,3));
      primitive_node pnode;
      pnode.uses_dynamic_scope = cpp_primitive_uses_dynamic_scope(obj[i].first);
      pnode.cpp_name = extract_cppname(obj[i].first);
      pnode.scm_name = obj[i].second.Str;
      validate_scm_variable_name(pnode.scm_name, ast, property_No, i+1);
      fnode.primitives.push_back(std::move(pnode));
    }
  }


  void populate_PRIMITIVE_FILES_using_JSON_ast(const JSON::Datum& ast) {
    if(ast.type != JSON::Types::Obj)
      signal_error("File didn't begin with an object!\n   JSON:\n" + JSON::stringify(ast,2,3));
    for(std::size_t i = 0, n = ast.Obj.size(); i < n; ++i) {
      file_node fnode;
      fnode.filename = ast.Obj[i].first;
      switch(ast.Obj[i].second.type) {
        case JSON::Types::Nul:
          PRIMITIVE_FILES.push_back(std::move(fnode));
          break;
        case JSON::Types::Obj:
          extract_JSON_primitive_names(fnode, ast, ast.Obj[i].second.Obj, i+1);
          PRIMITIVE_FILES.push_back(std::move(fnode));
          break;
        default:
          signal_error("JSON OBJECT PROPERTY #" + std::to_string(i+1) + 
            " DOESN'T HAVE 'null' OR AN OBJECT OF C++/SCM PRIMITIVE NAMES AS A VALUE:\n" + 
            JSON::stringify(ast,2));
      }
    }
  }


  std::string format_json_error_message(std::string&& err)noexcept{
    for(std::size_t i = 0; i < err.size(); ++i) {
      if(err[i] == '\n') {
        err.insert(i+1, "   ");
        i += 3;
      }
    }
    err += '\n';
    return std::move(err);
  }


  // Populates <PRIMITIVE_FILES>
  void parse_json(const std::string& file_contents) {
    auto [json_ast, err_message] = JSON::parse(file_contents);
    if(!err_message.empty()) signal_error(format_json_error_message(std::move(err_message)));
    populate_PRIMITIVE_FILES_using_JSON_ast(json_ast);
  }

  /******************************************************************************
  * VALIDATE PARSER RESULTS
  ******************************************************************************/

  // -- VALIDATE FILENAMES
  void verify_no_empty_or_duplicate_filenames() {
    for(std::size_t i = 0, n = PRIMITIVE_FILES.size(); i < n; ++i) {
      if(PRIMITIVE_FILES[i].filename.empty())
        signal_error("EMPTY FILENAME DETECTED AT PROPERTY #" + std::to_string(i+1) + '!');
      for(std::size_t j = i+1; j < n; ++j) {
        if(PRIMITIVE_FILES[i].filename == PRIMITIVE_FILES[j].filename)
          signal_error("DUPLICATE FILENAME DETECTED AT PROPERTIES #" + std::to_string(i+1) + " & #" + 
            std::to_string(j+1) + ": \"" + PRIMITIVE_FILES[i].filename + "\"!");
      }
    }
  }


  // -- VALIDATE C++ PRIMITIVE NAMES
  void verify_no_empty_cpp_primitive_names() {
    for(std::size_t file_idx = 0, total_files = PRIMITIVE_FILES.size(); file_idx < total_files; ++file_idx)
      for(std::size_t primitive_idx = 0, total_primitives = PRIMITIVE_FILES[file_idx].primitives.size(); primitive_idx < total_primitives; ++primitive_idx)
        if(extract_cppname(PRIMITIVE_FILES[file_idx].primitives[primitive_idx].cpp_name).empty())
          signal_error("INVALID EMPTY C++ PRIMITIVE NAME DETECTED AT PROPERTY #" + std::to_string(file_idx+1) + " PRIMITIVE #" + std::to_string(primitive_idx+1) + 
            " (\"" + PRIMITIVE_FILES[file_idx].filename + "\":\"" + PRIMITIVE_FILES[file_idx].primitives[primitive_idx].cpp_name + "\")!");
  }


  // -- VALIDATE SCM PRIMITIVE NAMES
  void verify_no_subsequent_scm_primitives_have_name(const std::string& scm_name, std::size_t file_idx, const std::size_t total_files, const std::size_t primitive_idx){
    const auto first_file_idx = file_idx;
    for(; file_idx < total_files; ++file_idx) {
      for(std::size_t i = (file_idx == first_file_idx) ? primitive_idx+1 : 0, n = PRIMITIVE_FILES[file_idx].primitives.size(); i < n; ++i) {
        if(PRIMITIVE_FILES[file_idx].primitives[i].scm_name == scm_name) {
          signal_error("DUPLICATE SCM PRIMITIVE NAMES DETECTED AT PROPERTY #" + std::to_string(first_file_idx+1) + " PRIMITIVE #" + std::to_string(primitive_idx+1) + 
            " (\"" + PRIMITIVE_FILES[first_file_idx].filename + "\":\"" + PRIMITIVE_FILES[first_file_idx].primitives[primitive_idx].scm_name + "\")"
            " & PROPERTY #" + std::to_string(file_idx+1) + " PRIMITIVE #" + std::to_string(i+1) + 
            " (\"" + PRIMITIVE_FILES[file_idx].filename + "\":\"" + PRIMITIVE_FILES[file_idx].primitives[i].scm_name + "\")!");
        }
      }
    }
  }


  void verify_no_empty_or_duplicate_scm_primitive_names() {
    for(std::size_t file_idx = 0, total_files = PRIMITIVE_FILES.size(); file_idx < total_files; ++file_idx) {
      for(std::size_t primitive_idx = 0, total_primitives = PRIMITIVE_FILES[file_idx].primitives.size(); primitive_idx < total_primitives; ++primitive_idx){
        const auto& scm_name = PRIMITIVE_FILES[file_idx].primitives[primitive_idx].scm_name;
        if(scm_name.empty())
          signal_error("INVALID EMPTY SCM PRIMITIVE NAME DETECTED AT PROPERTY #" + std::to_string(file_idx+1) + " PRIMITIVE #" + std::to_string(primitive_idx+1) + 
            " (\"" + PRIMITIVE_FILES[file_idx].filename + "\":\"" + PRIMITIVE_FILES[file_idx].primitives[primitive_idx].scm_name + "\")!");
        verify_no_subsequent_scm_primitives_have_name(scm_name,file_idx,total_files,primitive_idx);
      }
    }
  }


  // Verify found files, no duplicate filenames, no duplicate C++/Scm primitive names
  void validate_parser_results() {
    if(PRIMITIVE_FILES.empty())
      signal_error("MISSING PRIMITIVE FILES, JSON FILE IS EMPTY!");
    verify_no_empty_or_duplicate_filenames();
    verify_no_empty_cpp_primitive_names(); // ok to have duplicate C++ prm names so long as they are associated with different scheme names
    verify_no_empty_or_duplicate_scm_primitive_names();
  }

  /******************************************************************************
  * PARSER MAIN EXECUTION
  ******************************************************************************/

  // Populates <PRIMITIVE_FILES>
  void extract_data_from_primitives_json() {
    parse_json(read_json());
    validate_parser_results();
  }

  /******************************************************************************
  * GENERATE THE C++/SCM PRIMITIVE ASSOCIATIONS REGISTRY
  ******************************************************************************/

  std::string generate_cpp_primitives_array_registry()noexcept{
    std::string pairs("constexpr const std::pair<prm_ptr_t,const char*>primitive_procedure_declarations[]={\n");
    for(const auto& file : PRIMITIVE_FILES) {
      for(const auto& primitive : file.primitives)
        pairs += "  std::make_pair(" + extract_cppname(primitive.cpp_name) + ",\"" + primitive.scm_name + "\"),\n";
      pairs += '\n';
    }
    return pairs + "};";
  }

  /******************************************************************************
  * GENERATE THE C++ DYNAMIC-SCOPE REGISTRY
  ******************************************************************************/

  std::string generate_cpp_dynamic_scope_primitives_array_registry()noexcept{
    std::string registry("constexpr const prm_ptr_t PRIMITIVES_REQUIRING_CURRENT_ENVIRONMENT[]={\n");
    for(const auto& file : PRIMITIVE_FILES)
      for(const auto& primitive : file.primitives)
        if(primitive.uses_dynamic_scope)
          registry += "  " + extract_cppname(primitive.cpp_name) + ",\n";
    return registry + "};";
  }

  /******************************************************************************
  * GENERATE THE SCHEME PRIMITIVE SOURCE FILES REGISTRY
  ******************************************************************************/

  std::string generate_scm_primitives_array_registry()noexcept{
    std::string registry("constexpr const char* primitive_scm_source_files[]={\n");
    for(const auto& file : PRIMITIVE_FILES)
      if(file.is_scm_primitive_file())
        registry += "  \"" + file.filename + "\",\n";
    return registry + "};";
  }

  /******************************************************************************
  * GENERATE PRIMITIVE C++ FILE INCLUSION SET
  ******************************************************************************/

  std::string generate_cpp_file_include_set()noexcept{
    std::string includes;
    for(const auto& file : PRIMITIVE_FILES)
      if(file.is_cpp_primitive_file())
        includes += "\n#include \"" + file.filename + '"';
    return includes + '\n';
  }

  /******************************************************************************
  * POPULATE HEIST SCHEME'S "core/primitives.hpp" REGISTRY
  ******************************************************************************/

  std::string slurp_core_primitives_hpp() {
    FILE* read = fopen(PRIMITIVES_REGISTRY_FILEPATH, "r");
    if(!read) signal_error("PRIMITIVE REGISTRY \"" PRIMITIVES_REGISTRY_FILEPATH "\" NOT FOUND, "
                           "UNABLE TO ADD PRIMITIVES FROM " PRIMITIVES_JSON_FILEPATH "!");
    std::string contents;
    char buffer[1001];
    while(fgets(buffer,1000,read)) contents += buffer;
    fclose(read);
    return contents;
  }


  void replace_core_primitives_hpp(const std::string& buffer) {
    FILE* write = fopen(PRIMITIVES_REGISTRY_FILEPATH, "w");
    if(!write) signal_error("PRIMITIVE REGISTRY \"" PRIMITIVES_REGISTRY_FILEPATH "\" COULDN'T BE WRITTEN TO, "
                            "UNABLE TO ADD PRIMITIVES FROM " PRIMITIVES_JSON_FILEPATH "!");
    fputs(buffer.c_str(), write);
    fclose(write);
  }


  void inject_string(std::string& buffer, const std::string& injection_target, const std::string& injection_value) {
    const auto target_start = injection_target + "-BEGIN", target_end = injection_target + "-END";
    const auto start_pos = buffer.find(target_start), end_pos = buffer.find(target_end);
    if(start_pos == std::string::npos || end_pos == std::string::npos) {
      if(start_pos == std::string::npos) 
        signal_error("COULDN'T FIND " + target_start + " DURING PRIMITIVE REGISTRY CODE INJECTION!");
      signal_error("COULDN'T FIND " + target_end + " DURING PRIMITIVE REGISTRY CODE INJECTION!");
    }
    const auto start_end = start_pos+target_start.size();
    buffer.erase(start_end, end_pos-start_end);
    buffer.insert(start_end, '\n' + injection_value + '\n');
  }


  void populate_core_primitives_hpp_registry() {
    auto buffer = slurp_core_primitives_hpp();
    inject_string(buffer,"//@HEIST-INSTALLER-FILE-INCLUDE-REGISTRY",generate_cpp_file_include_set());
    inject_string(buffer,"//@HEIST-INSTALLER-DYNAMIC-SCOPE-REGISTRY",generate_cpp_dynamic_scope_primitives_array_registry());
    inject_string(buffer,"//@HEIST-INSTALLER-SCHEME-PRIMITIVE-REGISTRY",generate_scm_primitives_array_registry());
    inject_string(buffer,"//@HEIST-INSTALLER-PRIMITIVE-ASSOCIATION-REGISTRY",generate_cpp_primitives_array_registry());
    replace_core_primitives_hpp(buffer);
  }
} // End of namespace primitives_json_parser

/******************************************************************************
* PRIMITIVE JSON PARSING & REGISTRY CODE GENERATION MAIN EXECUTION
******************************************************************************/

int register_json_primitives() {
  try {
    primitives_json_parser::extract_data_from_primitives_json();
    primitives_json_parser::populate_core_primitives_hpp_registry();
    return 0;
  } catch(...) {
    return 1;
  }
}

/******************************************************************************
* MAIN EXECUTION
******************************************************************************/

#ifndef HEIST_SCHEME_CORE_INSTALLER_CPP_
int main() {return register_json_primitives();}
#endif


#undef signal_error
#undef PRIMITIVES_JSON_FILEPATH
#undef PRIMITIVES_REGISTRY_FILEPATH
#endif