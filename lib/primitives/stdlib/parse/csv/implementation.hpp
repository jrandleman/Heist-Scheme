// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for csv.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_CSV_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_CSV_IMPLEMENTATION_HPP_

namespace heist::stdlib_csv {

  /******************************************************************************
  * CSV VALIDATION
  ******************************************************************************/

  char validate_csv_parsing_args(data_vector& args, const char* name, const char* format) {
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args!" 
        << format << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 1st arg " << HEIST_PROFILE(args[0]) << " isn't a csv string!"
        << format << HEIST_FCN_ERR(name,args));
    if(args.size() == 2) {
      if(!args[1].is_type(types::chr) || !args[1].chr || args[1].chr == '\n')
        HEIST_THROW_ERR('\''<<name<<" 2nd arg " << HEIST_PROFILE(args[1]) << " isn't a a non-nul/newline character!" 
          << format << HEIST_FCN_ERR(name,args));
      return char(args[1].chr);
    }
    return ',';
  }


  template<bool THROWING_ERRORS>
  bool confirm_proper_LIST_csv_row_values(data_vector& row, const size_type row_number, data iter,     const data& d, 
                                                            const data_vector& args, const char* name, const char* format){
    size_type count = 1;
    while(iter.is_type(types::par)) {
      if(!iter.par->first.is_type(types::str) && !iter.par->first.is_type(types::num)) {
        if constexpr (THROWING_ERRORS) {
          HEIST_THROW_ERR('\''<<name<<" nested list #" << row_number << " item #" << count << ' ' 
            << HEIST_PROFILE(iter) << " in list " << HEIST_PROFILE(d) << " isn't a string or number!" 
            << format << HEIST_FCN_ERR(name,args));
        } else {
          return false;
        }
      }
      if constexpr (THROWING_ERRORS) row.push_back(iter.par->first); // add csv item
      ++count;
      iter = iter.par->second;
    }
    return true;
  }


  template<bool THROWING_ERRORS>
  bool confirm_proper_LIST_csv_datum(std::vector<data_vector>& csv_matrix, const data& d,    const data_vector& args, 
                                                                           const char* name, const char* format){
    if(!primitive_toolkit::data_is_proper_list(d)) {
      if constexpr (THROWING_ERRORS) {
        HEIST_THROW_ERR('\''<<name<<" item " << HEIST_PROFILE(d) << " isn't a proper list!" << HEIST_FCN_ERR(name,args));
      } else {
        return false;
      }
    }
    if(d.is_type(types::sym)) return true;
    auto iter = d;
    size_type count = 1;
    while(iter.is_type(types::par)) {
      if(!primitive_toolkit::data_is_proper_list(iter.par->first)) {
        if constexpr (THROWING_ERRORS) {
          HEIST_THROW_ERR('\''<<name<<" nested item #" << count << ' ' << HEIST_PROFILE(iter.par->first) 
            << " in list " << HEIST_PROFILE(d) << " isn't a proper list!" << HEIST_FCN_ERR(name,args));
        } else {
          return false;
        }
      }
      data_vector row;
      if(!confirm_proper_LIST_csv_row_values<THROWING_ERRORS>(row,count,iter.par->first,d,args,name,format)) return false;
      if constexpr (THROWING_ERRORS) csv_matrix.push_back(std::move(row)); // add csv row
      ++count;
      iter = iter.par->second;
    }
    return true;
  }


  template<bool THROWING_ERRORS>
  bool confirm_proper_VECTOR_csv_row_values(data_vector& row, const size_type row_number, const data& v,    const data& d, 
                                                              const data_vector& args,    const char* name, const char* format){
    for(size_type i = 0, n = v.vec->size(); i < n; ++i) {
      if(!v.vec->operator[](i).is_type(types::str) && !v.vec->operator[](i).is_type(types::num)) {
        if constexpr (THROWING_ERRORS) {
          HEIST_THROW_ERR('\''<<name<<" nested vector #" << row_number << " item #" << i+1 << ' ' 
            << HEIST_PROFILE(v) << " in vector " << HEIST_PROFILE(d) << " isn't a string or number!" 
            << format << HEIST_FCN_ERR(name,args));
        } else {
          return false;
        }
      }
      if constexpr (THROWING_ERRORS) row.push_back(v.vec->operator[](i)); // add csv item
    }
    return true;
  }


  template<bool THROWING_ERRORS>
  bool confirm_proper_VECTOR_csv_datum(std::vector<data_vector>& csv_matrix, const data& d,    const data_vector& args, 
                                                                             const char* name, const char* format){
    if(!d.is_type(types::vec)) {
      if constexpr (THROWING_ERRORS) {
        HEIST_THROW_ERR('\''<<name<<" item " << HEIST_PROFILE(d) << " isn't a vector!" << HEIST_FCN_ERR(name,args));
      } else {
        return false;
      }
    }
    for(size_type i = 0, n = d.vec->size(); i < n; ++i) {
      if(!d.vec->operator[](i).is_type(types::vec)) {
        if constexpr (THROWING_ERRORS) {
          HEIST_THROW_ERR('\''<<name<<" nested item #" << i+1 << ' ' << HEIST_PROFILE(d.vec->operator[](i)) 
            << " in list " << HEIST_PROFILE(d) << " isn't a vector!" << HEIST_FCN_ERR(name,args));
        } else {
          return false;
        }
      }
      data_vector row;
      if(!confirm_proper_VECTOR_csv_row_values<THROWING_ERRORS>(row,i+1,d.vec->operator[](i),d,args,name,format)) return false;
      if constexpr (THROWING_ERRORS) csv_matrix.push_back(std::move(row)); // add csv row
    }
    return true;
  }

  /******************************************************************************
  * CSV GENERATION
  ******************************************************************************/

  data generate_csv(const std::vector<data_vector>& csv_matrix, const char delimiter)noexcept{
    string csv;
    for(size_type i = 0, n = csv_matrix.size(); i < n; ++i) {
      for(size_type j = 0, m = csv_matrix[i].size(); j < m; ++j) {
        csv += csv_matrix[i][j].write() + delimiter;
      }
      csv += '\n';
    }
    return make_str(csv);
  }

  /******************************************************************************
  * CSV PARSING
  ******************************************************************************/

  void trim_edge_whitespace(string& sym)noexcept{
    size_type i = 0;
    for(size_type n = sym.size(); i < n && isspace(sym[i]); ++i);
    sym.erase(sym.begin(),sym.begin()+i);
    for(i = sym.size(); i-- != 0;) if(!isspace(sym[i])) break;
      sym.erase(sym.begin()+i+1,sym.end());
  }


  void print_csv_reader_error_alert(const char* seq_name) {
    std::cerr << '\n' << HEIST_AFMT(heist::AFMT_131) << "-------" << string(7+strlen(seq_name), '-')
      << HEIST_AFMT(heist::AFMT_01) << "--------------\n"
      << HEIST_AFMT(heist::AFMT_131) << "> CSV->" << seq_name
      << HEIST_AFMT(heist::AFMT_01) << " READER ERROR:" << HEIST_AFMT(heist::AFMT_0);
  }


  // Note that since, unlike JSON, csv is untyped, CSV is parsed as if all data within were strings
  data parse_csv(const char* seq_prefix, const char delimiter, string csv) {
    // Convert CSV into a Scheme expression to be read & evaluated
    trim_edge_whitespace(csv);
    string scm_expr(seq_prefix); // open the matrix
    scm_expr += seq_prefix;      // open the first row
    scm_expr += '\"';
    for(size_type i = 0, n = csv.size(); i < n; ++i) {
      // end of row
      if(csv[i] == '\n') {
        scm_expr += "\")";
        scm_expr += seq_prefix;
        scm_expr += "\"";
      // skip non-newline whitespace
      } else if(isspace(csv[i])) {
        scm_expr += csv[i];
        continue;
      // string literal
      } else if(is_non_escaped_double_quote(i,csv)) { // from lib/core/reader/parser.hpp
        auto j = i;
        skip_string_literal(i,csv);                   // from lib/core/reader/parser.hpp
        ++j;                                          // skip past the initial '"'
        while(j < i) scm_expr += csv[j++];            // j<i instead of j<=i to avoid cpying the last '"'
      // delimiter
      } else if(csv[i] == delimiter) {
        scm_expr += "\" \"";
      // general character (presumably part of a numeric)
      } else {
        scm_expr += csv[i];
      }
    }
    // Prep CSV-converted scheme expression for evaluation
    trim_edge_whitespace(scm_expr);
    scm_expr += "\")"; // close the last row 
    scm_expr += ')';   // close the matrix
    // Try parsing the converted csv expression, & throw an error as needed
    try { 
      data_vector abstract_syntax_tree;
      // Return AST if successfully parsed an expression
      parse_input_exp(string(scm_expr),abstract_syntax_tree);
      if(abstract_syntax_tree.empty()) return GLOBALS::VOID_DATA_OBJECT;
      return scm_eval(std::move(abstract_syntax_tree[0]),G.GLOBAL_ENVIRONMENT_POINTER);
    } catch(const READER_ERROR& read_error) {
      print_csv_reader_error_alert(seq_prefix[1] == 'v' ? "VECTOR" : "LIST");
      if(is_non_repl_reader_error(read_error))
           alert_non_repl_reader_error(stdout,read_error,scm_expr);
      else alert_reader_error(stdout,read_error,scm_expr);
      throw SCM_EXCEPT::READ;
    } catch(const size_type& read_error_index) {
      print_csv_reader_error_alert(seq_prefix[1] == 'v' ? "VECTOR" : "LIST");
      alert_reader_error(stdout,read_error_index,scm_expr);
      throw SCM_EXCEPT::READ;
    }
  }

} // End of namespace heist::stdlib_csv

#endif