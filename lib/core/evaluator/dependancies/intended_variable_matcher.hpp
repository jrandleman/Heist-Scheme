// Author: Jordan Randleman -- jordanran199@gmail.com -- intended_variable_matcher.hpp
// => Generates the list of possibly intended variable names upon an 
//    "unbound variable" error for the C++ Heist Scheme Interpreter
// => Provides functionality via <possibly_intended_variables>

#ifndef HEIST_SCHEME_CORE_INTENDED_VARIABLE_MATCHER_HPP_
#define HEIST_SCHEME_CORE_INTENDED_VARIABLE_MATCHER_HPP_

namespace heist {
  namespace evaluator_variable_matching {

    // NOTE: "LCS" DENOTES "LONGEST COMMON SUBSTRING" BEWTWEEN THE UNBOUND VAR & A POSSIBLY INTENDED VAR

    using match_metadata_type = std::pair<size_type,size_type>;        // LCS length, LCS appearance index
    using match_type          = std::pair<string,match_metadata_type>; // Possible match, match metadata
    using match_vector_type   = std::vector<match_type>;

    /***
     * Match Function:
     *   Get the intended vars with the lowest case-insensitive "match-hash" per environment frame 
     *     => "match-hash" = "max length of <var> and <intended_var>" - "LCS length"
     *   Sort the resulting variable set by ascending "match-hash"
     *     For each LCS-length set, sort them by increasing intended variable size
     *       For each variable size set, sort them by ascending LCS appearance (in the intended variable) index
     */

    string get_lowercase_string(string s)noexcept{
      for(auto& ch : s) if(ch >= 'A' && ch <= 'Z') ch += 32;
      return s;
    }


    size_type hash_match_parameters(const size_type m, const size_type n, const size_type longestCommonSubstrLength)noexcept{
      if(!longestCommonSubstrLength) return GLOBALS::MAX_SIZE_TYPE; // no common substring -> "worst" possible match hash value
      if(m > n) return m - longestCommonSubstrLength;
      return n - longestCommonSubstrLength;
    }


    // Returns a pair: {matchHash, lcsIndexInRhs}
    // Traditional DP Soln: solve the "longest common suffix" prob instead
    match_metadata_type get_substring_match_metadata(const string& s1, const string& s2)noexcept{
      const auto m = s1.size(), n = s2.size();
      std::vector<std::vector<size_type>> longestCommonSuffix(m+1,std::vector<size_type>(n+1,0));
      size_type longest = 0, rhs_end_match_index = 0;
      for(size_type i = 0; i <= m; i++) {
        for(size_type j = 0; j <= n; j++) {
          if(i && j && s1[i-1] == s2[j-1]) {
            longestCommonSuffix[i][j] = longestCommonSuffix[i-1][j-1] + 1;
            if(longestCommonSuffix[i][j] > longest) {
              longest = longestCommonSuffix[i][j];
              rhs_end_match_index = j;
            }
          }
        }
      }
      return std::make_pair(hash_match_parameters(m,n,longest), rhs_end_match_index - longest);
    }


    // Sort <match_vector> by increasing substring match hash length
    void sort_match_vector_by_match_hash(const match_vector_type::iterator& begin, const match_vector_type::iterator& end)noexcept{
      std::sort(begin,end,[](const auto& p1,const auto& p2){return p1.second.first < p2.second.first;});
    }


    // Sort <match_vector> by increasing substring match appearance-start-index
    void sort_match_vector_by_LCS_appearance(const match_vector_type::iterator& begin, const match_vector_type::iterator& end)noexcept{
      std::sort(begin,end,[](const auto& p1,const auto& p2){return p1.second.second < p2.second.second;});
    }


    // Sort <match_vector> by increasing substring match length
    void sort_match_vector_by_match_length(const match_vector_type::iterator& begin, const match_vector_type::iterator& end)noexcept{
      std::sort(begin,end,[](const auto& p1,const auto& p2){return p1.first.size() < p2.first.size();});
    }


    // Populate <match_vector> label-substringLength pairs
    void get_match_vector_of_possibly_intended_variables_in_objs(const string& var, match_vector_type& match_vector, const frame_objs& objs)noexcept{
      for(const auto& binding : objs)
        match_vector.push_back(std::make_pair(binding.first,get_substring_match_metadata(var,get_lowercase_string(binding.first))));
    }


    // Populate <match_vector> for <env>
    void get_possibly_intended_variables_in_env(const string& var, match_vector_type& match_vector, const env_type& env)noexcept{
      // Get all possible intended matches
      match_vector_type local_match_vector;
      get_match_vector_of_possibly_intended_variables_in_objs(var,local_match_vector,env->objects());
      if(local_match_vector.empty()) return;
      sort_match_vector_by_match_hash(local_match_vector.begin(),local_match_vector.end());
      // Cut off miniscule possible matches
      const auto target_lcs_length = local_match_vector[0].second.first;
      size_type i = 0;
      for(const auto& match : local_match_vector) {
        if(match.second.first == target_lcs_length) ++i;
        else break;
      }
      // Add in found possible matches of the highest substring-match-length
      match_vector.insert(match_vector.end(),local_match_vector.begin(),local_match_vector.begin()+i);
    }


    // Sort <match_vector> according to the "Match Function" described in the comment at the top of this file
    void sort_match_vector_prior_message_generation(match_vector_type& match_vector)noexcept{
      sort_match_vector_by_match_hash(match_vector.begin(),match_vector.end());
      for(size_type i = 0, n = match_vector.size(); i < n;) {
        size_type j = i+1;
        for(; j < n && match_vector[j].second.first == match_vector[i].second.first; ++j);
        if(i+1 != j) {
          sort_match_vector_by_match_length(match_vector.begin()+i,match_vector.begin()+j);
          for(size_type k = i; k < j;) {
            const auto k_match_length = match_vector[k].first.size();
            size_type l = k+1;
            for(; l < j && match_vector[l].first.size() == k_match_length; ++l);
            if(k+1 != l) sort_match_vector_by_LCS_appearance(match_vector.begin()+k,match_vector.begin()+l);
            k = l;
          }
        }
        i = j;
      }
    }


    // Convert <match_vector> to a printable message string
    string generate_match_message(match_vector_type& match_vector)noexcept{
      if(match_vector.empty()) return "";
      sort_match_vector_prior_message_generation(match_vector);
      string message(1,'\n');
      message += HEIST_AFMT(heist::AFMT_01);
      message += HEIST_AFMT(heist::AFMT_35);
      message += "  >> Did you mean:";
      message += HEIST_AFMT(heist::AFMT_01);
      // Print out the 1st 20 suggestions
      for(size_type i = 0, n = match_vector.size() >= 20 ? 20 : match_vector.size(); i < n; ++i)
        message += "\n     " + match_vector[i].first;
      return message + HEIST_AFMT(heist::AFMT_0);
    }
  } // End of namespace evaluator_variable_matching


  // Return error-message string for the variables intended to be referenced in <env> by var
  string possibly_intended_variables(const string& var, const env_type& env)noexcept{
    // Don't bother trying to match against single-letter variables (too many possible matches)
    if(var.size() < 2) return "";
    // Make matching case-insensitive
    const auto lowercase_var = evaluator_variable_matching::get_lowercase_string(var);
    // Populate <match_vector> vector
    auto env_iterator = env;
    evaluator_variable_matching::match_vector_type match_vector;
    while(env_iterator) {
      evaluator_variable_matching::get_possibly_intended_variables_in_env(lowercase_var,match_vector,env_iterator);
      env_iterator = env_iterator->parent;
    }
    // Convert <match_vector> vector to a match message-string
    return evaluator_variable_matching::generate_match_message(match_vector);
  }
}

#endif