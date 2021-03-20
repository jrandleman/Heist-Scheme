// Author: Jordan Randleman -- jrandleman@scu.edu -- map_object.hpp
// => Contains "map_object" data structure for the C++ Heist Scheme Interpreter

#ifndef HEIST_MAP_OBJECT_HPP_
#define HEIST_MAP_OBJECT_HPP_

namespace heist {
  struct map_object {
    std::unordered_map<string,struct data> val;
    static bool hashable(const data& key)noexcept{
      return key.type==types::num||key.type==types::str||key.type==types::chr||
             key.type==types::sym||key.type==types::bol;
    }
    static data unhash_key(string key)noexcept{ // unhash a key back into a datum
      types t = types(*key.rbegin());
      key.pop_back();
      switch(t) {
        case types::sym: return key;
        case types::num: return num_type(key);
        case types::str: return str_type(key);
        case types::bol: return bol_type(key[1] == 't');
        case types::chr: return key[0];
        default:         return data(); // ONLY TRIGGERED IF GIVEN INVALID KEY
      }
    }
    static string hash_key(const data& key)noexcept{
      return key.display()+char(key.type);
    }
    auto& operator[](const data& key)noexcept{ // PRECONDITION: hashable(key)
      return val[hash_key(key)];
    }
  };
}

#endif