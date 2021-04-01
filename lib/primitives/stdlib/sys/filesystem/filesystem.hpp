// Author: Jordan Randleman -- jrandleman@scu.edu -- filesystem.hpp
// => Defines the primitive filesystem functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_FILESYSTEM_HPP_
#define HEIST_SCHEME_CORE_STDLIB_FILESYSTEM_HPP_

#include "implementation.hpp"

namespace heist {

  // Returns a string of the current working directory
  data primitive_GETCWD(data_vector&& args) {
    if(!args.empty())
      HEIST_THROW_ERR("'getcwd doesn't accept any args!\n     (getcwd)" << HEIST_FCN_ERR("getcwd",args));
    try {
      return make_str(std::filesystem::current_path());  
    } catch(...) {
      return GLOBALS::FALSE_DATA_BOOLEAN;
    }
  }

  // Returns a string of the parent directory of the given path string
  data primitive_DIRNAME(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'dirname didn't get a path <string> arg:"
        "\n     (dirname <path-string>)" << HEIST_FCN_ERR("dirname",args));
    try {
      return make_str(std::filesystem::path(*args[0].str).parent_path());
    } catch(...) {
      return GLOBALS::FALSE_DATA_BOOLEAN;
    }
  }

  // Returns whether successfully created the given directory name
  data primitive_MKDIR(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'mkdir didn't get a directory name <string> arg:"
        "\n     (mkdir <new-directory-name-string>)" << HEIST_FCN_ERR("mkdir",args));
    try {
      return boolean(std::filesystem::create_directory(*args[0].str));
    } catch(...) {
      return GLOBALS::FALSE_DATA_BOOLEAN;
    }
  }

  // Returns whether successfully changed the current directory
  data primitive_CHDIR(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'chdir didn't get a directory path <string> arg:"
        "\n     (chdir <directory-path-string>)" << HEIST_FCN_ERR("chdir",args));
    try {
      std::filesystem::current_path(*args[0].str);
      return GLOBALS::TRUE_DATA_BOOLEAN;
    } catch(...) {
      return GLOBALS::FALSE_DATA_BOOLEAN;
    }
  }

  // file predicate
  data primitive_FILEP(data_vector&& args) {
    stdlib_filesystem::confirm_given_one_string_arg(args, "file?", "\n     (file? <filename-string>)");
    return boolean(stdlib_filesystem::is_file(*args[0].str));
  }

  // directory predicate
  data primitive_DIRECTORYP(data_vector&& args) {
    stdlib_filesystem::confirm_given_one_string_arg(args, "directory?", "\n     (directory? <directory-name-string>)");
    return boolean(stdlib_filesystem::is_directory(*args[0].str));
  }

  // path predicate (file | directory)
  data primitive_PATHP(data_vector&& args) {
    stdlib_filesystem::confirm_given_one_string_arg(args, "path?", "\n     (path? <path-string>)");
    return boolean(stdlib_filesystem::is_path(*args[0].str));
  }

  // returns list of paths of entries in the given directory name, or "#f" if given string isn't a directory
  data primitive_DIRECTORY_ENTRIES(data_vector&& args) {
    stdlib_filesystem::confirm_given_one_string_arg(args, "directory-entries", "\n     (directory-entries <directory-name-string>)");
    if(!stdlib_filesystem::is_directory(*args[0].str))
      return GLOBALS::FALSE_DATA_BOOLEAN;
    data_vector entries;
    for(auto& p : std::filesystem::directory_iterator(*args[0].str))
      entries.push_back(make_str(p.path()));
    return primitive_toolkit::convert_data_vector_to_proper_list(entries.begin(),entries.end());
  }

  // identical to "primitive_DIRECTORY_ENTRIES" but also filters out dotfiles
  data primitive_DIRECTORY_ENTRIES_STAR(data_vector&& args) {
    stdlib_filesystem::confirm_given_one_string_arg(args, "directory-entries*", "\n     (directory-entries* <directory-name-string>)");
    if(!stdlib_filesystem::is_directory(*args[0].str))
      return GLOBALS::FALSE_DATA_BOOLEAN;
    const auto dirname_length = args[0].str->size();
    data_vector entries;
    for(auto& p : std::filesystem::directory_iterator(*args[0].str)) {
      const string path = p.path();
      if(path.find("/.", dirname_length) == string::npos && path.find("\\.", dirname_length) == string::npos)
        entries.push_back(make_str(path));
    }
    return primitive_toolkit::convert_data_vector_to_proper_list(entries.begin(),entries.end());
  }

  // returns whether succeed copying given filename-string
  data primitive_COPY_PATH(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (copy-path <source-path-string> <destination-path-string>)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_COPY_PATH,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'copy-path didn't receive any args:"<<format<<HEIST_FCN_ERR("copy-path",args));
    for(size_type i = 0; i < 2; ++i)
      if(!args[i].is_type(types::str)) 
        HEIST_THROW_ERR("'copy-path arg "<<HEIST_PROFILE(args[i])<<" isn't a string:"<<format<<HEIST_FCN_ERR("copy-path",args));
    try {
      std::filesystem::copy(*args[0].str,*args[1].str,std::filesystem::copy_options::recursive);
      return GLOBALS::TRUE_DATA_BOOLEAN;
    } catch(...) {
      return GLOBALS::FALSE_DATA_BOOLEAN;
    }
  }

  // returns whether succeed deleting given filename-string
  data primitive_DELETE_PATH_BANG(data_vector&& args) {
    stdlib_filesystem::confirm_given_one_string_arg(args, "delete-path!", "\n     (delete-path! <filename-string>)");
    try {
      return boolean(std::filesystem::remove_all(*args[0].str) > 0);
    } catch(...) {
      return GLOBALS::FALSE_DATA_BOOLEAN;
    }
  }

  // returns whether succeed deleting given filename-string
  data primitive_RENAME_PATH_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (rename-path! <old-name-string> <new-name-string>)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_RENAME_PATH_BANG,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'rename-path! didn't receive any args:"<<format 
        << HEIST_FCN_ERR("rename-path!",args));
    for(size_type i = 0; i < 2; ++i)
      if(!args[i].is_type(types::str)) 
        HEIST_THROW_ERR("'rename-path! arg "<<HEIST_PROFILE(args[i])<<" isn't a string:"<<format 
          << HEIST_FCN_ERR("rename-path!",args));
    try {
      std::filesystem::rename(*args[0].str,*args[1].str);
      return GLOBALS::TRUE_DATA_BOOLEAN;
    } catch(...) {
      return GLOBALS::FALSE_DATA_BOOLEAN;
    }
  }

  data primitive_FILE_SIZE(data_vector&& args) {
    stdlib_filesystem::confirm_given_one_string_arg(args, "file-size", "\n     (file-size <filename-string>)");
    try {
      return num_type(std::filesystem::file_size(*args[0].str));
    } catch(...) {
      return GLOBALS::FALSE_DATA_BOOLEAN;
    }
  }

} // End of namespace heist

#endif