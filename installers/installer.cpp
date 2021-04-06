// Author: Jordan Randleman -- jrandleman@scu.edu -- installer.cpp
// => Gets the filepath to the current repo & writes such in the generated "HEIST_FILEPATH.hpp" file
// => Initializes the Heist Scheme interpreter with its primitives via "primitives_json_parser.cpp"

#ifndef HEIST_SCHEME_CORE_INSTALLER_CPP_
#define HEIST_SCHEME_CORE_INSTALLER_CPP_

/******************************************************************************
* VALIDATE USING C++17 OR ABOVE
******************************************************************************/

#if __cplusplus < 201703L
  #error The Heist-Scheme installer requires compilation with C++17 or above!
#endif

/******************************************************************************
* MAIN EXECUTION
******************************************************************************/

#include <cstdio>
#include <filesystem>
#include <iostream>

#include "../lib/installation/primitives_json_parser.cpp"

int main() {
  // Populate the "primitives.hpp" registry with entries from "primitives.json"
  if(register_json_primitives()) {
    printf(">> Fix the above JSON mistake(s) prior installing Heist Scheme!\n");
    return 1;
  }
  // Generate the "/lib/HEIST_FILEPATH.hpp" file
  FILE* fp = fopen("../lib/HEIST_FILEPATH.hpp", "w");
  fprintf(fp, "/* GENERATED BY ../installers/installer.cpp */\n\n"
              "/***\n"
              " * ALIAS FOR THE REPL TO PUT IN `~/.bashrc` OR `~/.zshrc`:\n"
              " *   alias heist='%s/heist_main'\n"
              " */\n\n"
              "/***\n"
              " * SUBLIME TEXT BUILD SYSTEM:\n"
              " {\n"
              "   \"cmd\": [\"%s/heist_main\", \"-nansi\", \"$file\"],\n"
              "   \"file_regex\": \"^(..[^:]*):([0-9]+):?([0-9]+)?:? (.*)$\",\n"
              " }\n"
              " */\n\n"
              "#define HEIST_DIRECTORY_FILE_PATH \"%s\"\n", 
              std::filesystem::current_path().parent_path().c_str(),
              std::filesystem::current_path().parent_path().c_str(),
              std::filesystem::current_path().parent_path().c_str());
  fclose(fp);
  return 0;
}

#endif