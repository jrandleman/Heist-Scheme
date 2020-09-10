// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_installer.cpp
// => Gets the filepath to the current repo & writes such in the generated "HEIST_FILEPATH.hpp" file
#include <filesystem>
#include <cstdio>
int main() {
  FILE* fp = fopen("../heist_interpreter_headers/HEIST_FILEPATH.hpp", "w");
  fprintf(fp, "/***\n"
              " * ALIAS FOR THE REPL TO PUT IN `~/.bash_aliases` OR `~/.zshrc`:\n"
              " *   alias heist='%s/heist_main'\n"
              " */\n\n"
              "/***\n"
              " * SUBLIME TEXT BUILD SYSTEM:\n"
              " {\n"
              "   \"cmd\": [\"%s/heist_main\", \"-nansi\", \"-script\", \"$file\"],\n"
              "   \"file_regex\": \"^(..[^:]*):([0-9]+):?([0-9]+)?:? (.*)$\",\n"
              " }\n"
              " */\n\n"
              "#define HEIST_DIRECTORY_FILE_PATH \"%s\"\n", 
              std::filesystem::current_path().c_str(),
              std::filesystem::current_path().c_str(),
              std::filesystem::current_path().c_str());
  fclose(fp);
  return 0;
}