# Author: Jordan Randleman -- heist_installer.py
#   IFF python3 and Clang++/G++ are installed, automates compiling Heist Scheme
#   ELSE, follow INSTALL.md to manually compile Heist using heist_installer.cpp

import os

def makeFilePathHeader(cwd):
  return """/***
 * ALIAS FOR THE REPL TO PUT IN `~/.bash_aliases` OR `~/.zshrc`:
 *   alias heist='""" + cwd + """/heist_main'
 */

/***
 * SUBLIME TEXT BUILD SYSTEM:
 {
   \"cmd\": [\"""" + cwd + """/heist_main\", \"-nansi\", \"-script\", \"$file\"],
   \"file_regex\": \"^(..[^:]*):([0-9]+):?([0-9]+)?:? (.*)$\",
 }
 */

#define HEIST_DIRECTORY_FILE_PATH \"""" + cwd + "\""



def alertCompileErr(message, result):
  if(result != 0):
    print(message + ">>> Proceed with manual compilation of \"heist_main.cpp\" using C++17!")
    return True
  return False



def dispatchCompilation():
  print("\n############################################")
  print("# Checking for Clang++ or G++ to Compile ...")
  print("############################################\n")
  if(os.system("clang++ --version") == 0):
    print("\n>>> Found Clang++!\n>>> Compiling \"clang++ -std=c++17 -O3 -o ../heist_main ../heist_main.cpp\" ...\n")
    result = os.system("clang++ -std=c++17 -O3 -o ../heist_main ../heist_main.cpp")
    print("")
    if(alertCompileErr("\n>>> WARNING: FAILED COMPILATION!\n", result)): return
  elif(os.system("g++ --version") == 0):
    print("\n>>> Found G++!\n>>> Compiling \"g++ -std=c++17 -Wno-psabi -O3 -o ../heist_main ../heist_main.cpp\" ...\n")
    result = os.system("g++ -std=c++17 -Wno-psabi -O3 -o ../heist_main ../heist_main.cpp")
    print("")
    if(alertCompileErr("\n>>> WARNING: FAILED COMPILATION!\n", result)): return
  else:
    alertCompileErr("\n>>> Neither Clang++ Nor G++ detected!\n", 1)
    return
  print(">>> Successful Compilation! Run \"./heist_main\" to start the REPL!")
  print(">>> Read \"INSTALL.md\" for how to make a shell alias & SublimeText Build for Heist!\n")



# Write the header file needed and dispatch to automate compilation
f = open("../heist_interpreter_headers/HEIST_FILEPATH.hpp", "w")
f.write(makeFilePathHeader(os.path.dirname(os.getcwd())))
f.close()
dispatchCompilation()