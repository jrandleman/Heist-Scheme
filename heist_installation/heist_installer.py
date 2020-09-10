# Author: Jordan Randleman -- heist_installer.py
#   IFF python3 and Clang++/G++ are installed, automates compiling Heist Scheme
#   ELSE, follow INSTALL.md to manually compile Heist using heist_installer.cpp

import os

###############################################################################
# Generate Aliases & Header File w/ Path to Interpreter
###############################################################################

def getHeistInterpreterAlias(pathToInterpreter):
  return "alias heist='" + pathToInterpreter + "/heist_main'"


def getSublimeTextBuildSystem(pathToInterpreter):
  return """
    {
      \"cmd\": [\"""" + pathToInterpreter + """/heist_main\", \"-nansi\", \"-script\", \"$file\"],
      \"file_regex\": \"^(..[^:]*):([0-9]+):?([0-9]+)?:? (.*)$\",
    }"""


def makeFilePathHeader(pathToInterpreter):
  return """/***
 * ALIAS FOR THE REPL TO PUT IN `~/.bash_aliases` OR `~/.zshrc`:
 *   """ + getHeistInterpreterAlias(pathToInterpreter) + """
 */

/***
 * SUBLIME TEXT BUILD SYSTEM: """ + getSublimeTextBuildSystem(pathToInterpreter) + """
 */

#define HEIST_DIRECTORY_FILE_PATH \"""" + pathToInterpreter + "\""

###############################################################################
# Automate Compilation
###############################################################################

def alertCompileErr(message, result):
  if(result != 0):
    print(message + ">>> Proceed with manual compilation of \"heist_main.cpp\" using C++17!\n")
    return True
  return False


def dispatchCompilation(pathToInterpreter):
  print("\n############################################")
  print("# Checking for Clang++ or G++ to Compile ...")
  print("############################################\n")
  if(os.system("clang++ --version") == 0):
    print("\n>>> Found Clang++!\n>>> Compiling \"clang++ -std=c++17 -O3 -o ../heist_main ../heist_main.cpp\" ...")
    result = os.system("clang++ -std=c++17 -O3 -o ../heist_main ../heist_main.cpp")
    print("")
    if(alertCompileErr("\n>>> WARNING: FAILED COMPILATION!\n", result)): return
  elif(os.system("g++ --version") == 0):
    print("\n>>> Found G++!\n>>> Compiling \"g++ -std=c++17 -Wno-psabi -O3 -o ../heist_main ../heist_main.cpp\" ...")
    result = os.system("g++ -std=c++17 -Wno-psabi -O3 -o ../heist_main ../heist_main.cpp")
    print("")
    if(alertCompileErr("\n>>> WARNING: FAILED COMPILATION!\n", result)): return
  else:
    alertCompileErr("\n>>> Neither Clang++ Nor G++ detected!\n", 1)
    return
  print(">>> Successful Compilation!\n>>> Run \"./heist_main\" to start the REPL!\n")
  print(">>> Alias to add to \"~/.zshrc\" or \"~/.bash_aliases\" to launch Heist-Scheme:")
  print("    " + getHeistInterpreterAlias(pathToInterpreter))
  print(">>> SublimeText Build System JSON:" + getSublimeTextBuildSystem(pathToInterpreter) + "\n")

###############################################################################
# Main Execution
###############################################################################

# Write the header file needed and dispatch to automate compilation
pathToInterpreter = os.path.dirname(os.getcwd())
f = open("../heist_interpreter_headers/HEIST_FILEPATH.hpp", "w")
f.write(makeFilePathHeader(pathToInterpreter))
f.close()
dispatchCompilation(pathToInterpreter)