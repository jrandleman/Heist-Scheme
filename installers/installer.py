# Author: Jordan Randleman -- jordanran199@gmail.com -- installer.py
#   IFF python3 and Clang++/G++ are installed, automates compiling Heist Scheme
#   ELSE, follow "docs/INSTALL.md" to manually compile Heist using installer.cpp

import os
import sys

###############################################################################
# Determine if Debugging & the Heist Interpreter Path
###############################################################################

DEBUGGING = len(sys.argv) == 2 and sys.argv[1] == "-debug"

PATH_TO_INTERPRETER = os.path.dirname(os.getcwd())

###############################################################################
# Generate Bash/Zsh Alias & Sublime Text Build System w/ Path to Interpreter
###############################################################################

SHELL_ALIAS = "alias heist='" + PATH_TO_INTERPRETER + "/heist'"

SUBLIME_TEXT_BUILD_SYSTEM = """
    {
      \"cmd\": [\"""" + PATH_TO_INTERPRETER + """/heist\", \"-nansi\", \"$file\"],
      \"file_regex\": \"^(..[^:]*):([0-9]+):?([0-9]+)?:? (.*)$\",
    }"""

###############################################################################
# Compilation Commands for Clang++ & G++
###############################################################################

if DEBUGGING:
  CLANG_COMPILE_CMD = "clang++ -std=c++17 -O0 -Wall -Wextra -Wpedantic"
  GCC_COMPILE_CMD = "g++ -std=c++17 -Wno-psabi -O0 -Wall -Wextra -Wpedantic"
else:
  CLANG_COMPILE_CMD = "clang++ -std=c++17 -O3"
  GCC_COMPILE_CMD = "g++ -std=c++17 -Wno-psabi -O3"

###############################################################################
# Determine the Compiler Vendor
###############################################################################

print("\n############################################")
print("# Checking for Clang++ or G++ to Compile ...")
print("############################################\n")


USING_CLANG = os.system("clang++ --version") == 0
USING_GCC = not USING_CLANG and os.system("g++ --version") == 0


if USING_CLANG:
  print("\n>>> Found Clang++!\n")
elif USING_GCC:
  print("\n>>> Found G++!\n")
else:
  print("\n>>> Neither Clang++ Nor G++ detected!")
  print(">>> Proceed with manual compilation of \"installer.cpp\" & \"heist.cpp\" using C++17!\n")
  sys.exit()

###############################################################################
# Generic System Call Automatation
###############################################################################

def alertSystemErr(message, result):
  if(result != 0):
    print(message + ">>> FIX THE ABOVE ERROR(S) PRIOR INSTALLING!\n")
    return True
  return False


def launchSystem(cmd, errorMessage):
  print(">>> Executing \"" + cmd + "\" ...")
  result = os.system(cmd)
  print("")
  if(alertSystemErr(errorMessage, result)): sys.exit()

###############################################################################
# Generic Compilation Automatation
###############################################################################

def launchCompiler(executableName, sourceName):
  if USING_CLANG:
    cmd = CLANG_COMPILE_CMD + " -o " + executableName + " " + sourceName
  elif USING_GCC:
    cmd = GCC_COMPILE_CMD + " -o " + executableName + " " + sourceName
  launchSystem(cmd,"\n>>> WARNING: FAILED COMPILATION!\n")

###############################################################################
# Automate Heist Scheme C++ Installer Compilation & Execution
###############################################################################

print("\n########################################################")
print("# Compiling & Running the Heist-Scheme C++ Installer ...")
print("########################################################\n")

launchCompiler("installer", "installer.cpp")
launchSystem("./installer", "\n>>> WARNING: FAILED INSTALLATION!\n")

###############################################################################
# Automate Heist Scheme Interpreter Compilation
###############################################################################

print("\n############################################")
print("# Compiling the Heist-Scheme Interpreter ...")
print("############################################\n")

launchCompiler("../heist", "../heist.cpp")

###############################################################################
# Success!
###############################################################################

print("\n########################################")
print("# Successful Compilation, Happy Hacking!")
print("########################################\n")

print(">>> Run \"../heist\" to start the REPL!\n")
print(">>> Alias to add to \"~/.zshrc\" or \"~/.bashrc\" to launch Heist-Scheme:")
print("    " + SHELL_ALIAS + "\n")
print(">>> SublimeText Build System JSON:" + SUBLIME_TEXT_BUILD_SYSTEM + "\n")
