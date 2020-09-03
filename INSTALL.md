```
=================================
  ________  ______________
 /__  __/ \/ / __\ __/\ \ \
 __/ /_/ // /_ \/ /  - \ \_\___
/_____/_/\_/\__/_/__/^\_\___\__\
=================================
```

INSTALLING THE INTERPRETER:
----------------------------

0. DOWNLOAD THIS REPOSITORY

1. COMPILE AND RUN `INSTALL_SCRIPT.cpp` USING THE `-std=c++17` FLAG
   - This generates a file named `HEIST_FILEPATH.hpp`
   - Any instance of `<HEIST_DIRECTORY_FILE_PATH>` below refers to the string in this file

2. COMPILE THE INTERPRETER: `$ clang++ -std=c++17 -O3 -o heist_main heist_main.cpp`<br>
   0. FLAG DESCRIPTIONS:
      - `-std=c++17`: [REQUIRED] compile using the C++17 standard
      - `-O3`: [RECOMMENDED FOR FASTEST EXECUTION] maximum optimization
        * longest compile time, but fastest runtime
      - `-Os`: [RECOMMENDED FOR MOST BUILDS] optimizes for binary's size
        * faster compile-time than `-O3`, smaller binary, & close runtime
   1. ON COMPILE TIME:
      - Full `-O3` compilation takes about 30s. Be patient.
        * Compilation time has been traded for FAST runtime.
      - `-Os` compilation takes about 20s. Generated binary is smaller than<br>
        `-O3`'s (as expected) & its runtime is nearly as fast



SETTING UP BETTER COMMAND-LINE INTERFACE -- FOR BASH & ZSH:
------------------------------------------------------------

0. OPEN THE FILE:
   * FOR BASH: `~/.bash_aliases` (OR `~/.bashrc`)
   * FOR ZSH: `~/.zshrc`

1. WRITE: `alias heist='<HEIST_DIRECTORY_FILE_PATH>/heist_main'`

2. SAVE THE FILE, CLOSE THE CURRENT TERMINAL WINDOW, & RELAUNCH THE TERMINAL

3. NOW, WRITING `heist` FROM ANYWHERE IN THE TERMINAL LAUNCHES THE INTERPRETER!



SETTING UP A BUILD SYSTEM -- FOR SUBLIME TEXT:
-----------------------------------------------

0. GOTO `Tools > Build System > New Build System`

1. WRITE:
    ```json
    {
      "cmd": ["<HEIST_DIRECTORY_FILE_PATH>/heist_main", "-nansi", "-script", "$file"],
      "file_regex": "^(..[^:]*):([0-9]+):?([0-9]+)?:? (.*)$",
    }
    ```

2. SAVE THE FILE AS `Heist-Scheme.sublime-build`

3. NOW THE HEIST SCHEME INTERPRETER OUGHT TO APPEAR IN `Tools > Build System`
