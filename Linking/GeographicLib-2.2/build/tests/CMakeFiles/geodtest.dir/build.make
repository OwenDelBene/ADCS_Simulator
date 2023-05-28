# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.26

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /home/odelbene/.local/lib/python3.8/site-packages/cmake/data/bin/cmake

# The command to remove a file.
RM = /home/odelbene/.local/lib/python3.8/site-packages/cmake/data/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build

# Include any dependencies generated for this target.
include tests/CMakeFiles/geodtest.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include tests/CMakeFiles/geodtest.dir/compiler_depend.make

# Include the progress variables for this target.
include tests/CMakeFiles/geodtest.dir/progress.make

# Include the compile flags for this target's objects.
include tests/CMakeFiles/geodtest.dir/flags.make

tests/CMakeFiles/geodtest.dir/geodtest.cpp.o: tests/CMakeFiles/geodtest.dir/flags.make
tests/CMakeFiles/geodtest.dir/geodtest.cpp.o: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tests/geodtest.cpp
tests/CMakeFiles/geodtest.dir/geodtest.cpp.o: tests/CMakeFiles/geodtest.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object tests/CMakeFiles/geodtest.dir/geodtest.cpp.o"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tests && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT tests/CMakeFiles/geodtest.dir/geodtest.cpp.o -MF CMakeFiles/geodtest.dir/geodtest.cpp.o.d -o CMakeFiles/geodtest.dir/geodtest.cpp.o -c /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tests/geodtest.cpp

tests/CMakeFiles/geodtest.dir/geodtest.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/geodtest.dir/geodtest.cpp.i"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tests && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tests/geodtest.cpp > CMakeFiles/geodtest.dir/geodtest.cpp.i

tests/CMakeFiles/geodtest.dir/geodtest.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/geodtest.dir/geodtest.cpp.s"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tests && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tests/geodtest.cpp -o CMakeFiles/geodtest.dir/geodtest.cpp.s

# Object files for target geodtest
geodtest_OBJECTS = \
"CMakeFiles/geodtest.dir/geodtest.cpp.o"

# External object files for target geodtest
geodtest_EXTERNAL_OBJECTS =

tests/geodtest: tests/CMakeFiles/geodtest.dir/geodtest.cpp.o
tests/geodtest: tests/CMakeFiles/geodtest.dir/build.make
tests/geodtest: src/libGeographicLib.so.24.1.0
tests/geodtest: tests/CMakeFiles/geodtest.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable geodtest"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tests && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/geodtest.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tests/CMakeFiles/geodtest.dir/build: tests/geodtest
.PHONY : tests/CMakeFiles/geodtest.dir/build

tests/CMakeFiles/geodtest.dir/clean:
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tests && $(CMAKE_COMMAND) -P CMakeFiles/geodtest.dir/cmake_clean.cmake
.PHONY : tests/CMakeFiles/geodtest.dir/clean

tests/CMakeFiles/geodtest.dir/depend:
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2 /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tests /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tests /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tests/CMakeFiles/geodtest.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tests/CMakeFiles/geodtest.dir/depend

