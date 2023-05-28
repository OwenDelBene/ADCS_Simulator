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
include tools/CMakeFiles/CartConvert.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include tools/CMakeFiles/CartConvert.dir/compiler_depend.make

# Include the progress variables for this target.
include tools/CMakeFiles/CartConvert.dir/progress.make

# Include the compile flags for this target's objects.
include tools/CMakeFiles/CartConvert.dir/flags.make

tools/CMakeFiles/CartConvert.dir/CartConvert.cpp.o: tools/CMakeFiles/CartConvert.dir/flags.make
tools/CMakeFiles/CartConvert.dir/CartConvert.cpp.o: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/CartConvert.cpp
tools/CMakeFiles/CartConvert.dir/CartConvert.cpp.o: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/man/CartConvert.usage
tools/CMakeFiles/CartConvert.dir/CartConvert.cpp.o: tools/CMakeFiles/CartConvert.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object tools/CMakeFiles/CartConvert.dir/CartConvert.cpp.o"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT tools/CMakeFiles/CartConvert.dir/CartConvert.cpp.o -MF CMakeFiles/CartConvert.dir/CartConvert.cpp.o.d -o CMakeFiles/CartConvert.dir/CartConvert.cpp.o -c /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/CartConvert.cpp

tools/CMakeFiles/CartConvert.dir/CartConvert.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/CartConvert.dir/CartConvert.cpp.i"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/CartConvert.cpp > CMakeFiles/CartConvert.dir/CartConvert.cpp.i

tools/CMakeFiles/CartConvert.dir/CartConvert.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/CartConvert.dir/CartConvert.cpp.s"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/CartConvert.cpp -o CMakeFiles/CartConvert.dir/CartConvert.cpp.s

# Object files for target CartConvert
CartConvert_OBJECTS = \
"CMakeFiles/CartConvert.dir/CartConvert.cpp.o"

# External object files for target CartConvert
CartConvert_EXTERNAL_OBJECTS =

tools/CartConvert: tools/CMakeFiles/CartConvert.dir/CartConvert.cpp.o
tools/CartConvert: tools/CMakeFiles/CartConvert.dir/build.make
tools/CartConvert: src/libGeographicLib.so.24.1.0
tools/CartConvert: tools/CMakeFiles/CartConvert.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable CartConvert"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/CartConvert.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tools/CMakeFiles/CartConvert.dir/build: tools/CartConvert
.PHONY : tools/CMakeFiles/CartConvert.dir/build

tools/CMakeFiles/CartConvert.dir/clean:
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && $(CMAKE_COMMAND) -P CMakeFiles/CartConvert.dir/cmake_clean.cmake
.PHONY : tools/CMakeFiles/CartConvert.dir/clean

tools/CMakeFiles/CartConvert.dir/depend:
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2 /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools/CMakeFiles/CartConvert.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tools/CMakeFiles/CartConvert.dir/depend
