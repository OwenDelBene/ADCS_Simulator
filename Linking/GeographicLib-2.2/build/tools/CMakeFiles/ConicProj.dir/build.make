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
include tools/CMakeFiles/ConicProj.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include tools/CMakeFiles/ConicProj.dir/compiler_depend.make

# Include the progress variables for this target.
include tools/CMakeFiles/ConicProj.dir/progress.make

# Include the compile flags for this target's objects.
include tools/CMakeFiles/ConicProj.dir/flags.make

tools/CMakeFiles/ConicProj.dir/ConicProj.cpp.o: tools/CMakeFiles/ConicProj.dir/flags.make
tools/CMakeFiles/ConicProj.dir/ConicProj.cpp.o: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/ConicProj.cpp
tools/CMakeFiles/ConicProj.dir/ConicProj.cpp.o: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/man/ConicProj.usage
tools/CMakeFiles/ConicProj.dir/ConicProj.cpp.o: tools/CMakeFiles/ConicProj.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object tools/CMakeFiles/ConicProj.dir/ConicProj.cpp.o"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT tools/CMakeFiles/ConicProj.dir/ConicProj.cpp.o -MF CMakeFiles/ConicProj.dir/ConicProj.cpp.o.d -o CMakeFiles/ConicProj.dir/ConicProj.cpp.o -c /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/ConicProj.cpp

tools/CMakeFiles/ConicProj.dir/ConicProj.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/ConicProj.dir/ConicProj.cpp.i"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/ConicProj.cpp > CMakeFiles/ConicProj.dir/ConicProj.cpp.i

tools/CMakeFiles/ConicProj.dir/ConicProj.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/ConicProj.dir/ConicProj.cpp.s"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/ConicProj.cpp -o CMakeFiles/ConicProj.dir/ConicProj.cpp.s

# Object files for target ConicProj
ConicProj_OBJECTS = \
"CMakeFiles/ConicProj.dir/ConicProj.cpp.o"

# External object files for target ConicProj
ConicProj_EXTERNAL_OBJECTS =

tools/ConicProj: tools/CMakeFiles/ConicProj.dir/ConicProj.cpp.o
tools/ConicProj: tools/CMakeFiles/ConicProj.dir/build.make
tools/ConicProj: src/libGeographicLib.so.24.1.0
tools/ConicProj: tools/CMakeFiles/ConicProj.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable ConicProj"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/ConicProj.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tools/CMakeFiles/ConicProj.dir/build: tools/ConicProj
.PHONY : tools/CMakeFiles/ConicProj.dir/build

tools/CMakeFiles/ConicProj.dir/clean:
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && $(CMAKE_COMMAND) -P CMakeFiles/ConicProj.dir/cmake_clean.cmake
.PHONY : tools/CMakeFiles/ConicProj.dir/clean

tools/CMakeFiles/ConicProj.dir/depend:
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2 /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools/CMakeFiles/ConicProj.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tools/CMakeFiles/ConicProj.dir/depend

