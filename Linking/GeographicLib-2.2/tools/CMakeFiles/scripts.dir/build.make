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
CMAKE_SOURCE_DIR = /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools

# Utility rule file for scripts.

# Include any custom commands dependencies for this target.
include CMakeFiles/scripts.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/scripts.dir/progress.make

scripts: CMakeFiles/scripts.dir/build.make
.PHONY : scripts

# Rule to build all files generated by this target.
CMakeFiles/scripts.dir/build: scripts
.PHONY : CMakeFiles/scripts.dir/build

CMakeFiles/scripts.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/scripts.dir/cmake_clean.cmake
.PHONY : CMakeFiles/scripts.dir/clean

CMakeFiles/scripts.dir/depend:
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/CMakeFiles/scripts.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/scripts.dir/depend

