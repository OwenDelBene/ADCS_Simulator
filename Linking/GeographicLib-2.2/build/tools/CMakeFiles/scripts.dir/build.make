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

# Utility rule file for scripts.

# Include any custom commands dependencies for this target.
include tools/CMakeFiles/scripts.dir/compiler_depend.make

# Include the progress variables for this target.
include tools/CMakeFiles/scripts.dir/progress.make

tools/CMakeFiles/scripts: tools/geographiclib-get-geoids
tools/CMakeFiles/scripts: tools/geographiclib-get-gravity
tools/CMakeFiles/scripts: tools/geographiclib-get-magnetic

tools/geographiclib-get-geoids: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/geographiclib-get-geoids.sh
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating geographiclib-get-geoids"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && /home/odelbene/.local/lib/python3.8/site-packages/cmake/data/bin/cmake -E copy scripts/geographiclib-get-geoids geographiclib-get-geoids && chmod +x geographiclib-get-geoids

tools/geographiclib-get-gravity: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/geographiclib-get-gravity.sh
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating geographiclib-get-gravity"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && /home/odelbene/.local/lib/python3.8/site-packages/cmake/data/bin/cmake -E copy scripts/geographiclib-get-gravity geographiclib-get-gravity && chmod +x geographiclib-get-gravity

tools/geographiclib-get-magnetic: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools/geographiclib-get-magnetic.sh
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating geographiclib-get-magnetic"
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && /home/odelbene/.local/lib/python3.8/site-packages/cmake/data/bin/cmake -E copy scripts/geographiclib-get-magnetic geographiclib-get-magnetic && chmod +x geographiclib-get-magnetic

scripts: tools/CMakeFiles/scripts
scripts: tools/geographiclib-get-geoids
scripts: tools/geographiclib-get-gravity
scripts: tools/geographiclib-get-magnetic
scripts: tools/CMakeFiles/scripts.dir/build.make
.PHONY : scripts

# Rule to build all files generated by this target.
tools/CMakeFiles/scripts.dir/build: scripts
.PHONY : tools/CMakeFiles/scripts.dir/build

tools/CMakeFiles/scripts.dir/clean:
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools && $(CMAKE_COMMAND) -P CMakeFiles/scripts.dir/cmake_clean.cmake
.PHONY : tools/CMakeFiles/scripts.dir/clean

tools/CMakeFiles/scripts.dir/depend:
	cd /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2 /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/tools/CMakeFiles/scripts.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tools/CMakeFiles/scripts.dir/depend
