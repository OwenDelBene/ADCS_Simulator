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
CMAKE_SOURCE_DIR = /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake/build

# Include any dependencies generated for this target.
include CMakeFiles/glew_s.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/glew_s.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/glew_s.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/glew_s.dir/flags.make

CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.o: CMakeFiles/glew_s.dir/flags.make
CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.o: /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c
CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.o: CMakeFiles/glew_s.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.o"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -MD -MT CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.o -MF CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.o.d -o CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.o -c /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c

CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.i"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c > CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.i

CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.s"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c -o CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.s

# Object files for target glew_s
glew_s_OBJECTS = \
"CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.o"

# External object files for target glew_s
glew_s_EXTERNAL_OBJECTS =

lib/libGLEW.a: CMakeFiles/glew_s.dir/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/src/glew.c.o
lib/libGLEW.a: CMakeFiles/glew_s.dir/build.make
lib/libGLEW.a: CMakeFiles/glew_s.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C static library lib/libGLEW.a"
	$(CMAKE_COMMAND) -P CMakeFiles/glew_s.dir/cmake_clean_target.cmake
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/glew_s.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/glew_s.dir/build: lib/libGLEW.a
.PHONY : CMakeFiles/glew_s.dir/build

CMakeFiles/glew_s.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/glew_s.dir/cmake_clean.cmake
.PHONY : CMakeFiles/glew_s.dir/clean

CMakeFiles/glew_s.dir/depend:
	cd /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake/build /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake/build /mnt/c/users/owend/documents/c++/ADCSSimulator/Linking/glew-2.1.0/build/cmake/build/CMakeFiles/glew_s.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/glew_s.dir/depend

