# CMAKE generated file: DO NOT EDIT!
# Generated by "MinGW Makefiles" Generator, CMake Version 3.17

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

# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

SHELL = cmd.exe

# The CMake executable.
CMAKE_COMMAND = "D:\Soft\Jetbrain\CLion 2020.3.2\bin\cmake\win\bin\cmake.exe"

# The command to remove a file.
RM = "D:\Soft\Jetbrain\CLion 2020.3.2\bin\cmake\win\bin\cmake.exe" -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = D:\Study\new\compiler

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = D:\Study\new\compiler\cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/compiler.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/compiler.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/compiler.dir/flags.make

CMakeFiles/compiler.dir/compiler.cpp.obj: CMakeFiles/compiler.dir/flags.make
CMakeFiles/compiler.dir/compiler.cpp.obj: ../compiler.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=D:\Study\new\compiler\cmake-build-debug\CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/compiler.dir/compiler.cpp.obj"
	D:\Environment\mingw64\bin\g++.exe  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles\compiler.dir\compiler.cpp.obj -c D:\Study\new\compiler\compiler.cpp

CMakeFiles/compiler.dir/compiler.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/compiler.dir/compiler.cpp.i"
	D:\Environment\mingw64\bin\g++.exe $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E D:\Study\new\compiler\compiler.cpp > CMakeFiles\compiler.dir\compiler.cpp.i

CMakeFiles/compiler.dir/compiler.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/compiler.dir/compiler.cpp.s"
	D:\Environment\mingw64\bin\g++.exe $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S D:\Study\new\compiler\compiler.cpp -o CMakeFiles\compiler.dir\compiler.cpp.s

# Object files for target compiler
compiler_OBJECTS = \
"CMakeFiles/compiler.dir/compiler.cpp.obj"

# External object files for target compiler
compiler_EXTERNAL_OBJECTS =

compiler.exe: CMakeFiles/compiler.dir/compiler.cpp.obj
compiler.exe: CMakeFiles/compiler.dir/build.make
compiler.exe: CMakeFiles/compiler.dir/linklibs.rsp
compiler.exe: CMakeFiles/compiler.dir/objects1.rsp
compiler.exe: CMakeFiles/compiler.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=D:\Study\new\compiler\cmake-build-debug\CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable compiler.exe"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles\compiler.dir\link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/compiler.dir/build: compiler.exe

.PHONY : CMakeFiles/compiler.dir/build

CMakeFiles/compiler.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles\compiler.dir\cmake_clean.cmake
.PHONY : CMakeFiles/compiler.dir/clean

CMakeFiles/compiler.dir/depend:
	$(CMAKE_COMMAND) -E cmake_depends "MinGW Makefiles" D:\Study\new\compiler D:\Study\new\compiler D:\Study\new\compiler\cmake-build-debug D:\Study\new\compiler\cmake-build-debug D:\Study\new\compiler\cmake-build-debug\CMakeFiles\compiler.dir\DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/compiler.dir/depend
