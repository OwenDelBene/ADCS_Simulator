# Decide whether to compile for ARM or for x86_64
ifeq ($(origin CXX), environment)
$(info CXX was defined by the environment! Building ARM target)
OBJ_FTYPE := arm
TARGET := AGS6-CDH-ARM
LDFLAGS += -lstdc++fs
else
$(info CXX was defined by the makefile! Building Native target)
TARGET := AGS6-CDH
OBJ_FTYPE := o
CXXFLAGS += -D__cpp_lib_filesystem=1
endif


# CXX is only set if not set by the environment
CXX ?= clang++

# CXX Flags are empty for x86, but set for ARM, solely add these flags
CXXFLAGS += --std=c++17 -Wall -Werror -ggdb -pedantic -O0 -I include

# Linker flags are also set for ARM, but not x86. Add only those necessary for both
LDFLAGS += -L /usr/local/lib -lglfw -lGeographicLib

# Build Type Warning
ifeq (${OBJ_FTYPE}, arm)
$(info Building for ARM)
else
$(info Building for Native)
endif

# Define build and soure directories
BUILD_DIR := build
SRC_DIR := src

# Finds all the sources
SRCS := $(shell find $(SRC_DIR) -name '*.cpp')

# Creates names for all object files to create
# Creates different object file suffixes for ARM and x86
# ARM has the suffix *.arm while x86 has *.o
# This allows for build objects from ARM and x86 to coexist
OBJS := $(SRCS:src/%.cpp=$(BUILD_DIR)/%.${OBJ_FTYPE})

# # Generate files stating the .hpp dependecies of each .cpp unit
# DEPS := $(OBJS:%=%.d)
# 
# -include ( $(DEPS) )

DEPS := $(shell find $(BUILD_DIR) -name '*.d')


# PHONY Targets for default builds and cleanup 
.PHONY: all
all: $(TARGET)

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)/*

.PHONY: clean-arm
clean-arm:
	find $(BUILD_DIR)/ -name \*.arm -delete

.PHONY: clean-x86
clean-x86:
	find $(BUILD_DIR)/ -name \*.o -delete

# Compiles individual object files
# Default recipe for building C++ objects
$(BUILD_DIR)/%.${OBJ_FTYPE}: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)
	@echo "Building $<"
	@$(CXX) $(CXXFLAGS) -c $< -o $@ -MMD -MT $@ #$(subst .o,.d,$(@))	


# Performs compilation of main and linking with object files
$(TARGET): $(OBJS)
	@echo "Linking $(TARGET)"
	@$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

include $(DEPS)
