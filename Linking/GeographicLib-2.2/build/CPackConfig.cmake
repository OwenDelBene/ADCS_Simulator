# This file will be configured to contain variables for CPack. These variables
# should be set in the CMake list file of the project before CPack module is
# included. The list of available CPACK_xxx variables and their associated
# documentation may be obtained using
#  cpack --help-variable-list
#
# Some variables are common to all generators (e.g. CPACK_PACKAGE_NAME)
# and some are specific to a generator
# (e.g. CPACK_NSIS_EXTRA_INSTALL_COMMANDS). The generator specific variables
# usually begin with CPACK_<GENNAME>_xxxx.


set(CPACK_BUILD_SOURCE_DIRS "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build")
set(CPACK_CMAKE_GENERATOR "Unix Makefiles")
set(CPACK_COMPONENT_UNSPECIFIED_HIDDEN "TRUE")
set(CPACK_COMPONENT_UNSPECIFIED_REQUIRED "TRUE")
set(CPACK_DEFAULT_PACKAGE_DESCRIPTION_FILE "/home/odelbene/.local/lib/python3.8/site-packages/cmake/data/share/cmake-3.26/Templates/CPack.GenericDescription.txt")
set(CPACK_DEFAULT_PACKAGE_DESCRIPTION_SUMMARY "GeographicLib built using CMake")
set(CPACK_DMG_SLA_USE_RESOURCE_FILE_LICENSE "ON")
set(CPACK_GENERATOR "TGZ")
set(CPACK_INSTALL_CMAKE_PROJECTS "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build;GeographicLib;ALL;/")
set(CPACK_INSTALL_PREFIX "/usr/local")
set(CPACK_MODULE_PATH "")
set(CPACK_NSIS_DISPLAY_NAME "GeographicLib-2.2")
set(CPACK_NSIS_INSTALLER_ICON_CODE "")
set(CPACK_NSIS_INSTALLER_MUI_ICON_CODE "")
set(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES")
set(CPACK_NSIS_PACKAGE_NAME "GeographicLib-2.2")
set(CPACK_NSIS_UNINSTALL_NAME "Uninstall")
set(CPACK_OBJCOPY_EXECUTABLE "/usr/bin/objcopy")
set(CPACK_OBJDUMP_EXECUTABLE "/usr/bin/objdump")
set(CPACK_OUTPUT_CONFIG_FILE "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CPackConfig.cmake")
set(CPACK_PACKAGE_CONTACT "charles@karney.com")
set(CPACK_PACKAGE_DEFAULT_LOCATION "/")
set(CPACK_PACKAGE_DESCRIPTION_FILE "/home/odelbene/.local/lib/python3.8/site-packages/cmake/data/share/cmake-3.26/Templates/CPack.GenericDescription.txt")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "GeographicLib library, utilities, and documentation")
set(CPACK_PACKAGE_FILE_NAME "GeographicLib-2.2-Linux")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "GeographicLib-2.2")
set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "GeographicLib-2.2")
set(CPACK_PACKAGE_NAME "GeographicLib")
set(CPACK_PACKAGE_RELOCATABLE "true")
set(CPACK_PACKAGE_VENDOR "GeographicLib")
set(CPACK_PACKAGE_VERSION "2.2")
set(CPACK_PACKAGE_VERSION_MAJOR "2")
set(CPACK_PACKAGE_VERSION_MINOR "2")
set(CPACK_PACKAGE_VERSION_PATCH "0")
set(CPACK_READELF_EXECUTABLE "/usr/bin/readelf")
set(CPACK_RESOURCE_FILE_LICENSE "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/LICENSE.txt")
set(CPACK_RESOURCE_FILE_README "/home/odelbene/.local/lib/python3.8/site-packages/cmake/data/share/cmake-3.26/Templates/CPack.GenericDescription.txt")
set(CPACK_RESOURCE_FILE_WELCOME "/home/odelbene/.local/lib/python3.8/site-packages/cmake/data/share/cmake-3.26/Templates/CPack.GenericWelcome.txt")
set(CPACK_SET_DESTDIR "OFF")
set(CPACK_SOURCE_GENERATOR "TGZ;ZIP")
set(CPACK_SOURCE_IGNORE_FILES "#;~$;/\\.git;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/BUILD;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/man/(.*\\.pod|makeusage\\.sh|dummy\\..*)$;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/cmake/maintainer-.*\\.cmake$;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/(develop|cgi-bin|.*\\.cache)/;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/(data-distrib|data-installer)/;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/(archive|scratch|mpfr_mpir_x86_x64_msvc2010)/;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/.*\\.(zip|tar\\.gz|bak|lsp)$;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/(autogen|biblio)\\.sh$;/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/(pom.xml|makefile-admin|HOWTO-RELEASE.txt)$")
set(CPACK_SOURCE_OUTPUT_CONFIG_FILE "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CPackSourceConfig.cmake")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "GeographicLib-2.2")
set(CPACK_SYSTEM_NAME "Linux")
set(CPACK_THREADS "1")
set(CPACK_TOPLEVEL_TAG "Linux")
set(CPACK_WIX_SIZEOF_VOID_P "8")

if(NOT CPACK_PROPERTIES_FILE)
  set(CPACK_PROPERTIES_FILE "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/CPackProperties.cmake")
endif()

if(EXISTS ${CPACK_PROPERTIES_FILE})
  include(${CPACK_PROPERTIES_FILE})
endif()
