# Install script for directory: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/GeographicLib" TYPE FILE FILES
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Accumulator.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/AlbersEqualArea.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/AuxAngle.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/AuxLatitude.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/AzimuthalEquidistant.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/CassiniSoldner.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/CircularEngine.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Constants.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/DAuxLatitude.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/DMS.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/DST.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Ellipsoid.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/EllipticFunction.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/GARS.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/GeoCoords.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Geocentric.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Geodesic.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/GeodesicExact.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/GeodesicLine.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/GeodesicLineExact.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Geohash.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Geoid.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Georef.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Gnomonic.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/GravityCircle.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/GravityModel.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/LambertConformalConic.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/LocalCartesian.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/MGRS.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/MagneticCircle.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/MagneticModel.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Math.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/NearestNeighbor.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/NormalGravity.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/OSGB.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/PolarStereographic.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/PolygonArea.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Rhumb.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/SphericalEngine.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/SphericalHarmonic.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/SphericalHarmonic1.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/SphericalHarmonic2.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/TransverseMercator.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/TransverseMercatorExact.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/UTMUPS.hpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/include/GeographicLib/Utility.hpp"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/GeographicLib" TYPE FILE FILES "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/build/include/GeographicLib/Config.h")
endif()

