# Install script for directory: /mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/doc/GeographicLib-dev" TYPE FILE FILES
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/CMakeLists.txt"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Accumulator.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-AlbersEqualArea.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-AuxAngle.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-AuxLatitude.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-AzimuthalEquidistant.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-CassiniSoldner.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-CircularEngine.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Constants.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-DMS.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-DST.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Ellipsoid.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-EllipticFunction.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-GARS.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-GeoCoords.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Geocentric.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Geodesic.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Geodesic-small.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-GeodesicExact.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-GeodesicLine.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-GeodesicLineExact.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-GeographicErr.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Geohash.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Geoid.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Georef.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Gnomonic.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-GravityCircle.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-GravityModel.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-LambertConformalConic.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-LocalCartesian.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-MGRS.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-MagneticCircle.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-MagneticModel.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Math.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-NearestNeighbor.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-NormalGravity.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-OSGB.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-PolarStereographic.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-PolygonArea.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Rhumb.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-RhumbLine.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-SphericalEngine.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-SphericalHarmonic.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-SphericalHarmonic1.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-SphericalHarmonic2.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-TransverseMercator.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-TransverseMercatorExact.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-UTMUPS.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/example-Utility.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/GeoidToGTX.cpp"
    "/mnt/c/users/owend/Downloads/GeographicLib-2.2/GeographicLib-2.2/examples/make-egmcof.cpp"
    )
endif()

