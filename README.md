# Overview
Visualizer of satellite orbits  

- 3D graphics engine with OpenGL
- Satellite SGP4 orbit propogator 
- Bdot Controller with magnetorquers


## Compile and run
```
cd src
make
./ADCS_Sim
```



## Dependencies
GeographicLib: https://geographiclib.sourceforge.io/C++/doc/install.html
GLFW: https://shnoh171.github.io/gpu%20and%20gpu%20programming/2019/08/26/installing-glfw-on-ubuntu.html


## All the translational and rotational dynamics are computed in orbit.cpp
Aerospace_mechanics.pdf was used as reference for all equations
