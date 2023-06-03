#pragma once
#include "Mesh.h"
#include <vector>
#define _USE_MATH_DEFINES
#include <math.h>
#include <chrono>
#include <iostream>
//#include <cstlib>

#include <GeographicLib/MagneticModel.hpp>
#define MU 3.986e14
#define RE 6378100


using std::vector;

class Orbit
{
public:

	//magnetic field	
	GeographicLib::MagneticModel model = GeographicLib::MagneticModel("wmm2020");

	float mass;
    glm::mat3 Inertia = glm::mat3(0.9, 0, 0,
                                  0, 0.9, 0,
                                  0, 0, 0.3);
	
	std::vector <Vertex> verts;
	std::vector <GLuint> ind; 
	glm::vec4 color; 

	glm::vec3 pos;
    glm::vec3 Velocity;
    glm::vec3 acceleration;
    
    glm::vec4 q0123;
    glm::vec3 pqr;


    vector<double> stateVec;


    double phi;
    double theta;
    double psi;
    double rho; //norm pos
    double lattitude;
    double longitude;
    double altitude;


    //sensor parameters
    double MagScaleBias = 4.0e-9; //T
    double MagFieldBias = MagScaleBias*  (-1 + 2.0 * (rand() % 100) / 100.0f );
    
    double MagScaleNoise = 1e-9;
    double MagFieldNoise = MagScaleNoise * (-1 + 2.0 * (rand() % 100) / 100.0f);
	
    double AngleScaleBias = .0001; //rad/s
    double AngleFieldBias = AngleScaleBias * (-1 + 2.0 * (rand() % 100) / 100.0f);

    double AngleScaleNoise = .00005;
    double AngleFieldNoise = AngleScaleNoise * (-1 + 2.0 * (rand() % 100) / 100.0f);


    //controller parameters
    glm::vec3 M_mag = glm::vec3(0.0);//magnetorquer moment
    glm::vec3 M_wheel = glm::vec3(0.0); //reaction wheel moment
    float k_bdot = 672000.0f;

    
    float n = 84;
    float A = .02;


    glm::vec3 Bmeasure;
    glm::vec3 Wmeasure;


    glm::vec3 bodyMagneticField;

    glm::vec3 Bfilter = glm::vec3(0.0f);
    glm::vec3 Wfilter = glm::vec3(0.0f);

    glm::mat3 InvI = glm::inverse(Inertia);

	void CircularOrbit(float mass, glm::vec3 Pos, glm::vec4 Color,  Shader shader, int time); 

	void RK4(int time);

	std::vector<double> TwoBodyODE(float time, vector<double> statevec);

    glm::vec4 RotationalKinematics(glm::vec4& _q0123, glm::vec3& _pqr);
	
    glm::mat3 TIB();
    glm::mat3 TIBQuat();

    glm::vec3 bFieldI();
    vector<double> getStateVec(glm::vec3 v, glm::vec3 a, glm::vec4 q0123dot, glm::vec3 pqrdot);

    void Sensor();
    void SensorFilter();
    void Bdot();

private:
    void _calculateAngles();

    

};




