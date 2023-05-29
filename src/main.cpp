#include <iostream>
#include "../include/Mesh.h"
#include "../include/Planet.h"
#include "../include/Sat.h"
//#include "../Linking/GeographicLib/MagneticModel.hpp"
#define _USE_MATH_DEFINES
#include <math.h>
#include <fstream>
#include <exception>




// X = left to right | Y= Down to Up | Z = back to front

//TODO ORBIT MECHANICS, SAT GEN, EARTH TEX, MOON?


//extern "C"
//{
//	void igrf13syn(double* isv, double* date, double* itype, double* alt, double* colat, double* elong, double* x, double* y, double* z, double* f);
//	void DMDDEC(double* I, double* M, double* X);
//	void DDECDM(double* X, double* I, double* M);
//}


const unsigned int width = 1200;
const unsigned int height = 1200;


void printvec(std::vector<double> vec)
{
	for (auto x : vec)
	{
		std::cout << x << " ";

	}
	std::cout << std::endl;
}

#define RE 6378100
#define MU 3.986e14

using std::cout, std::endl;
// Vertices coordinates
//Vertex vertices[] =
////std::vector<Vertex> vertices=
//{ //               COORDINATES           /            COLORS          /           TexCoord         /       NORMALS         //
//	Vertex{glm::vec3(-1.0f, 0.0f,  1.0f), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(1.0f, 1.0f, 1.0f), glm::vec2(0.0f, 0.0f)},
//	Vertex{glm::vec3(-1.0f, 0.0f, -1.0f), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(1.0f, 1.0f, 1.0f), glm::vec2(0.0f, 1.0f)},
//	Vertex{glm::vec3(1.0f, 0.0f, -1.0f), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(1.0f, 1.0f, 1.0f), glm::vec2(1.0f, 1.0f)},
//	Vertex{glm::vec3(1.0f, 0.0f,  1.0f), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(1.0f, 1.0f, 1.0f), glm::vec2(1.0f, 0.0f)}
//};



// Indices for vertices order
//GLuint indices[] =
//{
//	0, 1, 2,
//	0, 2, 3
//};



//Vertex lightVertices[] =
//{ //     COORDINATES     //
//	Vertex{glm::vec3(-0.1f, -0.1f,  0.1f)},
//	Vertex{glm::vec3(-0.1f, -0.1f, -0.1f)},
//	Vertex{glm::vec3(0.1f, -0.1f, -0.1f)},
//	Vertex{glm::vec3(0.1f, -0.1f,  0.1f)},
//	Vertex{glm::vec3(-0.1f,  0.1f,  0.1f)},
//	Vertex{glm::vec3(-0.1f,  0.1f, -0.1f)},
//	Vertex{glm::vec3(0.1f,  0.1f, -0.1f)},
//	Vertex{glm::vec3(0.1f,  0.1f,  0.1f)}
//};
//
//GLuint lightIndices[] =
//{
//	0, 1, 2,
//	0, 2, 3,
//	0, 4, 7,
//	0, 7, 3,
//	3, 7, 6,
//	3, 6, 2,
//	2, 6, 5,
//	2, 5, 1,
//	1, 5, 4,
//	1, 4, 0,
//	4, 5, 6,
//	4, 6, 7
//};



int main()
{

	

/*
	double lat = 27.99, lon = 86.93, h = 8820.00, t = 2012.00;
	double Bx, By, Bz;
	model(t, lat, lon, h, Bx, By, Bz);
	double H, F, D, I;
	GeographicLib::MagneticModel::FieldComponents(Bx, By, Bz, H, F, D, I);

	cout << H << " " << F << " " << D << " " << " " << I << endl;

*/	

	

	glfwInit();
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

	GLFWwindow* window = glfwCreateWindow(width, height, "OrbitSim", NULL, NULL);
	if (window == NULL)
	{
		cout << "Failed to create GLFW window" << endl;
		glfwTerminate();
		return -1;
	}
	glfwMakeContextCurrent(window);
	gladLoadGL();
	glViewport(0, 0, width, height);








	// Texture data
	Texture textures[]
	{
		Texture("Earth.png", "diffuse" , 0, GL_RGBA, GL_UNSIGNED_BYTE),
		//Texture("Earth2.png", "diffuse", 0, GL_RGBA, GL_UNSIGNED_BYTE)
		
	};





	

	// Generates Shader object using shaders default.vert and default.frag
	Shader shaderProgram("default.vert", "default.frag");
	// Shader for light cube
	Shader lightShader("light.vert", "light.frag");
	Shader MoonShader("moon.vert", "default.frag");
	Shader SatShader("Sat.vert", "default.frag");

	std::vector <Texture> tex(textures, textures + sizeof(textures) / sizeof(Texture));
	

	//Planets
	Planet Earth(5.97219 * pow(10, 15), 0.5f, 12, 12);
	Planet Sun(1000, 2.0f, 12, 12);
	Planet Moon(100, 0.25f, 12, 12);

	//Satellites
	// initial conditions
	float initalAltitude = 400000.0f;
	float initalVelocity = sqrt(MU / (initalAltitude + RE));
	float inclination = 0.0f; //degrees
	glm::vec3 initalPos = glm::vec3(initalAltitude + RE, 0.0f, 0.0f);
	glm::vec3 initialVel = glm::vec3(0.0f, initalVelocity * sin(inclination * M_PI / 180.0f), initalVelocity * cos(inclination * M_PI / 180.0f));
	
	
	//initial angles
	glm::vec3 initAngles(0.0, 0.0, 0.0);
	glm::vec3 initAngularVelocity(0.8, 0.5, 0.3);
	
	glm::quat initQuat(initAngles);

	
	
	Sat AGS6(0.42f, 1.0f, 1.0f, 1.0f, initalPos, initialVel,glm::vec4(initQuat.w, initQuat.x, initQuat.y, initQuat.z), initAngularVelocity );
	
	
	
	
	// Store mesh data in vectors for the mesh
	// Crate light mesh
	Mesh earth(Earth.verts, Earth.ind, tex);
	Mesh sun(Sun.verts, Sun.ind, tex);
	Mesh sat6(AGS6.verts, AGS6.ind, tex);
	Mesh moon(Moon.verts, Moon.ind, tex); 
	glm::vec4 SunColor = glm::vec4(1.0f, 1.0f, 1.0f, 1.0f);
	
	glm::vec3 SunPos = glm::vec3(149600.0 * std::pow(10, 6), 0.0f, 0.0f);
	glm::vec3 EarthPos = glm::vec3(0.0f, 0.0f, 0.0f);
	glm::vec3 MoonPos = glm::vec3(-384.4 * std::pow(10, 6), 0.0f, 0.0f);

	glm::mat4 SunModel = glm::mat4(1.0f);
	glm::mat4 EarthModel = glm::mat4(1.0f);
	glm::mat4 SatModel = glm::mat4(1.0f);
	glm::mat4 MoonModel = glm::mat4(1.0f);

	
	EarthModel = glm::translate(EarthModel, EarthPos);
	SunModel = glm::translate(SunModel, SunPos  );
	SatModel = glm::translate(SatModel, AGS6.pos);
	MoonModel = glm::translate(MoonModel, MoonPos );
	

	
	


	shaderProgram.Activate();
	glUniformMatrix4fv(glGetUniformLocation(shaderProgram.ID, "model"), 1, GL_FALSE, glm::value_ptr(EarthModel));
	glUniform4f(glGetUniformLocation(shaderProgram.ID, "lightColor"), SunColor.x, SunColor.y, SunColor.z, SunColor.w);
	glUniform3f(glGetUniformLocation(shaderProgram.ID, "lightPos"), SunPos.x, SunPos.y, SunPos.x); 

	SatShader.Activate(); 
	glUniformMatrix4fv(glGetUniformLocation(SatShader.ID, "model"), 1, GL_FALSE, glm::value_ptr(SatModel));
	glUniform4f(glGetUniformLocation(SatShader.ID, "lightColor"), SunColor.x, SunColor.y, SunColor.z, SunColor.w);
	glUniform3f(glGetUniformLocation(SatShader.ID, "lightPos"), SunPos.x, SunPos.y, SunPos.x);

	lightShader.Activate();
	glUniformMatrix4fv(glGetUniformLocation(lightShader.ID, "model"), 1, GL_FALSE, glm::value_ptr(SunModel));
	glUniform4f(glGetUniformLocation(lightShader.ID, "lightColor"), SunColor.x, SunColor.y, SunColor.z, SunColor.w);

	MoonShader.Activate();
	glUniformMatrix4fv(glGetUniformLocation(MoonShader.ID, "model"), 1, GL_FALSE, glm::value_ptr(MoonModel));
	glUniform4f(glGetUniformLocation(MoonShader.ID, "lightColor"), SunColor.x, SunColor.y, SunColor.z, SunColor.w);
	glUniform3f(glGetUniformLocation(MoonShader.ID, "lightPos"), SunPos.x, SunPos.y, SunPos.x);

	// Enables the Depth Buffer
	glEnable(GL_DEPTH_TEST);

	// Creates camera object
	Camera camera(width, height, glm::vec3(0.0f, 0.0f, 2.0f));


	
	int time = 0; 
	int end = 5400 * 10;
	// Main while loop
	std::ofstream f("test.csv");
	while (!glfwWindowShouldClose(window) && (time < end) )
	{
		 
		// Specify the color of the background
		glClearColor(0.07f, 0.13f, 0.17f, 1.0f);
		// Clean the back buffer and depth buffer
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		// Handles camera inputs
		camera.Inputs(window);
		// Updates and exports the camera matrix to the Vertex Shader
		camera.updateMatrix(45.0f, 0.1f, 100.0f);

		// Draws different meshes
	//	std::cout << "Position: " <<AGS6.pos.x << " " << AGS6.pos.y << " " << AGS6.pos.z << std::endl;
	//	std::cout << "Velocity: " << AGS6.Velocity.x << " " << AGS6.Velocity.y << " " << AGS6.Velocity.z << std::endl;
	//	f << AGS6.pos.x << "," << AGS6.pos.z << '\n';
		f << AGS6.bodyMagneticField.x << "," << AGS6.bodyMagneticField.y << "," << AGS6.bodyMagneticField.z << "\n";
	//	printvec(AGS6.stateVec);
		AGS6.CircularOrbit(Earth.mass, Sun.pos, Sun.color, SatShader, time); 

		
		 
		
		earth.Draw(shaderProgram, camera);
		sun.Draw(lightShader, camera);
		sat6.Draw(SatShader, camera); 
		moon.Draw(MoonShader, camera); 


		// Swap the back buffer with the front buffer
		glfwSwapBuffers(window);
		// Take care of all GLFW events
		glfwPollEvents();

		time+=1;
	}
	f.close();



	// Delete all the objects we've created
	shaderProgram.Delete();
	lightShader.Delete();
	SatShader.Delete();
	// Delete window before ending the program
	glfwDestroyWindow(window);
	// Terminate GLFW before ending the program
	glfwTerminate();
	return 0;
}


