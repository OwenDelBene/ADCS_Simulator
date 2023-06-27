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



const unsigned int width = 1600;
const unsigned int height = 1000;


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


int main()
{

	


	

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
		Texture("Earth2.png", "diffuse", 0, GL_RGBA, GL_UNSIGNED_BYTE)
		
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
	// Translational Dynamics
	float initalAltitude = 400000.0f;
	float initalVelocity = sqrt(MU / (initalAltitude + RE));
	float inclination = 45.0f; //degrees
	glm::vec3 initalPos = glm::vec3(initalAltitude + RE, 0.0f, 0.0f);
	glm::vec3 initialVel = glm::vec3(0.0f, initalVelocity * sin(inclination * M_PI / 180.0f), initalVelocity * cos(inclination * M_PI / 180.0f));
	
	
	//Rotational Dynamics
	glm::vec3 initAngles(0.0, 0.0, 0.0);
	glm::vec3 initAngularVelocity(0.8, 0.5, 0.3);
	
	glm::quat initQuat(initAngles);
    
	glm::vec4 initAttitude(initQuat.w, initQuat.x, initQuat.y, initQuat.z);
	
	Sat AGS6(0.42f, 0.50f, 1.5f, 1.0f, initalPos, initialVel,initAttitude, initAngularVelocity );
	
	
	
	
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
	int end = 5400 * 10 ;
	int dt = 1;
	// Main while loop
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

		AGS6.CircularOrbit(Earth.mass, Sun.pos, Sun.color, SatShader, time); 

		
		 
		
		earth.Draw(shaderProgram, camera);
		sun.Draw(lightShader, camera);
		sat6.Draw(SatShader, camera); 
		moon.Draw(MoonShader, camera); 


		// Swap the back buffer with the front buffer
		glfwSwapBuffers(window);
		// Take care of all GLFW events
		glfwPollEvents();

		time+=dt;
	}


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


