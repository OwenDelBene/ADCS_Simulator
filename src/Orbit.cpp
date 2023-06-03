#include "../include/Orbit.h"
//TODO USe template



glm::vec3 operator*(glm::vec3 lhs, int rhs)
{
	return glm::vec3(lhs.x * rhs, lhs.y * rhs, lhs.z * rhs); 
}
glm::vec3 operator*(glm::vec3 lhs, float rhs)
{
	return glm::vec3(lhs.x * rhs, lhs.y * rhs, lhs.z * rhs);
}
glm::vec3 operator*(glm::vec3 lhs, double rhs)
{
	return glm::vec3(lhs.x * rhs, lhs.y * rhs, lhs.z * rhs);
}



glm::vec3 operator+(glm::vec3 lhs, double rhs)
{
	return glm::vec3(lhs.x + rhs, lhs.y + rhs, lhs.z + rhs);
}

glm::vec3 operator+(double lhs, glm::vec3 rhs)
{
	return glm::vec3(rhs.x + lhs, rhs.y + lhs, rhs.z + lhs);
}


vector<double> operator*(vector<double> lhs, double rhs)
{
	vector<double> ret;
	for (auto x : lhs)
	{
		ret.push_back(x * rhs);
	}
	return ret;
}

vector<double> operator*( double lhs, vector<double> rhs)
{
	vector<double> ret;
	for (auto x : rhs)
	{
		ret.push_back(x * lhs);
	}
	return ret;
}


vector<double> operator+(vector<double> lhs, double rhs)
{
	vector<double> ret;
	for (auto x : lhs)
	{
		ret.push_back(x  + rhs);
	}
	return ret;
}

vector<double> operator+(double lhs, vector<double> rhs)
{
	vector<double> ret;
	for (auto x : rhs)
	{
		ret.push_back(x + lhs);
	}
	return ret;
}

vector<double> operator+(vector<double> lhs, vector<double> rhs)
{
	vector<double> retval;
	for (unsigned int i=0; i< lhs.size(); i++)
	{
		retval.push_back(lhs.at(i) + rhs.at(i));
	}
	return retval;
}





std::vector<double> Orbit::TwoBodyODE(float time, vector<double> statevec)
{
	//TODO pass in vectors for position and velocity
	//Need different ones for each k1-k4
	//TODO Find out how to return both position and velocity vectors
	//Maybe 2*3 matrix?
	glm::vec3 pos(statevec.at(0), statevec.at(1), statevec.at(2));
	glm::vec3 vel(statevec.at(3), statevec.at(4), statevec.at(5));

	glm::vec4 attitude(statevec.at(6), statevec.at(7), statevec.at(8), statevec.at(9));
	glm::vec3 rates(statevec.at(10), statevec.at(11), statevec.at(12));



	//translational
	glm::vec3 accel = pos * (-MU / std::pow(glm::length(pos), 3));
	// compute acceleration using  gmm/r2, new position := old velocity , old velocity = calculated acceleration
	
	glm::vec4 q0123dot = RotationalKinematics(attitude, rates);
	
	
	glm::vec3 H = Inertia * rates;
	glm::vec3 pqrDot = InvI * (M_mag + M_wheel - glm::cross(rates, H));

	
	return getStateVec(vel, accel, q0123dot, pqrDot);
}

void Orbit::RK4(int time)
{

	float step=1;
	 


	std::vector<double> k1 = this->TwoBodyODE(time, stateVec); 


	std::vector<double> k2 = this->TwoBodyODE(time + .5*step , stateVec + ( k1 * (.5*step))); 
 
	std::vector<double> k3 = this->TwoBodyODE(time + .5*step, stateVec + k2 * .5 * step); 
	
	std::vector<double> k4 = this->TwoBodyODE(time + step, stateVec + k3 * step); 


	this->stateVec = this->stateVec + (step / 6.0f) * ((k1 + ( k2 * 2 ) + ( k3 * 2) + k4));
	



	//this->Velocity =  the same ^
	// Return type of TwoBodyODE for each k1-k4,
	//set this pos and this velocity to weighted average. 
	//Incoroporate timestep in main. 
	// obfuscate circular orbit. 
	
}

void Orbit::CircularOrbit(float mass, glm::vec3 Pos, glm::vec4 Color, Shader shader, int time)
{

	/*

	 by  CALL sigrf(YEAR), CALL sdgrf(YEAR) or CALL spgrf(YEAR) .    */
	 /*      Then, CALL igrfc(FI, FK, H, F) gives TotalForce (F) of that model   */
	 /*          at the point of Lat.=FI, Long.=FK, Alt.=H                       */
	 /*      If other components are desired, CALL igrfm(FM) .                   */
	 /*          Here FM is an array with 6 elements, which correspond to        */
	 /*              North(X), East(Y), Downward(Z), Horizontal(H) components,   */
	 /*              Inclination(I) and Declination(D).
		 */
	
	

	//todo call for igrf
	//lat, long, alt, gives force F
	
	
	Sensor();
	Bdot();
	this->RK4(time);
	


	this->pos = glm::vec3(stateVec.at(0), stateVec.at(1), stateVec.at(2));
	this->Velocity = glm::vec3(stateVec.at(3), stateVec.at(4), stateVec.at(5));

	this->q0123 = glm::vec4(stateVec.at(6), stateVec.at(7), stateVec.at(8), stateVec.at(9));
	this->pqr = glm::vec3(stateVec.at(10), stateVec.at(11), stateVec.at(12));


	

	glm::mat4 model = glm::mat4(1.0f);
	glm::vec3 modelPos = this->pos / glm::length(this->pos); //normalize position to fit on screen

	

	model = glm::translate(model , modelPos);
	model = glm::rotate(model, q0123.x, glm::vec3(q0123.y, q0123.z, q0123.w));
	//model = model * rot;
	

	


	shader.Activate();
	glUniformMatrix4fv(glGetUniformLocation(shader.ID, "model"), 1, GL_FALSE, glm::value_ptr(model));
	glUniform4f(glGetUniformLocation(shader.ID, "lightColor"), Color.x,Color.y, Color.z, Color.w);
	glUniform3f(glGetUniformLocation(shader.ID, "lightPos"), Pos.x, Pos.y, Pos.z);

	


}

//Matrix to convert North, East, Down to ECI coordinates
glm::mat3 Orbit::TIB()
{
	
	//std::cout << "theta, phi, psi" << theta << " " << phi << " " << psi << std::endl;
	double ct = cos(theta);
	double st = sin(theta);
	double sp = sin(phi);
	double cp = cos(phi);
	double ss = sin(psi);
	double cs = cos(psi);

	return glm::mat3(ct * cs, sp * st * cs - cp * ss, cp * st * cp + sp * ss,
		ct * sp, sp * st * ss + cp * cs, cp * st * ss - sp * cs,
		-st, sp * ct, cp * ct
	);
}

glm::mat3 Orbit::TIBQuat()
{


	double q0 = this->q0123.x;
	double q1 = this->q0123.y;
	double q2 = this->q0123.z;
	double q3 = this->q0123.w;
	
//	std::cout << "printvec: " << q0 << " " << q1 << " " << q2 << " " << q3  << std::endl;
	

	double q02 = pow(q0, 2);
	double q12 = pow(q1, 2);
	double q22 = pow(q2, 2);
	double q32 = pow(q3, 2);

		
	return glm::mat3(q02+q12-q22-q32, 2*(q1*q1-q0*q3), 2*(q0*q2*q1*q3),
					 2*(q1*q2+q0*q3), q02-q12+q22-q32, 2*(q2*q3-q0*q1),
					 2*(q1*q3-q0*q2), 2*(q0*q1+q2*q1), q02-q12-q22+q32
					);
}


void Orbit::_calculateAngles()
{
	glm::vec3 pos = glm::vec3(stateVec.at(0), stateVec.at(1), stateVec.at(2));
	rho = glm::length(pos);
	phi = 0;
	theta = acos(pos.z / rho); //rho is mag(pos)
	psi = atan2(pos.y, pos.x);
	lattitude = 90 - theta * 180 / M_PI;
	longitude = psi * 180 / M_PI;
	altitude = (rho - RE);



}


//inertial bfield
glm::vec3 Orbit::bFieldI()
{
	_calculateAngles();
	//initialize igrf
	/*          by  CALL sigrf(YEAR), CALL sdgrf(YEAR) or CALL spgrf(YEAR) .    */
	//sigrf(1955.);
	//spgrf(1975.);
	//double F;
	//igrfc(lattitude, longitude, altitude, &F);

	//std::cout << "lat long alt: " << lattitude << " " << longitude << " " << altitude << std::endl;

	
	//double fm[6];
	//igrfm(fm);
	double BE, BN, BD;
	this->model(2020., lattitude, longitude, altitude,BE, BN, BD );

	glm::vec3 NED(BN, BE, -BD);
	NED = NED * 1e-9 ;
//	std::cout << "NED: " << NED.x << " " << NED.y << " " << NED.z << std::endl;
	return TIB() * NED;



}



glm::vec4 Orbit::RotationalKinematics( glm::vec4& _q0123, glm::vec3& _pqr)
{
	glm::mat4 PQR(0, -_pqr.x, -_pqr.y, -_pqr.z,
				_pqr.x, 0, _pqr.z, -_pqr.y,
				_pqr.y, -_pqr.z, 0, _pqr.x,
				_pqr.z, _pqr.y, -_pqr.x, 0);
	return  0.5f * (PQR * _q0123);
	

}

vector<double> Orbit::getStateVec(glm::vec3 v, glm::vec3 a, glm::vec4 q0123dot, glm::vec3 pqrdot)
{
	vector<double> statevec;
	
	statevec.push_back(v.x);
	statevec.push_back(v.y);
	statevec.push_back(v.z);

	statevec.push_back(a.x);
	statevec.push_back(a.y);
	statevec.push_back(a.z);

	statevec.push_back(q0123dot.x);
	statevec.push_back(q0123dot.y);
	statevec.push_back(q0123dot.z);
	statevec.push_back(q0123dot.w);


	statevec.push_back(pqrdot.x);
	statevec.push_back(pqrdot.y);
	statevec.push_back(pqrdot.z);

	return statevec;
}




void Orbit::Sensor()
{
	MagFieldBias = MagScaleBias * (-1 + 2.0 * (rand() % 100) / 100.0f);
	MagFieldNoise = MagScaleNoise * (-1 + 2.0 * (rand() % 100) / 100.0f);

	 AngleFieldBias = AngleScaleBias * (-1 + 2.0 * (rand() % 100) / 100.0f);
	 AngleFieldNoise = AngleScaleNoise * (-1 + 2.0 * (rand() % 100) / 100.0f);

	 glm::vec3 inertialMagneticField = bFieldI();
//	 std::cout << "Binertial: " << inertialMagneticField.x << std::endl;
	 glm::mat3 temp = TIBQuat();
	 bodyMagneticField = temp * inertialMagneticField;

//	 std::cout << "temp: " << temp[0][0]  << " " << temp[1][1] << " " << temp[2][2] << std::endl;

	 Bmeasure = bodyMagneticField +  (MagFieldBias + MagFieldNoise);
	 Wmeasure =   pqr + (AngleFieldBias + AngleFieldNoise);
//	 std::cout << "mag bias, noise" << MagFieldBias << " " << MagFieldNoise << std::endl;
//	 std::cout << "bodyMag: " << bodyMagneticField.x << std::endl;
	 SensorFilter();


}

//complementary filter
void Orbit::SensorFilter()
{
	float s = 0.3; //how much you trust the measured value
	if (Bfilter == glm::vec3(0.0)) //for first step, if no previous value then have to use raw value
	{
		Bfilter = Bmeasure;
		Wfilter = Wmeasure;
		return;
	}
	glm::vec3 B_biasEstimate(0.0);
	glm::vec3 W_biasEstimate(0.0);
	Bfilter = Bfilter * (1 - s) + s * (Bmeasure - B_biasEstimate);
	Wfilter = Wfilter * (1 - s) + s * (Wmeasure - W_biasEstimate);
//	std::cout << "Bmeasure: " << Bmeasure.x << std::endl;
}

void Orbit::Bdot()
{
	glm::vec3 current = k_bdot * glm::cross(Wfilter, Bfilter) * (1.0f / n*A);
//	std::cout << "Wfiler" << Wfilter.x << "Bfilter" << Bfilter.x << std::endl;
	//saturation of current command
	if ((abs(current.x) + abs(current.y) + abs(current.z)) > .08f)
	{
		current = 0.08f * (current / glm::length(current)); 
	}


	glm::vec3 muB = current * n * A;


	M_mag = glm::cross(muB, bodyMagneticField) * 1e3;
	
	std::cout << "output current" << current.x << " " << current.y << " " << current.z <<  std::endl;
	std::cout << "output torque" << M_mag.x << " " << M_mag.y << " " << M_mag.z <<  std::endl;
}


void Orbit::Disturbances()
{
    this->MagneticDisturbance = this->residualDipole * bodyMagneticField;
}

