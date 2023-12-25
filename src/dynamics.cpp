#include "../include/Dynamics.h"

using Eigen::Matrix;



/*
StateVector operator*(Matrix<float,6,1>& a, StateVector& b)
{
	return StateVector {
		.pos = {{a(0,0) * b.pos(0,0), a(0,1) * b.pos(0,1), a(0,2) * b.pos(0,2)}},
		.vel = {{a(0,3) * b.vel(0,0), a(0,4) * b.vel(0,1), a(0,5) * b.vel(0,2)}}

	
	};	
}



StateVector operator*(StateVector& b, Matrix<float,6,1>& a)
{
	return StateVector {
		.pos = {{a(0,0) * b.pos(0,0), a(0,1) * b.pos(0,1), a(0,2) * b.pos(0,2)}},
		.vel = {{a(0,3) * b.vel(0,0), a(0,4) * b.vel(0,1), a(0,5) * b.vel(0,2)}}

	
	};	
}
*/

Matrix<float,6,1> translationalDynamics(float time, Matrix<float,6,1> stateVec)
{
	
	Matrix<float,3,1> pos {stateVec(0,0), stateVec(0,1), stateVec(0,2)};
	Matrix<float,3,1> vel {stateVec(0,3), stateVec(0,5), stateVec(0,5)};

	Matrix<float,3,1> accel = pos * (-MU / std::pow(pos.norm(), 3));	
	Matrix<float, 6,1> newState;
	newState << vel;
	newState << accel;
	return  newState;
}	


Matrix<float, 4,1> rotationalKinematics(const Matrix<float,4,1>& _q0123, const Matrix<float,3,1>& _pqr)
{
	
	Matrix<float,4,4> PQR {{0, -_pqr.x(), -_pqr.y(), -_pqr.z()},
						  {_pqr.x(), 0, _pqr.z(), -_pqr.y()},
						  {_pqr.y(), -_pqr.z(), 0, _pqr.x()},
						  {_pqr.z(), _pqr.y(), -_pqr.x(), 0}};
	return  0.5f * (PQR * _q0123);
	

}

Matrix<float,7,1> rotationalDynamics(float time,Matrix<float,7,1> stateVec,  const StateVector& state)
{

	Matrix<float,4,1> attitude = stateVec.segment(0,3);
	Matrix<float,3,1> angularRates = stateVec.segment(0,2);

	Matrix<float,4,1>  q0123dot = rotationalKinematics(attitude, angularRates);
	
	
	Matrix<float,3,1>  H = state.Inertia * angularRates;
	Matrix<float,3,1> pqrDot = state.InvI * (state.M_mag + state.M_wheel - angularRates.cross( H));
	
	Matrix<float,7,1> newState;
	newState << q0123dot, pqrDot;
	return newState;
}


Matrix<float,7,1>  rK4r(const StateVector& state, float time, float step )
{
	Matrix<float,7,1> stateVec;
	stateVec << state.attitude, state.angularRates;
		
	Matrix<float,7,1> k1 = rotationalDynamics(time, stateVec, state); 


	Matrix<float,7,1> k2 = rotationalDynamics(time + .5*step , stateVec + ( k1 * (.5*step)), state); 
 
	Matrix<float,7,1> k3 = rotationalDynamics(time + .5*step, stateVec + k2 * .5 * step, state); 
	
	Matrix<float,7,1> k4 = rotationalDynamics(time + step, stateVec + k3 * step, state); 


	return stateVec + (step / 6.0f) * ((k1 + ( k2 * 2 ) + ( k3 * 2) + k4));
}






Matrix<float,6,1>  rK4t(const StateVector& state, float time, float step )
{
	Matrix<float,6,1> stateVec;
	stateVec << state.pos, state.vel;
		
	Matrix<float,6,1> k1 = translationalDynamics(time, stateVec); 


	Matrix<float,6,1> k2 = translationalDynamics(time + .5*step , stateVec + ( k1 * (.5*step))); 
 
	Matrix<float,6,1> k3 = translationalDynamics(time + .5*step, stateVec + k2 * .5 * step); 
	
	Matrix<float,6,1> k4 = translationalDynamics(time + step, stateVec + k3 * step); 


	return stateVec + (step / 6.0f) * ((k1 + ( k2 * 2 ) + ( k3 * 2) + k4));
}



vector<float> _calculateAngles(StateVector& state)
{
	float rho = state.pos.norm();
	state.phi = 0;
	state.theta = acos(state.pos(0,2) / rho); //rho is mag(pos)
	state.psi = atan2(state.pos(0,1), state.pos(0,0));
	float lattitude = 90 - state.theta * 180 / M_PI;
	float longitude = state.psi * 180 / M_PI;
	float altitude = (rho - RE);


	return vector<float>({lattitude, longitude, altitude});
}


Matrix<float,3,1> bFieldI(StateVector& state)
{
	vector<float> lla = _calculateAngles(state);
	
	float lattitude = lla.at(0);
	float longitude = lla.at(1);
	float altitude  = lla.at(2);

	double BE, BN, BD;
	state.model(2020., lattitude, longitude, altitude,BE, BN, BD );

	Matrix<float,3,1> NED {BN, BE, -BD};
	NED = NED * 1e-9 ;
	return tIB(state) * NED;
}




Matrix<float,3,3> tIB(const StateVector& state )
{
	
	//std::cout << "theta, phi, psi" << theta << " " << phi << " " << psi << std::endl;
	double ct = cos(state.theta);
	double st = sin(state.theta);
	double sp = sin(state.phi);
	double cp = cos(state.phi);
	double ss = sin(state.psi);
	double cs = cos(state.psi);

	Matrix<float,3,3> TIB{ {ct * cs, sp * st * cs - cp * ss, cp * st * cp + sp * ss},
								{ct * sp, sp * st * ss + cp * cs, cp * st * ss - sp * cs},
								{-st, sp * ct, cp * ct}};
	return TIB;
}




Matrix<float,3,3> tIBQuat(const StateVector& state)
{


	double q0 = state.attitude(0,0);
	double q1 = state.attitude(0,1);
	double q2 = state.attitude(0,2);
	double q3 = state.attitude(0,3);
	
	

	double q02 = pow(q0, 2);
	double q12 = pow(q1, 2);
	double q22 = pow(q2, 2);
	double q32 = pow(q3, 2);

		
	return Matrix<float,3,3> {{q02+q12-q22-q32, 2*(q1*q1-q0*q3), 2*(q0*q2*q1*q3)},
		   {2*(q1*q2+q0*q3), q02-q12+q22-q32, 2*(q2*q3-q0*q1)},
		   {		 2*(q1*q3-q0*q2), 2*(q0*q1+q2*q1), q02-q12-q22+q32}
			};
}



void sunSensor(StateVector& state)
{
/*

function [S_o, S_b] = SunSensor(omega,Omega,inc,T,C,sigma_s,time,N)
for i = 1:N+1
    JD = JDate(2022,1,1,0,0,i-1);
    T_TDB = (JD - 2451545.0) / 36525;
    
    Lamdba_Msun = 280.460 + 36000.770 * T_TDB; %mean longitude of the sun (deg)
    M_sun = 357.5277233 + 35999.05034 * T_TDB; %mean anomaly of the sun (deg)
    Lamdba_ec = Lamdba_Msun + 1.914666471 * sind(M_sun) ...
        + 0.019994643 * sind(2 * M_sun); %ecliptic longitude of the sun (deg)
    
    %linear model of the ecliptic of the sun (deg)
    e_linear = 23.439291 - 0.0130042 * T_TDB;
    
    S_ECI = [cosd(Lamdba_ec);...
        sind(Lamdba_ec) * cosd(e_linear);...
        sind(Lamdba_ec) * sind(e_linear)];
    
    %Orbital Parameters
    v = 2 * pi * (time(i)/ T);  %true anomaly (rad)
    u = omega + rad2deg(v);     %Argument of latitude (deg)
    
    %Transformation Matrix ECI to Orbit Frame
    Tr = [-cosd(u)*cosd(inc)*sind(Omega)-sind(u)*cosd(Omega),...
        cosd(u)*cosd(inc)*cosd(Omega)-sind(u)*sind(Omega),...
        cosd(u)*sind(inc);
        ...
        -sind(inc)*sind(Omega),...
        sind(inc)*cosd(Omega),...
        -cosd(inc);
        ...
        sind(u)*cosd(inc)*sind(Omega)-cosd(u)*cosd(Omega),...
        -sind(u)*cosd(inc)*cosd(Omega)-cosd(u)*sind(Omega),...
        -sind(u)*sind(inc)];
    %Sun direction vectors in Orbit frame
    S_o0 = Tr * S_ECI;
    S_o(:,i) = S_o0 ./ norm(S_o0);
    
    Sxo(i,1) = S_o(1,i);
    Syo(i,1) = S_o(2,i);
    Szo(i,1) = S_o(3,i);
    
    %Sun direction vectors in Body frame
    S_b0 = C(i).a * S_o(:,i) + sigma_s * randn(3,1);
    S_b(:,i) = S_b0 ./ norm(S_b0);
end
end

*/

}




float getJulianDay()
{
	std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
	time_t tt = std::chrono::system_clock::to_time_t(now);
	tm utc_tm = *gmtime(&tt);
	tm local_tm = *localtime(&tt);
	
	int month = local_tm.tm_mon;
	int day = local_tm.tm_mday;
	int year = local_tm.tm_year;
	int a, m, y, leap_days;  
   a = (14 - month) / 12;  
   m = (month - 3) + (12 * a);  
   y = year + 4800 - a;  
   leap_days = (y / 4) - (y / 100) + (y / 400);  
   return day + (((153 * m) + 2) / 5) + (365 * y) + leap_days - 32045;  

}




void sensorModel(StateVector& state)
{
	 float MagFieldBias = state.MagScaleBias * (-1 + 2.0 * (rand() % 100) / 100.0f);
	 float MagFieldNoise = state.MagScaleNoise * (-1 + 2.0 * (rand() % 100) / 100.0f);

	 float AngleFieldBias = state.AngleScaleBias * (-1 + 2.0 * (rand() % 100) / 100.0f);
	 float AngleFieldNoise = state.AngleScaleNoise * (-1 + 2.0 * (rand() % 100) / 100.0f);

	 Matrix<float,3,1>  inertialMagneticField = bFieldI(state);
	 Matrix<float, 3,3> DCM = tIBQuat(state);
	 Matrix<float,3,1>  bodyMagneticField = DCM * inertialMagneticField;

	 Matrix<float,3,1> bias {1,2,3};
	 Matrix<float,3,1> Bbias = bias * MagFieldBias;
	 Matrix<float,3,1> Wbias = bias * AngleFieldBias;
	 state.Bmeasure = bodyMagneticField + (Bbias  * (1+ MagFieldNoise));
	 state.Wmeasure =   state.angularRates + ( (Wbias * (1+ AngleFieldNoise)));


}












