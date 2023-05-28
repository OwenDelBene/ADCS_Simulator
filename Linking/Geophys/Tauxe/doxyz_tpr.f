!DEC$ DECLARE
!DEC$ INTEGER:4 	
c________________________________________________________
	subroutine doxyz_tpr(x,y,z,t,p,r)
c
c	calls no other routines.
c	 takes x,y,z components and returns theta (t) and phi (p)
c	 in radians
c
	real*4 t[REFERENCE],p[REFERENCE],r [REFERENCE]
	real*4 x [VALUE],y[VALUE],z [VALUE]
	real pi
	pi=2.0*asin(1.0)
	r=sqrt(x*x+y*y+z*z)
	t=acos(z/r)
   	if (x.eq.0.0) then
        if (y.lt.0) then
                p= 3*pi/2
       		 else 
       		 p= pi/2     
	endif
	return
	endif
        p = (atan(y/x))
        if (x.lt.0) then
        p = p + pi
        endif
c	p= (atan2(y,x)) 
	if (p.lt.0) then
	 p = p+2*pi
	endif
	return
 100	end               
c
