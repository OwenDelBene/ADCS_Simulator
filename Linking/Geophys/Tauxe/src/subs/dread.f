c_______________________________________________________________________

	subroutine dread(n,theta,phi)
c$$$$ calls no other routines
      dimension theta(*),phi(*)
	real dec,inc
	pi=2.0*asin(1.0)
        rad=pi/180
c
	do 10 i=1,3000
         read (*,*, end=1100) dec,inc
	  phi(i)=dec*rad
	  theta(i)=(90-inc)*rad
 10	continue
 1100   n=i-1
	return
	end
c