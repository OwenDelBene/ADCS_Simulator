c________________________________________________________________
	subroutine fcalc(nf,f,ierr)
c
c	looks up f from f tables 
	ierr=0
	if(nf.lt.2) then
	write(20,*)'doesnt work on this few samples'
	ierr=1
	return
	endif
	if(nf.ge.2)f=19
	if(nf.ge.3)f=9.55
	if(nf.ge.4)f=6.94
	if(nf.ge.5)f=5.79
	if(nf.ge.6)f=5.14
	if(nf.ge.7)f=4.74
	if(nf.ge.8)f=4.46
	if(nf.ge.9)f=4.26
	if(nf.ge.10)f=4.1
	if(nf.ge.11)f=3.98
	if(nf.ge.12)f=3.89
	if(nf.ge.13)f=3.81
	if(nf.ge.14)f=3.74
	if(nf.ge.15)f=3.68
	if(nf.ge.16)f=3.63
	if(nf.ge.17)f=3.59
	if(nf.ge.18)f=3.55
	if(nf.ge.19)f=3.52
	if(nf.ge.20)f=3.49
	if(nf.ge.21)f=3.47
	if(nf.ge.22)f=3.44
	if(nf.ge.23)f=3.42
	if(nf.ge.24)f=3.4
	if(nf.ge.25)f=3.39
	if(nf.ge.26)f=3.37
	if(nf.ge.27)f=3.35
	if(nf.ge.28)f=3.34
	if(nf.ge.29)f=3.33
	if(nf.ge.30)f=3.32
	if(nf.ge.32)f=3.29
	if(nf.ge.34)f=3.28
	if(nf.ge.36)f=3.26
	if(nf.ge.38)f=3.24
	if(nf.ge.40)f=3.23
	if(nf.ge.60)f=3.15
	if(nf.ge.120)f=3.07
	if(nf.ge.500)f=3
	return
	end