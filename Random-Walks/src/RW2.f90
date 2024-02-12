program exercize_2
implicit none
!-----------------------------------------------------------------------
!  compile with: gfortran -cpp -fopenmp -O3 -march=native exercize2.f90
!-----------------------------------------------------------------------

	integer, parameter :: rk = selected_real_kind(13)
	real(kind=rk), parameter 	 :: pi = acos(-1.0_rk)
	real(kind=rk)      :: dR, xi, yi, rnd, rnd2, aux
	integer            :: N, nruns, sizer, i, irun, istep, clock
	integer, dimension(:), allocatable        :: seed
	real (kind=rk), dimension(:), allocatable :: x_N, x2_N, y_N, y2_N
	integer, dimension(0:3) :: Ndir
	
#define _USE_OMP TRUE
#define _PLOT TRUE

	call random_seed(sizer)
	allocate(seed(sizer))
	CALL SYSTEM_CLOCK(COUNT=clock)
	seed = clock + 37 * (/ (i - 1, i = 1, sizer) /)
	call random_seed(put=seed)

	! initialize variables
	N = 100
	nruns = 200

	allocate(x_N(N))
	allocate(x2_N(N))
	allocate(y_N(N))
	allocate(y2_N(N))
	

!----------------------------------------------------------------
! first algorithm: l=1, phi with uniform distribution in [0, pi]
!----------------------------------------------------------------

	x_N  = 0.0_rk
	x2_N = 0.0_rk
	y_N  = 0.0_rk
	y2_N = 0.0_rk
	
	do irun = 1, nruns
		xi = 0.0_rk
		yi = 0.0_rk

		do istep = 1, N

			call random_number(rnd)
			rnd = rnd*2.0_rk*pi
			xi = xi + cos(rnd)
			yi = yi + sin(rnd)
			x_N(istep) = x_N(istep) + xi
			x2_N(istep) = x2_N(istep) + xi**2
			y_N(istep) = y_N(istep) + yi
			y2_N(istep) = y2_N(istep) + yi**2

		end do

	end do
	
	open(10, file="dR1.dat", STATUS="REPLACE", ACTION="WRITE")
	do i = 1, N
		dR = x2_N(i)/real(nruns) - (x_N(i)/real(nruns))**2 + y2_N(i)/real(nruns) - (y_N(i)/real(nruns))**2
		write(unit=10, fmt=*) i, dR
	end do
	print*, x2_N(8)/real(nruns) -  (x_N(8)/real(nruns))**2  + y2_N(8)/real(nruns)  - (y_N(8)/real(nruns))**2
	print*, x2_N(16)/real(nruns) - (x_N(16)/real(nruns))**2 + y2_N(16)/real(nruns) - (y_N(16)/real(nruns))**2
	print*, x2_N(32)/real(nruns) - (x_N(32)/real(nruns))**2 + y2_N(32)/real(nruns) - (y_N(32)/real(nruns))**2
	print*, x2_N(64)/real(nruns) - (x_N(64)/real(nruns))**2 + y2_N(64)/real(nruns) - (y_N(64)/real(nruns))**2


!-------------------------------------------------------------------
! second algorithm: xi, yi, rnd in [-1;1], then normalize to l = 1
!-------------------------------------------------------------------
	! initialize cumulative variables
	x_N = 0.0_rk
	x2_N = 0.0_rk
	y_N = 0.0_rk
	y2_N = 0.0_rk

! this loop is almost embarassingly parallel task
!$OMP PARALLEL DO PRIVATE(irun, istep, rnd, rnd2, xi, yi) REDUCTION(+ : x_N, x2_N, y_N, y2_N) NUM_THREADS(10)
	do irun = 1, nruns, 1
		xi = 0.0_rk
		yi = 0.0_rk
		do istep = 1, N, 1
			do
				call random_number(rnd)
				call random_number(rnd2)
				if (rnd /= 0 .or. rnd2 /= 0) exit
     	end do
     	! uniform distribution in [-1,1]
      rnd = (rnd - 0.5_rk)*2.0_rk 
      rnd2 = (rnd2 - 0.5_rk)*2.0_rk
			xi = xi + rnd/sqrt(rnd**2 + rnd2**2)
			yi = yi + rnd2/sqrt(rnd**2 + rnd2**2)
			x_N(istep) = x_N(istep) + xi
			x2_N(istep) = x2_N(istep) + xi**2
			y_N(istep) = y_N(istep) + yi
			y2_N(istep) = y2_N(istep) + yi**2
		end do
	end do
!$OMP END PARALLEL DO

	open(11, file="dR2.dat", STATUS="REPLACE", ACTION="WRITE")
	do i= 1, N
		dR = x2_N(i)/real(nruns) - (x_N(i)/real(nruns))**2 + y2_N(i)/real(nruns)-(y_N(i)/real(nruns))**2
		write(unit=11, fmt=*) i, dR
	end do
	

!---------------------------------------------------------------------------
! third algorithm: xi, yi, rnd in [-sqrt(1.5);sqrt(1.5)], l = 1 on average
!---------------------------------------------------------------------------

	x_N  = 0.0_rk
	x2_N = 0.0_rk
	y_N  = 0.0_rk
	y2_N = 0.0_rk

!$OMP PARALLEL DO PRIVATE(irun, istep, rnd, rnd2, xi, yi, aux) REDUCTION(+ : x_N, x2_N, y_N, y2_N) NUM_THREADS(10)
	do irun = 1, nruns, 1
		xi = 0.0_rk
		yi = 0.0_rk
    aux = 0.0_rk  ! check if on average the step has length l = 1
		do istep = 1, N, 1
			call random_number(rnd)
			call random_number(rnd2)
			rnd = (rnd - 0.5_rk)*2.0_rk*sqrt(1.5_rk)
			rnd2 = (rnd2 - 0.5_rk)*2.0_rk*sqrt(1.5_rk)
			xi = xi + rnd
			yi = yi + rnd2
			x_N(istep) = x_N(istep) + xi
			x2_N(istep) = x2_N(istep) + xi**2
			y_N(istep) = y_N(istep) + yi
			y2_N(istep) = y2_N(istep) + yi**2
      aux = aux + rnd**2 + rnd2**2
		end do
    aux = aux/N
    !print*, "<Δx^2+Δy^2> in walk n.", i, " :", aux
	end do
!$OMP END PARALLEL DO

	open(12, file="dR3.dat", STATUS="REPLACE", ACTION="WRITE")
	do i = 1, N
		dR = x2_N(i)/real(nruns) - (x_N(i)/real(nruns))**2 + y2_N(i)/real(nruns) - (y_N(i)/real(nruns))**2
		write(unit=12, fmt=*) i, dR, x_N(i)/real(nruns)
	end do
	
!---------------------------------------------------------------------------
! simulation on a lattice grid
!---------------------------------------------------------------------------
! The steps (left, right up or down) are chosen at random by taking a random
! number in [0-1]. Steps to the right for 0 < = rand < 0.25, i. e. for floor(rand*4)=0 etc.
! The vector Ndir(0:3) contains how many steps are taken in each direction:
! right (Ndir(0)), left (Ndir(1)), up (Ndir(2)), down (Ndir(3))

x_N  = 0.0_rk
x2_N = 0.0_rk
y_N  = 0.0_rk
y2_N = 0.0_rk

do irun = 1, nruns, 1
	xi = 0.0_rk
	yi = 0.0_rk
	do istep = 1, N, 1
		call random_number(rnd)
		select case(floor(rnd*4))
			case(0)
				Ndir(0) = Ndir(0) + 1
				xi = xi + 1
			case(1)
				Ndir(1) = Ndir(1) + 1
				xi = xi - 1
			case(2)
				Ndir(2) = Ndir(2) + 1
				yi = yi + 1
			case(3)
				Ndir(3) = Ndir(3) + 1
				yi = yi - 1
		end select
		x_N(istep)  = x_N(istep)  + xi
		x2_N(istep) = x2_N(istep) + xi**2
		y_N(istep)  = y_N(istep)  + yi
		y2_N(istep) = y2_N(istep) + yi**2
	end do

end do

open(13, file="dR_lattice.dat", STATUS="REPLACE", ACTION="WRITE")
	do i = 1, N
		dR = x2_N(i)/real(nruns) - (x_N(i)/real(nruns))**2 + y2_N(i)/real(nruns) - (y_N(i)/real(nruns))**2
		write(unit=13, fmt=*) i, dR
	end do

end program exercize_2


