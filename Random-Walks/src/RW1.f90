program exercize_1
implicit none
!-----------------------------------------------------------------------
!  compile with: gfortran -cpp -fopenmp -O3 -march=native exercize1.f90
!-----------------------------------------------------------------------

integer :: N  ! number of steps
integer, dimension(:), allocatable :: seed
integer :: i, ix, irun, istep, nruns, sizer, clock
real :: p_right, p_left
real :: delta
real, dimension(:), allocatable :: rnd          			! array of random numbers
real, dimension(:), allocatable :: x_N, x2_N, MSD  		! average position, square position, square displacement after N steps
integer, dimension(:), allocatable :: P_N       			! final positions, sum over runs
! function declaration
real :: P_th
integer :: factorial
real		:: P_gauss

#define _USE_OMP TRUE
#define _PLOT TRUE

! initalizing variables:
N = 100
p_right = 0.5
p_left = 1.0 - p_right

allocate(rnd(N))
allocate(x_N(N))
allocate(x2_N(N))
allocate(MSD(N))
allocate(P_N(-N:N))
x_N  = 0
x2_N = 0
P_N  = 0
MSD  = 0
! allocate the seed for the random number generation:
call RANDOM_SEED(sizer)
allocate(seed(sizer))

#if 0
!----------------------------------------------------------------
!  perform the simulation of a single random walker, few times,
!  each time with a different value of the seed
!----------------------------------------------------------------
open(7, file="ix.dat", STATUS="REPLACE", ACTION="WRITE")
open(8, file="ix2.dat", STATUS="REPLACE", ACTION="WRITE")
do irun = 1, 5, 1
	! generate the seed with the clock:
	CALL SYSTEM_CLOCK(COUNT=clock)
	seed = clock + 37 * (/ (i - 1, i = 1, sizer) /) + irun
	CALL RANDOM_SEED(PUT = seed)
  ix = 0 ! initial position of each run
	write(7,*) 0, ix
	write(8,*) 0, ix**2
  call random_number(rnd) ! get a sequence of random numbers
  do istep = 1, N
    if (rnd(istep) < p_left) then ! random move
      ix = ix - 1 ! left
    else
      ix = ix + 1 ! right
    end if
    write(7,*) istep, ix
    write(8,*) istep, ix**2
  end do
  write(7,*)
  write(7,*)
  write(8,*)
  write(8,*)
  ! print summarizing quantities:
  print*, "simulation of a single random walker: "
	print*, "x_N = ", ix
	print*, "x^2_N = ", ix**2
	print*, "x_N - <x_N>(th) = ", ix - 0
	print*, "x^2_N - <x_N^2>(th) = ", ix**2 - N
end do
close(7)
close(8)
#endif


!----------------------------------------------------------------
!  perform the simulation averaging over many walkers, choosing
!  some values for the number of walkers. Study the MSD accuracy
!----------------------------------------------------------------
! generate the seed with the clock:
CALL SYSTEM_CLOCK(COUNT=clock)
seed = clock + 37 * (/ (i - 1, i = 1, sizer) /)
CALL RANDOM_SEED(PUT = seed)
open(12, file="x_N.dat", STATUS="REPLACE", ACTION="WRITE")
open(13, file="x2_N.dat", STATUS="REPLACE", ACTION="WRITE")
open(14, file="MSD.dat", STATUS="REPLACE", ACTION="WRITE")
open(15, file="delta.dat", STATUS="REPLACE", ACTION="WRITE")

do nruns = 401, 40, -40
x_N  = 0
x2_N = 0
P_N  = 0
MSD  = 0

#ifdef _USE_OMP
!$OMP PARALLEL DO PRIVATE(irun, istep, ix, rnd) SHARED(p_left, nruns, N) REDUCTION(+ : x_N, x2_N, P_N) NUM_THREADS(10)
! perform now the average over many walkers:
do irun = 1, nruns, 1
  ix = 0 ! initial position of each run
  call random_number(rnd) ! get a sequence of random numbers
  do istep = 1, N
    if (rnd(istep) < p_left) then ! random move
      ix = ix - 1 ! left
    else
      ix = ix + 1 ! right
    end if
    x_N (istep) = x_N (istep) + ix
    x2_N(istep) = x2_N(istep) + ix**2
  end do
  !P_N(ix) = P_N(ix) + 1 ! accumulate (only for istep = N)
end do
!$OMP END PARALLEL DO
#endif

! calculate the effective averages over the number of walks, and compute the MSD:
do i = 1, N, 1
	x_N(i) = x_N(i) / nruns
	x2_N(i) = x2_N(i) / nruns
	MSD(i) = x2_N(i) - x_N(i)**2
end do
! calculate the delta function: (it can be also an array)
delta = abs(MSD(N)/N - 1)
! write into file:
do i = 1, N, 1
	write(12,*) i, x_N(i)
	write(13,*) i, x2_N(i)
	write(14,*) i, MSD(i)
end do
write(12,*)
write(12,*)
write(13,*)
write(13,*)
write(14,*)
write(14,*)
write(15,*) nruns, delta

end do ! cycle on nruns
close(12)
close(13)
close(14)
close(15)


!--------------------------------------------------------------------
!  now fix the number of walkers such that the accuracy is about 5%:
!  Study the behaviour of P_N and the theoretical functions.
!--------------------------------------------------------------------
nruns = 200
x_N  = 0
x2_N = 0
P_N  = 0
MSD  = 0

! generate the seed with the clock:
CALL SYSTEM_CLOCK(COUNT=clock)
seed = clock + 37 * (/ (i - 1, i = 1, sizer) /)
CALL RANDOM_SEED(PUT = seed)

#ifdef _USE_OMP
!$OMP PARALLEL DO PRIVATE(irun, istep, ix, rnd) SHARED(p_left, nruns, N) REDUCTION(+ : x_N, x2_N, P_N) NUM_THREADS(10)
do irun = 1, nruns, 1
  ix = 0 ! initial position of each run
  call random_number(rnd) ! get a sequence of random numbers
  do istep = 1, N
    if (rnd(istep) < p_left) then ! random move
      ix = ix - 1 ! left
    else
      ix = ix + 1 ! right
    end if
    x_N (istep) = x_N (istep) + ix
    x2_N(istep) = x2_N(istep) + ix**2
  end do
  P_N(ix) = P_N(ix) + 1 ! accumulate (only for istep = N)
end do
!$OMP END PARALLEL DO
#endif

! calculate the effective averages over the number of walks, and compute the MSD:
do i = 1, N, 1
	x_N(i) = x_N(i) / nruns
	x2_N(i) = x2_N(i) / nruns
	MSD(i) = x2_N(i) - x_N(i)**2
end do
! calculate the delta function: (it can be also an array of length N)
! check is it is < 0.05
delta = abs(MSD(N)/N - 1)

! print summarizing quantities in the final step:
print*, "simulation averaging over many walkers: "
print*, "N = ", N," ; nruns = ", nruns
print*, "<x_N> = ", x_N(N)
print*, "<x^2_N> = ", x2_N(N)
print*, "<x^2_N> - <x_N>^2 = ", MSD(N)
print*, "delta = ", delta
! print numerical estimated values for MSD(N). The theoretical value is equal to N 
! with p_left = p_right = 1/2, and l = 1.
print*, MSD(8), MSD(16), MSD(32), MSD(64)

! write into file:
open(10, file="averages.dat", STATUS="REPLACE", ACTION="WRITE")
do i = 1, N, 1
	write(10,*) i, x_N(i)
end do
write(10,*)
write(10,*)
do i = 1, N, 1
	write(10,*) i, x2_N(i)
end do
write(10,*)
write(10,*)
do i = 1, N , 1
	write(10,*) i, MSD(i)
end do
close(10)
open(11, file="P_N.dat", STATUS="REPLACE", ACTION="WRITE")
do ix = -N, N
	write(11,*) ix, real(P_N(ix))/nruns
end do
write(11,*)
write(11,*)
do ix = -N, N
	! check if the function P_th exists, i.e if the number (N+x)/2 is integer
	! so its factorial is defined:
	if (mod(N+ix,2) == 0) write(11,*) ix, P_th(ix, N, 0.5, 0.5)
end do
close(11)
open(16, file="P_N_large.dat", STATUS="REPLACE", ACTION="WRITE")
do ix = -N, N
	write(16,*) ix, real(P_N(ix))/nruns
end do
write(16,*)
write(16,*)
do ix = -N, N
	write(16,*) ix, 2*P_gauss(real(ix), x_N(N), MSD(N))
end do
close(16)

deallocate (rnd, x_N, x2_N, P_N, seed, MSD)
end program exercize_1


!--------------------------------------------------------------------
!  auxilliary functions:
!--------------------------------------------------------------------

integer function factorial(n) result(fact)
  implicit none
  integer, intent (in) :: n
  integer :: i
  fact = 1
	do i = 1, n, 1
    fact = fact * i
  end do
end function factorial

real function P_th(x, N, p_right, p_left) result (P)
	implicit none
	integer :: factorial
	integer, intent (in)  :: N, x
	real, intent (in)     :: p_right, p_left
	!P = ( factorial(N)*p_right**((N+x)/2)*P_left**((N-x)/2) )/( factorial((N+x)/2)*factorial((N-x)/2) )
	!P = ( gamma(real(N))*p_right**((N+x)/2)*P_left**((N-x)/2) )/( gamma((N+x)/2)*gamma((N-x)/2) )
	!P = ( gamma(real(N))*0.5**N )/( gamma((N+x)/2)*gamma((N-x)/2) )
	! with p_left = p_right = 0.5
	P = ( factorial(N)*(0.5**N) )/( factorial((N+x)/2)*factorial((N-x)/2) )
end function P_th

real function P_gauss(x, MU, MSD) result (P)
	implicit none
	real, intent (in)     :: x, MU, MSD
	real, parameter				:: pi = acos(-1.0)
	P = exp((-(x-MU)**2)/(2*MSD)) / sqrt(2*pi*MSD)
end function P_gauss





