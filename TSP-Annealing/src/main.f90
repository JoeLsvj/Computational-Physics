PROGRAM TSP_anneal
! Simulated annealing for Travel Salesman Problem
USE TSP
#define _OMP

IMPLICIT NONE
REAL, DIMENSION(:,:),  ALLOCATABLE  :: nodes
REAL, DIMENSION(:,:),  ALLOCATABLE  :: distance_matrix
INTEGER, DIMENSION(:), ALLOCATABLE  :: route, new_route, opt_route
CHARACTER(len=10),     ALLOCATABLE  :: args(:)
!INTEGER, DIMENSION(:),ALLOCATABLE  :: seed
INTEGER                             :: number_of_nodes, istep, nsteps
INTEGER                             :: i, start, end, num_args!, sizer
REAL                                :: energy, energy_min, energy_new, initial_energy
REAL                                :: temp, tfactor
REAL                                :: rand

! for testing purposes:
!CALL RANDOM_SEED(sizer)
!ALLOCATE(seed(sizer))
!seed = 10003
!CALL RANDOM_SEED(PUT=seed)

! default values
number_of_nodes = 48; temp = 10; tfactor = 0.9; nsteps = 1000
! parsing arguments:
num_args = command_argument_count()
allocate(args(num_args))
do i = 1, num_args
    call get_command_argument(i,args(i)) 
end do
read(args(1), *) number_of_nodes
read(args(2), *) temp
read(args(3), *) tfactor
read(args(4), *) nsteps

ALLOCATE(nodes(number_of_nodes, 2))
ALLOCATE(distance_matrix(number_of_nodes, number_of_nodes))
ALLOCATE(route(number_of_nodes), new_route(number_of_nodes), opt_route(number_of_nodes))

! Initialize the system
CALL RANDOM_INIT(number_of_nodes, nodes, route)
CALL COMPUTE_DISTANCE_MATRIX(number_of_nodes, nodes, distance_matrix)
! compute the energy for a particular route
CALL COMPUTE_ENERGY(number_of_nodes, distance_matrix, route, energy)
energy_min = energy
initial_energy = energy
! save into file and print initial system's energy
OPEN(10, file="nodes.csv", STATUS="REPLACE", ACTION="WRITE")
DO i = 1, number_of_nodes, 1
  WRITE(10,'(*(G0.6,:,","))') nodes(i, :)
END DO
CLOSE(10)
OPEN(11, file="routes.csv", STATUS="REPLACE", ACTION="WRITE")
WRITE(11,'(*(G0.6,:,","))') route
!print*, "Energy of the initial state (route): ", energy

! SIMULATED ANNEALING:
OPEN(12, file="min_energy.txt",      STATUS="REPLACE", ACTION="WRITE")
OPEN(13, file="accepted_energy.txt", STATUS="REPLACE", ACTION="WRITE")
i = 0
!CALL SYSTEM_CLOCK(start)
DO WHILE (temp > 1E-5)        ! anneal cycle
  DO istep = 1, nsteps        ! steps for each value of temperature
    CALL RANDOM_NUMBER(rand)  ! acceptance probability of Metropolis
    CALL STOCHASTIC_MOVE(route, new_route)
    CALL COMPUTE_ENERGY(number_of_nodes, distance_matrix, new_route, energy_new)
    IF (EXP(-(energy_new - energy)/temp) > rand) THEN ! success, save
      energy = energy_new
      route = new_route
      WRITE(13, *) i*nsteps + istep, energy
    END IF
    IF (energy < energy_min) THEN
      energy_min = energy
      opt_route = route
      ! print if a new minimum is found:
      WRITE(12, *) i*nsteps + istep, energy_min
      WRITE(11,'(*(G0.6,:,","))') opt_route
      !PRINT*, temp, energy_min
    END IF
  END DO
  i = i + 1
  temp = temp * tfactor ! decrease temperature
END DO
!CALL SYSTEM_CLOCK(end)
!print*, (end - start) / 1000.0
!print*, "time required: ", (end - start) / 1000.0, " s"
print*, (initial_energy - energy_min) * 100 / initial_energy

CLOSE(11)
CLOSE(12)
CLOSE(13)

END PROGRAM TSP_anneal
