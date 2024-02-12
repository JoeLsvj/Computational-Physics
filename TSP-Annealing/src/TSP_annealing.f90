MODULE TSP

USE OMP_LIB
#define _OMP

contains

SUBROUTINE RANDOM_INIT(number_of_nodes, nodes, route)
IMPLICIT NONE
INTEGER               :: i
INTEGER, INTENT (IN)  :: number_of_nodes
REAL, DIMENSION(2)    :: random_numbers
REAL, DIMENSION(:, :), ALLOCATABLE, INTENT (INOUT) :: nodes
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (INOUT) :: route
! check the arrays allocation:
IF (ALLOCATED(nodes) .AND. ALLOCATED(route)) THEN 
  CONTINUE
ELSE
  ALLOCATE(nodes(number_of_nodes, 2), route(number_of_nodes))
END IF
! generate random nodes and initialize a route:
!$OMP PARALLEL DO PRIVATE(random_numbers) SHARED(number_of_nodes, nodes, route)
DO i = 1, number_of_nodes, 1
  CALL RANDOM_NUMBER(random_numbers)
  nodes(i, 1) = random_numbers(1)*sqrt(real(number_of_nodes))
  nodes(i, 2) = random_numbers(2)*sqrt(real(number_of_nodes))
  route(i) = i
END DO
!$OMP END PARALLEL DO
RETURN
END SUBROUTINE RANDOM_INIT

SUBROUTINE COMPUTE_DISTANCE_MATRIX(number_of_nodes, nodes, distance_matrix)
IMPLICIT NONE
INTEGER,                            INTENT (IN)    :: number_of_nodes
REAL, DIMENSION(:, :), ALLOCATABLE, INTENT (IN)    :: nodes
REAL, DIMENSION(:, :), ALLOCATABLE, INTENT (INOUT) :: distance_matrix
INTEGER                                            :: i, j
REAL                                               :: x1, x2, y1, y2
!$OMP PARALLEL DO COLLAPSE(2) PRIVATE(x1,x2,y1,y2) SHARED(distance_matrix, nodes, number_of_nodes)
DO i = 1, number_of_nodes, 1
  DO j = 1, number_of_nodes, 1
    x1 = nodes(i, 1); y1 = nodes(i, 2)
    x2 = nodes(j, 1); y2 = nodes(j, 2)
    distance_matrix(i, j) = sqrt((x2-x1)**2+(y2-y1)**2)
  END DO
END DO
!$OMP END PARALLEL DO
RETURN
END SUBROUTINE COMPUTE_DISTANCE_MATRIX

SUBROUTINE COMPUTE_ENERGY(number_of_nodes, distance_matrix, route, energy)
IMPLICIT NONE
INTEGER,                               INTENT (IN)  :: number_of_nodes
REAL,     DIMENSION(:,:), ALLOCATABLE, INTENT (IN)  :: distance_matrix
INTEGER,  DIMENSION(:),   ALLOCATABLE, INTENT (IN)  :: route
REAL,                                  INTENT (OUT) :: energy
INTEGER                                             :: i
IF (ALLOCATED(distance_matrix) .AND. ALLOCATED(route)) THEN 
  CONTINUE
ELSE 
  print*, "not allocated arrays"
  RETURN
END IF
energy = 0.
! compute the energy for a particular route
DO i = 1, number_of_nodes-1, 1
  energy = energy + distance_matrix(route(i), route(i+1))
END DO
! add the closing segment:
energy = energy + distance_matrix(route(number_of_nodes), route(1))
RETURN
END SUBROUTINE COMPUTE_ENERGY

SUBROUTINE STOCHASTIC_SWAP(route, new_route)
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (IN)    :: route
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (INOUT) :: new_route
REAL, DIMENSION(2)  :: rand
INTEGER             :: rnd1, rnd2
INTEGER             :: number_of_nodes
! check the correct allocation of arrays:
IF (ALLOCATED(route) .AND. ALLOCATED(new_route)) THEN
  CONTINUE
ELSE
  print*, "not allocated arrays"
  RETURN
END IF
! define the random swap
number_of_nodes = SIZE(new_route)
DO
  CALL RANDOM_NUMBER(rand)
  rnd1 = int(rand(1)*number_of_nodes) + 1
  rnd2 = int(rand(2)*number_of_nodes) + 1
  IF (rnd1 .NE. rnd2) EXIT
END DO
new_route = route
new_route(rnd1) = route(rnd2)
new_route(rnd2) = route(rnd1)
END SUBROUTINE STOCHASTIC_SWAP

SUBROUTINE STOCHASTIC_INSERT(route, new_route)
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (IN)    :: route
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (INOUT) :: new_route
REAL, DIMENSION(2)  :: rand
INTEGER             :: rnd1, rnd2, i
INTEGER             :: number_of_nodes
! check the correct allocation of arrays:
IF (ALLOCATED(route) .AND. ALLOCATED(new_route)) THEN
  CONTINUE
ELSE
  print*, "not allocated arrays"
  RETURN
END IF
! define the random insertion
number_of_nodes = SIZE(new_route)
DO
  CALL RANDOM_NUMBER(rand)
  rnd1 = int(rand(1)*number_of_nodes) + 1
  rnd2 = int(rand(2)*number_of_nodes) + 1
  IF (rnd1 .NE. rnd2) EXIT
END DO
new_route = route
! if only new_route is passed to the subroutine, store new_route(rnd1) in tmp variable
IF (rnd1 > rnd2) THEN
  DO i = rnd1, rnd2+1, -1
    new_route(i) = route(i-1)
  END DO
  new_route(rnd2) = route(rnd1)
ELSE
  DO i = rnd1, rnd2-1, 1
    new_route(i) = route(i+1)
  END DO
  new_route(rnd2) = route(rnd1)
END IF
END SUBROUTINE STOCHASTIC_INSERT

SUBROUTINE STOCHASTIC_INVERSE(route, new_route)
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (IN)    :: route
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (INOUT) :: new_route
REAL, DIMENSION(2)  :: rand
INTEGER             :: rnd1, rnd2, i
INTEGER             :: number_of_nodes
! check the correct arrays allocation
IF (ALLOCATED(route) .AND. ALLOCATED(new_route)) THEN
  CONTINUE
ELSE
  print*, "not allocated arrays"
  RETURN
END IF
! define the stochastic inversion
number_of_nodes = SIZE(new_route)
DO
  CALL RANDOM_NUMBER(rand)
  rnd1 = int(rand(1)*number_of_nodes) + 1
  rnd2 = int(rand(2)*number_of_nodes) + 1
  IF (rnd1 .NE. rnd2) EXIT
END DO
new_route = route
! we can also set rnd2 instead of rnd2-1: the last choice avoids the route inversion
! that is not useful
DO i = rnd1, rnd2, SIGN(1, rnd2-rnd1)
  new_route(i) = route(rnd2-i+rnd1)
END DO
END SUBROUTINE STOCHASTIC_INVERSE

SUBROUTINE STOCHASTIC_MOVE(route, new_route)
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (IN)     :: route
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (INOUT)  :: new_route
!INTEGER                                             :: number_of_nodes
REAL                                                :: rnd
! check the correct arrays allocation
IF (ALLOCATED(route) .AND. ALLOCATED(new_route)) THEN
  CONTINUE
ELSE
  print*, "not allocated arrays"
  RETURN
END IF
new_route = route
! choose randomically a random move among the ones defined:
CALL RANDOM_NUMBER(rnd)
SELECT CASE (int(rnd*3)+1)
  CASE (1)
    CALL STOCHASTIC_SWAP(route, new_route)
  CASE (2)
    CALL STOCHASTIC_INSERT(route, new_route)
  CASE (3)
    CALL STOCHASTIC_INVERSE(route, new_route)
END SELECT
RETURN
END SUBROUTINE STOCHASTIC_MOVE

END MODULE TSP


