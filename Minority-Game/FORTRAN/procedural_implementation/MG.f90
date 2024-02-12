MODULE MG

USE OMP_LIB
#define _OMP

CONTAINS

SUBROUTINE COMPUTE_STATE(memory, history, state)
    IMPLICIT NONE
    INTEGER, INTENT (IN)                                :: memory
    INTEGER, INTENT (OUT)                               :: state
    INTEGER                                             :: i, p
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (INOUT)  :: history
    ! transform the history bit sequence {+-1}^m in the corresponding decimal number 
    ! +1 (in order to coincide with the fortran indexing), that is the univoque system's state
    state = 0
    p = memory - 1
    DO i = 1, memory, 1
        IF (history(i) == 1) THEN 
            state = state + 2**p 
        END IF
        p = p - 1
    END DO
    ! states from 1 to 2^m included
    state = state + 1
    RETURN
END SUBROUTINE COMPUTE_STATE

SUBROUTINE MG_DRIVER_1(agents_number, memory, state, scores, strategies, total_action)
    IMPLICIT NONE
    INTEGER, INTENT (IN)                                        :: agents_number, memory
    INTEGER, INTENT (INOUT)                                     :: state, total_action
    REAL, DIMENSION(:, :), ALLOCATABLE, INTENT (INOUT)          :: scores
    INTEGER, DIMENSION(:, :, :), ALLOCATABLE, INTENT (INOUT)    :: strategies
    !INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (INOUT)          :: history
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: actions, max_scores, max_array_i
    INTEGER                                                     :: i, winning_group, j!, number_of_winners
    REAL                                                        :: rnd, payoff
    LOGICAL                                                     :: back

    ! allocate the temporary arrays:
    ALLOCATE(actions(agents_number), max_scores(agents_number))
    ! fin the best strategy for each agent looking at the scores:
    ! optimal only for s = 2
    CALL RANDOM_NUMBER(rnd)
    IF (NINT(rnd) == 1) THEN; back = .TRUE.; ELSE; back = .FALSE.; END IF
    max_scores = MAXLOC(scores, 2, BACK = back)
    ! optimal in general
    !DO i = 1, agents_number, 1
    !    max_array_i = PACK([(j, j=1, SIZE(scores(i,:)))], scores(i,:) == MAXVAL(scores(i,:),1))
    !    CALL RANDOM_NUMBER(rnd)
    !    rnd = (SIZE(max_array_i) - 0.00001) * rnd + 1
    !    max_scores(i) = max_array_i(INT(rnd))
    !    DEALLOCATE(max_array_i)
    !END DO
    actions = 0
    !CALL COMPUTE_STATE(memory, history, state)
    ! compute the action for each agent with the best strategies
    !$OMP PARALLEL DO SHARED(actions, strategies, max_scores, state)
    DO i = 1, agents_number, 1
        actions(i) = strategies(i, max_scores(i), state)
    END DO
    !$OMP END PARALLEL DO
    ! compute the total action, the sign of which is the choice of the majority group
    total_action = SUM(actions)
    ! -SIGN(1, total_action) = -sgn(A(t)) is the winning (minority) group
    winning_group = - SIGN(1, total_action)
    ! select the appropriate payoff function:
    payoff = SIGN(1, total_action)
    !payoff = total_action !/ REAL(agents_number)
    !IF ( (agents_number+total_action)/2 < agents_number/2 ) THEN
    !    number_of_winners = (agents_number+total_action)/2
    !ELSE
    !    number_of_winners = (agents_number-total_action)/2
    !END IF
    !payoff = 10*(agents_number/REAL(number_of_winners) - 2)
    ! update the scores of each strategy for each agent
    scores = scores - payoff * strategies(:, :, state)
    ! we do not have to update the history bits and then compute the state,
    ! we can update the state direclty with the following formula
    state = MODULO(2*state + (winning_group - 1)/2, 2**memory)
    IF (state == 0) THEN
        state = 2**memory
    END IF
    DEALLOCATE(actions, max_scores)

END SUBROUTINE MG_DRIVER_1

SUBROUTINE MG_SIMULATION(agents_number, memory, strategy_number, runs, results)
    IMPLICIT NONE
    INTEGER, INTENT (IN)                        :: agents_number, memory, strategy_number, runs
    INTEGER                                     :: i, j, k, state
    REAL, DIMENSION(:, :), ALLOCATABLE          :: scores
    INTEGER, DIMENSION(:, :, :), ALLOCATABLE    :: strategies
    REAL, DIMENSION(:), ALLOCATABLE             :: total_action_array, winners_array
    INTEGER                                     :: total_action, number_of_winners
    REAL                                        :: rnd, mu_A, sigma_A, mu_W, sigma_W
    !CHARACTER(LEN = 32)                         :: filename_0
    REAL, DIMENSION(5), INTENT (INOUT)          :: results
    !REAL, DIMENSION(:), ALLOCATABLE             :: scores_flatten

    ! initialize randomly the strategies for each agent
    ALLOCATE(strategies(agents_number, strategy_number, 2**memory))
    ! we can use a unique random number and collapse 3 cycles with openMP to associate the integer values in strategies
    !$OMP PARALLEL DO COLLAPSE(3) PRIVATE(rnd) SHARED(strategies)
    DO i = 1, agents_number, 1
        DO j = 1, strategy_number, 1
            DO k = 1, 2**memory, 1
                CALL RANDOM_NUMBER(rnd)
                strategies(i, j, k) = 2 * NINT(rnd) - 1
            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO
    ! initialize the scores to zero
    ALLOCATE(scores(agents_number, strategy_number))
    scores = 0
    ! initialize randomly an initial state (that is the decimal number whose binary 
    ! representation are the bits history of the system)
    CALL RANDOM_NUMBER(rnd)
    state = INT(rnd*(2**memory)) + 1 ! states from 1 to 2^m included, as the strategies indexes
    ! store the total action of each step in an array
    ALLOCATE(total_action_array(runs), winners_array(runs))
    !WRITE(filename_0, "(A8, I1, A1, I4, A4)") "A_t_av_2", memory, "_", agents_number, ".txt"
    !OPEN(10, file = filename_0, STATUS = "REPLACE", ACTION = "WRITE")
    !OPEN(12, file = 'strategies_scores.txt', STATUS = "REPLACE", ACTION = "WRITE")

    DO i = 1, runs, 1
        CALL MG_DRIVER_1(agents_number, memory, state, scores, strategies, total_action)
        ! compute the number of winners
        total_action_array(i) = REAL(total_action)
        IF ( (agents_number+total_action)/2 < agents_number/2 ) THEN
            number_of_winners = (agents_number+total_action)/2
        ELSE
            number_of_winners = (agents_number-total_action)/2
        END IF
        winners_array(i) = number_of_winners
        !WRITE(10, *) i, total_action, (agents_number+total_action)/2, state
        !IF (i == 200 .OR. i == 1000 .OR. i == 5000) THEN
        !    scores_flatten = PACK(scores, .true.) / i
        !    DO j = 1, SIZE(scores_flatten), 1
        !        WRITE(12, *) scores_flatten(j)
        !    END DO
        !END IF
    END DO

    ! compute some statistical momenta with the sample of the simulation
    mu_A = SUM(total_action_array) / runs
    ! since runs is usually >> 1, we can provide an estimation of the sample variance with:
    sigma_A = SUM(total_action_array**2) / runs - mu_A**2
    mu_W = SUM(winners_array) / runs ! this is the win rate
    sigma_W = SUM(winners_array**2) / runs - mu_W**2
    !DO i = 1, runs, 1
    !    winners_array(i) = (winners_array(i) - mu_W)**2
    !END DO
    !sigma_W = SUM(winners_array) / (runs - 1)

    results(1) = 2**memory/REAL(agents_number); results(2) = mu_A/agents_number
    results(3) = sigma_A/agents_number; results(4) = mu_W/agents_number; results(5) = sigma_W/agents_number
    DEALLOCATE(strategies, scores, total_action_array, winners_array)
    !CLOSE(10)
    !CLOSE(12)

END SUBROUTINE MG_SIMULATION

END MODULE MG 