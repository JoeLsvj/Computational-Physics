! main
PROGRAM MAIN

USE strategy_mod; USE agent_mod; USE MG_mod
IMPLICIT NONE

INTEGER, DIMENSION(:, :), ALLOCATABLE   :: agents_parameters
INTEGER, DIMENSION(:),    ALLOCATABLE   :: seed, Ns, Np

TYPE(MG_class)                          :: MG_sim_1, MG_sim_2

INTEGER                         :: i, runs, sizer, clock
REAL, DIMENSION(:), ALLOCATABLE :: results
REAL                            :: Ns_gain, Np_gain

! random number generation
CALL RANDOM_SEED(sizer)
ALLOCATE(seed(sizer))
CALL SYSTEM_CLOCK(COUNT=clock)
seed = clock + 37 * (/ (i - 1, i = 1, sizer) /)
CALL RANDOM_SEED(PUT = seed)

! evolutionary mechanisms simulations
#if 0
ALLOCATE(agents_parameters(1,3))
agents_parameters = reshape( (/ 1001, 2, 2 /), &
                                shape(agents_parameters), order=(/2,1/) )

runs = 100000
ALLOCATE(results(runs))
CALL MG_sim_1%init(agents_number = 1001, agents_parameters = agents_parameters, runs = runs)
CALL MG_sim_1%evo_run(results)
OPEN(10, file = "evo_test_1001.txt", ACTION = "WRITE", STATUS="REPLACE")
OPEN(11, file = "evo_memory_1001.txt", ACTION = "WRITE", STATUS="REPLACE")
DO i = 1, runs, 1
    WRITE(10, *) i, MG_sim_1%total_action_array(i)
    WRITE(11, *) i, results(i)
END DO
CALL MG_sim_1%delete()
CLOSE(10); CLOSE(11)
DEALLOCATE(results)
DEALLOCATE(agents_parameters)
#endif


! market mechanism total_action simulation
#if 0
ALLOCATE(agents_parameters(2,3))
agents_parameters = reshape( (/ 501, 5, 2, &
                                1001, 5, 1/), &
                                shape(agents_parameters), order=(/2,1/) )
runs = 50000
ALLOCATE(Np(runs), Ns(runs))
CALL MG_sim_2%init(agents_number = 1502, agents_parameters = agents_parameters, runs = runs)
CALL MG_sim_2%market_run(Ns, Np, Ns_gain, Np_gain)
OPEN(12, file = "market_A_T_1001_501_5.txt", ACTION = "WRITE", STATUS="REPLACE")
OPEN(13, file = "market_Nsp_1001_501_5.txt", ACTION = "WRITE", STATUS="REPLACE")
DO i = 1, runs, 1
    WRITE(12, *) i, MG_sim_2%total_action_array(i)
    WRITE(13, *) i, Ns(i), Np(i)
END DO
CALL MG_sim_2%delete()
CLOSE(12); CLOSE(13)
DEALLOCATE(Ns, Np)
DEALLOCATE(agents_parameters)
#endif

! market mechanism gains simulations
#if 0
ALLOCATE(agents_parameters(2,3))
agents_parameters = reshape( (/ 0, 6, 2, &
                                256, 6, 1/), &
                                shape(agents_parameters), order=(/2,1/) )

OPEN(14, file = "Nsp_gain_256p.txt", ACTION = "WRITE", STATUS="REPLACE")

runs = 50000
ALLOCATE(Np(runs), Ns(runs))
DO i = 2**5, 640, 2**5
    print*, i
    agents_parameters(1, 1) = i
    CALL MG_sim_2%init(agents_number = 256 + i, agents_parameters = agents_parameters, runs = runs)
    CALL MG_sim_2%market_run(Ns, Np, Ns_gain, Np_gain)
    CALL MG_sim_2%delete()
    WRITE(14, *) i / REAL(2**6), Ns_gain, Np_gain
END DO
CLOSE(14)
DEALLOCATE(Np, Ns)
DEALLOCATE(agents_parameters)
#endif

! mixed population simulation
#if 0
ALLOCATE(agents_parameters(10,3))
agents_parameters = reshape( (/ 100, 1, 5, &
                                100, 2, 5, &
                                100, 3, 5, &
                                100, 4, 5, &
                                100, 5, 5, &
                                100, 6, 5, &
                                100, 7, 5, &
                                100, 8, 5, &
                                100, 9, 5, &
                                101, 10,5    /), &
                                shape(agents_parameters), order=(/2,1/) )

runs = 50000
ALLOCATE(results(runs))
CALL MG_sim_1%init(agents_number = 1001, agents_parameters = agents_parameters, runs = runs)
CALL MG_sim_1%run(results)
OPEN(15, file = "memory_scores_mixed.txt", ACTION = "WRITE", STATUS="REPLACE")
DO i = 1, MG_sim_1%agents_number, 1
    WRITE(15, *) MG_sim_1%agents_pool(i)%memory, (MG_sim_1%agents_pool(i)%score + runs) / (2*runs)
END DO
CALL MG_sim_1%delete()
CLOSE(15)
DEALLOCATE(results)
DEALLOCATE(agents_parameters)
#endif


END PROGRAM MAIN