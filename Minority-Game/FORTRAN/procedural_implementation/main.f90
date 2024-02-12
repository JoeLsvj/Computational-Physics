! main
PROGRAM MAIN
use MG
IMPLICIT NONE
INTEGER                             :: state, memory, agents_number, strategy_number, runs, j
REAL, DIMENSION(5)                  :: results, average_results
CHARACTER(LEN = 32)                 :: filename_1


! total action and winning rate statistics
#if 0
strategy_number = 2
runs = 100000

average_results = 0
DO agents_number = 101, 1001, 100
    WRITE(filename_1, "(A13, I5, A4)") "A_statistics_", agents_number, ".txt"
    OPEN(11, file = filename_1, ACTION = "WRITE", STATUS="REPLACE")
    DO memory = 1, 16, 1
        DO j = 1, 5, 1
            CALL MG_SIMULATION(agents_number, memory, strategy_number, runs, results)
            ! we can take the average here writing to file here:
            average_results = average_results + results
        END DO
        average_results = average_results / 5
        WRITE(11, *) average_results(1), average_results(2), average_results(3), average_results(4), average_results(5)
        average_results = 0
    END DO
    CLOSE(11)
END DO
#endif

! Total action array periodicity analysis:
#if 1
agents_number = 1001
strategy_number = 5
runs = 50000

CALL MG_SIMULATION(agents_number, 6, strategy_number, runs, results)
CALL MG_SIMULATION(agents_number, 7, strategy_number, runs, results)
CALL MG_SIMULATION(agents_number, 8, strategy_number, runs, results)
CALL MG_SIMULATION(agents_number, 9, strategy_number, runs, results)
#endif

! tendency histogram
#if 0
agents_number = 1001
strategy_number = 10
runs = 10001
CALL MG_SIMULATION(agents_number, 5, strategy_number, runs, results)
#endif

! strategy behavior:
#if 0
runs = 10000
agents_number = 301
memory = 8
OPEN(15, file = 'strategies_winrate_301_8.txt', ACTION = "WRITE", STATUS="REPLACE")
DO j = 1, 3, 1
    DO strategy_number = 1, 25, 1
        CALL MG_SIMULATION(agents_number, memory, strategy_number, runs, results)
        WRITE(15, *) strategy_number, results(2), results(4)
        PRINT*, strategy_number, j
    END DO
END DO
CLOSE(15)
#endif

! memory winrate behavior
#if 0
runs = 10000
agents_number = 301
strategy_number = 2
OPEN(16, file = 'memory_winrate_301_2.txt', ACTION = "WRITE", STATUS="REPLACE")
DO j = 1, 10, 1
    DO memory = 1, 15, 1
        CALL MG_SIMULATION(agents_number, memory, strategy_number, runs, results)
        WRITE(16, *) memory, results(2), results(4)
        PRINT*, memory, j
    END DO
END DO
CLOSE(16)
#endif

END PROGRAM MAIN