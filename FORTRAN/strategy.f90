MODULE strategy_mod
    USE OMP_LIB
    IMPLICIT NONE
    !PRIVATE
    ! the strategy class:
    TYPE, PUBLIC :: strategy_class
        INTEGER,    PUBLIC                          :: memory
        REAL,       PUBLIC                          :: weight
        INTEGER, DIMENSION(:), ALLOCATABLE, PUBLIC  :: strategy
        INTEGER,    PUBLIC                          :: last_predict
        CONTAINS
            PROCEDURE,  PUBLIC :: init          => strategy_init
            PROCEDURE,  PUBLIC :: predict       => strategy_predict
            PROCEDURE,  PUBLIC :: update_weight => strategy_update_weight
            PROCEDURE,  PUBLIC :: delete        => strategy_delete
    END TYPE strategy_class

    CONTAINS
    SUBROUTINE strategy_init(this, memory, weight)
        ! allocate variables and initialize them:
        CLASS(strategy_class), INTENT (INOUT) :: this
        INTEGER, INTENT (IN)                  :: memory
        REAL, OPTIONAL, INTENT (IN)           :: weight
        REAL                                  :: rnd            ! random number
        INTEGER                               :: i, rnd_int

        this%memory = memory
        IF (PRESENT(weight)) THEN; this%weight = weight; ELSE; this%weight = 0. ; END IF
        IF (ALLOCATED(this%strategy)) THEN
            PRINT*, "Already allocated object: check your code"
            CONTINUE
        ELSE
            ALLOCATE(this%strategy(2**this%memory))
        END IF
        ! ! $OMP PARALLEL DO PRIVATE(rnd, rnd_int)
        DO i = 1, 2**this%memory, 1
            CALL RANDOM_NUMBER(rnd)
            rnd_int = 2*NINT(rnd) - 1
            this%strategy(i) = rnd_int
        END DO
        ! ! $OMP END PARALLEL DO
        RETURN
    END SUBROUTINE strategy_init

    FUNCTION strategy_predict(this, state) RESULT(predict)
        ! predicts the next choice
        CLASS(strategy_class), INTENT (INOUT) :: this
        INTEGER, INTENT (IN)                  :: state
        INTEGER                               :: predict
        this%last_predict = this%strategy(state)
        predict = this%last_predict
        RETURN
    END FUNCTION strategy_predict

    SUBROUTINE strategy_update_weight(this, total_action)
        CLASS(strategy_class), INTENT (INOUT) :: this
        INTEGER, INTENT (IN)                  :: total_action
        this%weight = this%weight - this%last_predict * SIGN(1, total_action)
    END SUBROUTINE strategy_update_weight

    SUBROUTINE strategy_delete(this)
        CLASS(strategy_class), INTENT (INOUT) :: this
        DEALLOCATE(this%strategy)
        this%weight = 0.
    END SUBROUTINE strategy_delete

END MODULE strategy_mod