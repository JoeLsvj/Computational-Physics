MODULE agent_mod
    USE OMP_LIB
    USE strategy_mod
    IMPLICIT NONE

    TYPE, PUBLIC :: agent_class
        INTEGER,                                            PUBLIC :: memory
        INTEGER,                                            PUBLIC :: strategy_number
        INTEGER,                                            PUBLIC :: state
        REAL,                                               PUBLIC :: score
        INTEGER,                                            PUBLIC :: last_action
        TYPE (strategy_class), DIMENSION(:), ALLOCATABLE,   PUBLIC :: strategy_pool
        CONTAINS
            PROCEDURE, PUBLIC :: init   => agent_init
            PROCEDURE, PUBLIC :: action => agent_action
            PROCEDURE, PUBLIC :: update => agent_update
            PROCEDURE, PUBLIC :: delete => agent_delete
            PROCEDURE, PUBLIC :: print  => agent_print
    END TYPE agent_class

    CONTAINS

    SUBROUTINE agent_init(this, memory, strategy_number, score)
        CLASS(agent_class), INTENT (INOUT)  :: this
        INTEGER, INTENT (IN)                :: memory
        INTEGER, INTENT (IN), OPTIONAL      :: strategy_number
        REAL,    INTENT (IN), OPTIONAL      :: score
        TYPE(strategy_class)                :: strategy
        INTEGER                             :: i

        this%memory = memory
        IF (PRESENT(strategy_number)) THEN; this%strategy_number = strategy_number; ELSE; this%strategy_number = 2 ; END IF
        IF (PRESENT(score)) THEN; this%score = score; ELSE; this%score = 0. ; END IF
        ALLOCATE(this%strategy_pool(this%strategy_number))
        DO i = 1, this%strategy_number, 1
            CALL strategy%init(memory = this%memory)
            ! this is a deep copy of filds in the structure (type) not a shallow copy of the memory region
            this%strategy_pool(i) = strategy
            CALL strategy%delete()
        END DO
        !DO i = 1, this%strategy_number, 1
        !    PRINT*, this%strategy_pool(i)%strategy
        !END DO
    END SUBROUTINE agent_init

    FUNCTION agent_action(this) RESULT(action)
        CLASS(agent_class), INTENT (INOUT)      :: this
        INTEGER                                 :: action
        INTEGER                                 :: i, max_index
        REAL,       DIMENSION(:), ALLOCATABLE   :: weights_array
        INTEGER,    DIMENSION(:), ALLOCATABLE   :: max_indices
        REAL                                    :: rnd
        LOGICAL                                 :: back

        ALLOCATE(weights_array(this%strategy_number))
        ! choose the best strategy in the pool:
        DO i = 1, this%strategy_number, 1
            weights_array(i) = this%strategy_pool(i)%weight
        END DO
        ! general procedure for random choice
        !max_indices = PACK([(i, i=1, SIZE(weights_array))], weights_array == MAXVAL(weights_array,1))
        !CALL RANDOM_NUMBER(rnd)
        !rnd = (SIZE(max_indices) - 0.00001) * rnd + 1
        !max_index = max_indices(INT(rnd))
        !DEALLOCATE(max_indices)
        ! best procedure for s = 2
        CALL RANDOM_NUMBER(rnd)
        IF (NINT(rnd) == 1) THEN; back = .TRUE.; ELSE; back = .FALSE.; END IF
        max_index = MAXLOC(weights_array, 1, BACK = back)   !-> this individuates the best strategy
        DEALLOCATE(weights_array)                           ! deallocate the weights_array that is no more useful

        this%last_action = this%strategy_pool(max_index)%predict(this%state)
        action = this%last_action
        RETURN
    END FUNCTION agent_action

    SUBROUTINE agent_update(this, total_action, N)
        CLASS(agent_class), INTENT (INOUT)  :: this
        INTEGER,            INTENT (IN)     :: total_action
        INTEGER                             :: i, predict
        INTEGER, OPTIONAL,  INTENT (IN)     :: N            ! the number of total agents in a possible pool

        ! update agent's score (or economical gain throught trading)
        IF (PRESENT(N)) THEN
            this%score = this%score - this%last_action * total_action
        ELSE
            this%score = this%score - this%last_action * SIGN(1, total_action)
        END IF
        ! update all the strategies' weight in the agent's pool (with the old state)
        DO i = 1, this%strategy_number, 1
            predict = this%strategy_pool(i)%predict(this%state)
            CALL this%strategy_pool(i)%update_weight(total_action)
        END DO
        ! finally, update the state of the agent, depending on the total action of all agents in the pool
        ! -SIGN(1, total_action) is the winning group
        this%state = MODULO(2*this%state + (-SIGN(1, total_action) - 1)/2, 2**this%memory)
        IF (this%state == 0) THEN
            this%state = 2**this%memory
        END IF
    END SUBROUTINE agent_update

    SUBROUTINE agent_print(this)
        CLASS(agent_class), INTENT (INOUT) :: this
        PRINT*, this%memory, this%strategy_number, this%state
    END SUBROUTINE agent_print

    SUBROUTINE agent_delete(this)
        CLASS(agent_class), INTENT (INOUT) :: this
        this%score  = 0.
        DEALLOCATE(this%strategy_pool)
    END SUBROUTINE agent_delete

END MODULE agent_mod