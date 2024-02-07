MODULE MG_mod
    USE OMP_LIB
    USE strategy_mod
    USE agent_mod
    IMPLICIT NONE

    TYPE, PUBLIC :: MG_class
        INTEGER,                                         PUBLIC :: agents_number
        INTEGER,                                         PUBLIC :: runs
        INTEGER,         DIMENSION(:, :), ALLOCATABLE,   PUBLIC :: agents_parameters    ! not necessary
        TYPE(agent_class),  DIMENSION(:), ALLOCATABLE,   PUBLIC :: agents_pool
        INTEGER,            DIMENSION(:), ALLOCATABLE,   PUBLIC :: system_history
        INTEGER,            DIMENSION(:), ALLOCATABLE,   PUBLIC :: total_action_array   ! not necessary but maybe useful
        CONTAINS
            PROCEDURE, PUBLIC :: init       => MG_init
            PROCEDURE, PUBLIC :: run        => MG_run
            PROCEDURE, PUBLIC :: evo_run    => MG_evo_run
            PROCEDURE, PUBLIC :: market_run => MG_market_run
            PROCEDURE, PUBLIC :: delete     => MG_delete
    END TYPE MG_class

    CONTAINS

    ! manage the allocation of all the class members and provides to initialize them when necessary
    SUBROUTINE MG_init(this, agents_number, agents_parameters, runs)
        CLASS(MG_class),                        INTENT (INOUT)  :: this
        INTEGER,                                INTENT (IN)     :: agents_number, runs
        INTEGER, DIMENSION(:, :), ALLOCATABLE,  INTENT (INOUT)  :: agents_parameters
        TYPE(agent_class)                                       :: agent
        INTEGER                                                 :: i, j, index
        INTEGER                                                 :: max_memory, memory
        REAL                                                    :: rnd

        this%agents_number = agents_number; this%runs = runs
        this%agents_parameters = agents_parameters
        ALLOCATE(this%total_action_array(runs))
        ! allocate the agent_pool variable:
        ALLOCATE(this%agents_pool(this%agents_number))
        ! initialize all the agents with the correct parameters, and place them in the pool:
        index = 1
        DO i = 1, SIZE(agents_parameters(:, 1)), 1      ! choose the kind of agents
            DO j = 1, agents_parameters(i, 1), 1        ! number of agents of each kind
                ! initialize an agent with the correct parameters:
                CALL agent%init(memory = agents_parameters(i, 2), strategy_number = agents_parameters(i, 3))
                this%agents_pool(index) = agent
                CALL agent%delete()
                index = index + 1
            END DO
        END DO
        ! Initialize randomly the state of each agent and the history of the whole system
        max_memory = MAXVAL(this%agents_parameters(:, 2))
        ALLOCATE(this%system_history(max_memory + runs)); this%system_history = 0
        DO i = 1, max_memory, 1
            CALL RANDOM_NUMBER(rnd)
            this%system_history(i) = 2*NINT(rnd) - 1
        END DO
        DO i = 1, this%agents_number, 1
            memory = this%agents_pool(i)%memory
            this%agents_pool(i)%state = COMPUTE_STATE(memory, this%system_history, max_memory)
        END DO
        ! insert a validation check for the agent pool array:
        IF (this%agents_number == SIZE(this%agents_pool)) THEN; CONTINUE; ELSE; PRINT*, "Init ERROR"; END IF
        !DO i = 1, this%agents_number, 1
        !    PRINT*, this%agents_pool(i)%memory, this%agents_pool(i)%strategy_number, this%agents_pool(i)%state
        !END DO
    END SUBROUTINE MG_init


    ! The actual simulation algorithm with evolutionary mechanisms:
    SUBROUTINE MG_run(this)
        CLASS(MG_class), INTENT (INOUT)                 :: this
        INTEGER                                         :: i, j
        INTEGER                                         :: action, total_action, max_memory
        
        max_memory = MAXVAL(this%agents_parameters(:, 2))
        DO i = 1, this%runs, 1

            total_action = 0
            ! compute the total action of the system and find the winners of the game
            DO j = 1, this%agents_number, 1
                action = this%agents_pool(j)%action()
                total_action = total_action + action
            END DO
            this%total_action_array(i) = total_action
            ! update the state of each agent, the score and the strategies' weights
            ! once an agent has his own state, corresponding to the system's history, then he is independent
            !$OMP PARALLEL DO SHARED(total_action)
            DO j = 1, this%agents_number, 1
                CALL this%agents_pool(j)%update(total_action)
            END DO
            !$OMP END PARALLEL DO
            ! track the system's history record
            this%system_history(max_memory + i) = -SIGN(1, total_action) ! the winning group

        END DO
    END SUBROUTINE MG_run


    ! The actual simulation algorithm with evolutionary mechanisms:
    SUBROUTINE MG_evo_run(this, results)
        CLASS(MG_class), INTENT (INOUT)                 :: this
        REAL, DIMENSION(:), ALLOCATABLE, INTENT (INOUT) :: results
        INTEGER                                         :: i, j, s, action, total_action
        INTEGER                                         :: max_memory, min_index, max_index
        INTEGER                                         :: memory_temp, strategy_number_temp, state_temp
        REAL, DIMENSION(:), ALLOCATABLE                 :: agents_scores
        REAL                                            :: rnd
        INTEGER                                         :: rnd_int
        
        ALLOCATE(agents_scores(this%agents_number))
        max_memory = MAXVAL(this%agents_parameters(:, 2))
        DO i = 1, this%runs, 1

            total_action = 0
            ! compute the total action of the system and find the winners of the game
            DO j = 1, this%agents_number, 1
                action = this%agents_pool(j)%action()
                total_action = total_action + action
            END DO
            this%total_action_array(i) = total_action
            ! update the state of each agent, the score and the strategies' weights
            ! once an agent has his own state, corresponding to the system's history, then he is independent
            !$OMP PARALLEL DO SHARED(total_action)
            DO j = 1, this%agents_number, 1
                CALL this%agents_pool(j)%update(total_action)
            END DO
            !$OMP END PARALLEL DO
            ! track the system's history record
            this%system_history(max_memory + i) = -SIGN(1, total_action) ! the winning group

            ! we can introduce here one evolutionary mechanism:
            IF (MODULO(i, 10) == 0) THEN
                ! the worst agent is deleted:
                DO j = 1, this%agents_number, 1
                    agents_scores(j) = this%agents_pool(j)%score
                END DO
                min_index = MINLOC(agents_scores, 1)   ! detect the location of the worst agent
                max_index = MAXLOC(agents_scores, 1)   ! detect the location of the best agent
                ! replace the worst agent with the best agent (only the strategies, not the scores)
                CALL this%agents_pool(min_index)%delete()
                this%agents_pool(min_index) = this%agents_pool(max_index)
                this%agents_pool(min_index)%score = 0.
                DO j = 1, this%agents_pool(min_index)%strategy_number, 1
                    this%agents_pool(min_index)%strategy_pool(j)%weight = 0.
                END DO
                ! replace one strategy of the new-born agent with a random one:
                CALL RANDOM_NUMBER(rnd)
                rnd_int = INT((this%agents_pool(min_index)%strategy_number - 0.00001)* rnd + 1)
                CALL this%agents_pool(min_index)%strategy_pool(rnd_int)%delete()
                CALL this%agents_pool(min_index)%strategy_pool(rnd_int)%init(this%agents_pool(min_index)%memory)
            END IF

            !  here another evolutionary mechanism is introducted:
            IF (MODULO(i, 10) == 0) THEN
                DO j = 1, this%agents_number, 1
                    agents_scores(j) = this%agents_pool(j)%score
                END DO
                min_index = MINLOC(agents_scores, 1)   ! detect the location of the worst agent
                ! save the worst agent's parameters
                memory_temp = this%agents_pool(min_index)%memory; 
                strategy_number_temp = this%agents_pool(min_index)%strategy_number
                ! add a new agent with a certain probability to have 1 bit more or less of memory
                CALL RANDOM_NUMBER(rnd)
                IF (rnd < 0.3) THEN
                    ! wipe out the worst agent (only if the memory is > 1)
                    IF (memory_temp > 1) THEN
                        CALL this%agents_pool(min_index)%delete()
                        CALL this%agents_pool(min_index)%init(memory = memory_temp - 1, strategy_number = strategy_number_temp) 
                        ! we need to update the state since it is memory dependent:
                        state_temp = COMPUTE_STATE(memory = memory_temp-1, history = this%system_history, offset = max_memory + i)   
                        this%agents_pool(min_index)%state = state_temp   
                    END IF
                ELSE IF (rnd > 0.7) THEN
                    ! wipe out the worst agent:
                    CALL this%agents_pool(min_index)%delete()
                    CALL this%agents_pool(min_index)%init(memory = memory_temp + 1, strategy_number = strategy_number_temp)
                    ! we need to update the state since it is memory dependent:
                    state_temp = COMPUTE_STATE(memory = memory_temp+1, history = this%system_history, offset = max_memory + i)
                    this%agents_pool(min_index)%state = state_temp
                ELSE 
                    ! wipe out the worst agent:
                    CALL this%agents_pool(min_index)%delete()
                    CALL this%agents_pool(min_index)%init(memory = memory_temp, strategy_number = strategy_number_temp)
                    ! we need to update the state since it is memory dependent:
                    state_temp = COMPUTE_STATE(memory = memory_temp, history = this%system_history, offset = max_memory + i)
                    this%agents_pool(min_index)%state = state_temp
                END IF
            END IF
            ! count the memory average at this time step: (we can do this also for the strategies)
            memory_temp = 0 ! integer divion problem...
            DO j = 1, this%agents_number, 1
                memory_temp = memory_temp + this%agents_pool(j)%memory
            END DO
            results(i) = memory_temp / REAL(this%agents_number)

        END DO
        DEALLOCATE(agents_scores)
    END SUBROUTINE MG_evo_run


    ! The actual simulation algorithm for market mechanism:
    SUBROUTINE MG_market_run(this, Ns, Np, Ns_gain, Np_gain)
        CLASS(MG_class), INTENT (INOUT)                     :: this
        INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (INOUT)  :: Ns, Np
        INTEGER                                             :: i, j, s, action, total_action
        INTEGER                                             :: Ns_active, Np_active!, max_memory
        REAL, INTENT(INOUT)                                 :: Ns_gain, Np_gain                                           
        
        !max_memory = MAXVAL(this%agents_parameters(:, 2))
        DO i = 1, this%runs, 1

            ! introduce the market (grand canonical) mechanism (the number of active Ns may vary with time).
            ! in this case there are two types of agents: Np producers and Ns speculators.
            ! an agent plays in the market only if he has at least one strategy with a score grater then e*t/2**m
            total_action = 0
            Ns_active = 0; Np_active = 0
            DO j = 1, this%agents_number, 1
                this%agents_pool(j)%last_action = 0
                action = 0
                DO s = 1, this%agents_pool(j)%strategy_number, 1
                    !IF (this%agents_pool(j)%strategy_pool(s)%weight > 0.01 * i / 2**this%agents_pool(j)%memory) THEN
                    IF (this%agents_pool(j)%strategy_pool(s)%weight > 0.) THEN
                        action = this%agents_pool(j)%action()       ! this will update the last_action field
                        IF (this%agents_pool(j)%strategy_number > 1) THEN
                            Ns_active = Ns_active + 1
                        ELSE
                            Np_active = Np_active + 1
                        END IF
                        EXIT
                    END IF
                END DO
                total_action = total_action + action
            END DO
            this%total_action_array(i) = total_action
            Ns(i) = Ns_active
            Np(i) = Np_active
            ! update the state of each agent, the score and the strategies' weights
            DO j = 1, this%agents_number, 1
                CALL this%agents_pool(j)%update(total_action, this%agents_number)
            END DO
            ! track the system's history record
            !this%system_history(max_memory + i) = -SIGN(1, total_action) ! the winning group

        END DO
        ! count the total gain speculators:
        Ns_gain = 0; Np_gain = 0
        DO j = 1, this%agents_number, 1
            IF (this%agents_pool(j)%strategy_number > 1) THEN
                Ns_gain = Ns_gain + this%agents_pool(j)%score
            ELSE
                Np_gain = Np_gain + this%agents_pool(j)%score
            END IF
        END DO
        Ns_gain = Ns_gain / this%agents_number / this%runs
        Np_gain = Np_gain / this%agents_number / this%runs
    END SUBROUTINE MG_market_run


    SUBROUTINE MG_delete(this)
        CLASS(MG_class), INTENT (INOUT) :: this
        DEALLOCATE(this%system_history, this%total_action_array)
        DEALLOCATE(this%agents_pool)
    END SUBROUTINE MG_delete


    FUNCTION COMPUTE_STATE(memory, history, offset) RESULT(state)
        IMPLICIT NONE
        INTEGER, INTENT (IN)                                :: memory, offset
        INTEGER, DIMENSION(:), ALLOCATABLE, INTENT (INOUT)  :: history
        INTEGER, DIMENSION(:), ALLOCATABLE                  :: agent_history
        INTEGER                                             :: state
        INTEGER                                             :: i, p
        ! transform the history bit sequence {+-1}^m in the corresponding decimal number 
        ! +1 (in order to coincide with the fortran indexing), that is the univoque system's state
        ALLOCATE(agent_history(memory))
        agent_history = history(offset + 1 - memory : offset)
        state = 0
        p = memory - 1
        DO i = 1, memory, 1
            IF (agent_history(i) == 1) THEN 
                state = state + 2**p 
            END IF
            p = p - 1
        END DO
        ! states from 1 to 2^m included
        state = state + 1
        DEALLOCATE(agent_history)
    END FUNCTION COMPUTE_STATE

END MODULE MG_mod