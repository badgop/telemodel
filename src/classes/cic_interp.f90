MODULE cic_interp
    USE base_block_class
    USE sample
    USE comb
    USE integrator
    IMPLICIT NONE

    TYPE, EXTENDS(base_block_class_t), PUBLIC :: cic_interp_t

        TYPE(comb_t), DIMENSION       (:), ALLOCATABLE     :: combs
        TYPE(integrator_t), DIMENSION (:), ALLOCATABLE     :: ints
        TYPE(sample_t),  DIMENSION(1)                      :: sectionOut
        INTEGER(1) , PRIVATE                               :: order
        INTEGER(2) , PRIVATE                               :: interpKoeff
        INTEGER(2) , PRIVATE                               :: cnt
        ! this is the interpolation coeff
        INTEGER(1) , PRIVATE                               :: r


    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: In => InInterp
        PROCEDURE :: Out => OutInterp
        FINAL     :: Destructor

    END TYPE

    CONTAINS

    SUBROUTINE Constructor(this,order, interpKoeff )
        CLASS (cic_interp_t), INTENT(INOUT) :: this
        INTEGER(1) , INTENT(IN)             :: order
        INTEGER(2) , INTENT(IN)             :: interpKoeff

        this%order = order
        this%interpKoeff = interpKoeff
        this%cnt = 1

        ALLOCATE (this%combs(1:order))
        ALLOCATE (this%ints(1:order))

    END SUBROUTINE


    SUBROUTINE InInterp(this, inputSamples)
        CLASS (cic_interp_t), INTENT(INOUT) :: this
        TYPE(sample_t), INTENT(IN), DIMENSION(1:)  :: InputSamples
        TYPE(sample_t),  DIMENSION(1:2)  :: outSample
        INTEGER(1)                       :: i

        CALL this%combs(1)%In(InputSamples)
        CALL this%combs(1)%Out(outSample)

        DO i = 2, this%order
            CALL this%combs(i)%In(outSample)
            CALL this%combs(i)%Out(outSample)
        END DO

        this%sectionOut(1) =  outSample(1)



    END SUBROUTINE


    SUBROUTINE OutInterp(this, OutputSamples)
        CLASS (cic_interp_t), INTENT(INOUT) :: this
        TYPE(sample_t), INTENT(OUT), DIMENSION(1:) :: OutputSamples
        TYPE(sample_t),  DIMENSION(1:2)  :: outSample
        INTEGER(1)                       :: i
        TYPE(sample_t),  DIMENSION(1:2)  :: zero



        IF (this%cnt == this%interpKoeff) THEN
            CALL this%ints(1)%In(this%sectionOut)
            CALL this%ints(1)%Out(outSample)

            DO i = 2, this%order
                CALL this%ints(i)%In(outSample)
                CALL this%ints(i)%Out(outSample)
            END DO

            this%cnt = 1
        ELSE
            CALL zero(1)%Constructor(0)
            CALL this%ints(1)%In(zero)
            CALL this%ints(1)%Out(outSample)

            DO i = 2, this%order
                CALL this%ints(i)%In(outSample)
                CALL this%ints(i)%Out(outSample)
            END DO
            this%cnt = this%cnt +1
        END IF



        OutputSamples(1) = outSample(1)

    END SUBROUTINE

    SUBROUTINE Destructor(this)
        TYPE (cic_interp_t), INTENT(INOUT) :: this
        IF (ALLOCATED(this%combs)) DEALLOCATE (this%combs)
        IF (ALLOCATED(this%ints)) DEALLOCATE (this%ints)

    END SUBROUTINE

END MODULE