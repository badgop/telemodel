MODULE integrator
    USE sample
    USE delay

    IMPLICIT NONE

    PRIVATE
    TYPE, PUBLIC   :: integrator_t

    TYPE(delay_t)  :: delay
    TYPE(sample_t) :: internalWires(2)
    TYPE(sample_t)  :: feedbackSample(1)
    TYPE(sample_t) :: sampleIn(2)


    CONTAINS
    PROCEDURE :: In => InIntegrator
    PROCEDURE :: Out => OutIntegrator

    END TYPE

CONTAINS

    ! InputSamples(1)  - input sample
    ! InputSamples(2)  - reset signal input
SUBROUTINE InIntegrator(this, inputSamples)
    CLASS (integrator_t), INTENT(INOUT) :: this
    TYPE(sample_t), INTENT(IN), DIMENSION(1:)  :: InputSamples

    this%sampleIn(1) = InputSamples(1) + this%feedbackSample(1)
    IF(size(InputSamples)==2) THEN
        IF (InputSamples(2)%GetBoolean()) this%feedbackSample(1) = 0
    END IF

END SUBROUTINE

SUBROUTINE OutIntegrator(this, OutputSamples)
    CLASS (integrator_t), INTENT(INOUT) :: this
    TYPE(sample_t), INTENT(OUT), DIMENSION(1:)  :: OutputSamples
    TYPE(sample_t) :: outputDelay(1)
    TYPE(sample_t), DIMENSION(1:2)  :: tmp

    OutputSamples(1) = this%sampleIn(1)

    this%feedbackSample(1) = this%sampleIn(1)

END SUBROUTINE



END MODULE

