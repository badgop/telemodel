MODULE  delay
    USE base_block_class
    USE sample
    IMPLICIT NONE

    PRIVATE
    TYPE, EXTENDS  (base_block_class_t), PUBLIC   :: delay_t

    TYPE(sample_t) :: stored
    TYPE(sample_t) :: storeIn
    TYPE(sample_t) :: storeOut

    CONTAINS

    PROCEDURE :: In => InDelay
    PROCEDURE :: Out => OutDelay

    END TYPE


CONTAINS

    ! InputSamples(1)  - input sample
    ! InputSamples(2)  - reset signal input
SUBROUTINE InDelay(this, inputSamples)
    CLASS (delay_t), INTENT(INOUT)             :: this
    TYPE(sample_t), INTENT(IN), DIMENSION(1:)  :: InputSamples

    this%storeIn = InputSamples(1)

    IF(size(InputSamples)==2) THEN
        IF (InputSamples(2)%GetBoolean()) this%storeOut = 0
    END IF

END SUBROUTINE

SUBROUTINE OutDelay(this, OutputSamples)
    CLASS (delay_t), INTENT(INOUT)              :: this
    TYPE(sample_t), INTENT(OUT), DIMENSION(1:)  :: OutputSamples

    OutputSamples(1) = this%storeOut
    this%storeOut = this%storeIn


END SUBROUTINE





END MODULE