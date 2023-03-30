MODULE comb
    USE base_block_class
    USE sample
    USE delay

    IMPLICIT NONE

    PRIVATE
    TYPE, EXTENDS  (base_block_class_t), PUBLIC   :: comb_t

    TYPE(delay_t)  :: delay
    TYPE(sample_t) :: sampleIn

    CONTAINS
    PROCEDURE :: In => InComb
    PROCEDURE :: Out => OutComb
    END TYPE

CONTAINS

SUBROUTINE InComb (this, inputSamples)
    CLASS (comb_t), INTENT(INOUT) :: this
    TYPE(sample_t), INTENT(IN), DIMENSION(1:)  :: InputSamples

    this%sampleIn = InputSamples(1)
    CALL this%delay%In( InputSamples)


END SUBROUTINE

SUBROUTINE OutComb(this, OutputSamples)
    CLASS (comb_t), INTENT(INOUT) :: this
    TYPE(sample_t), INTENT(OUT), DIMENSION(1:)  :: OutputSamples
    TYPE(sample_t) :: outputDelay(1)

    CALL this%delay%Out(outputDelay)

    OutputSamples(1) = this%sampleIn - outputDelay(1)

END SUBROUTINE

END MODULE

