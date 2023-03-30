MODULE base_block_class
    USE sample
    IMPLICIT NONE

    TYPE, ABSTRACT :: base_block_class_t

    PRIVATE

    CONTAINS

    PROCEDURE :: In
    PROCEDURE :: Out

    END TYPE

    CONTAINS

    SUBROUTINE In(this, inputSamples)
        CLASS (base_block_class_t), INTENT(INOUT)  :: this
        TYPE(sample_t), INTENT(IN), DIMENSION(1:)  :: InputSamples

    END SUBROUTINE


    SUBROUTINE Out(this, OutputSamples)
        CLASS (base_block_class_t), INTENT(INOUT)  :: this
        TYPE(sample_t), INTENT(OUT), DIMENSION(:) :: OutputSamples

    END SUBROUTINE




END MODULE