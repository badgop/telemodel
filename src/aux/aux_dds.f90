MODULE aux_dds

    IMPLICIT NONE
    PUBLIC

CONTAINS

FUNCTION GetFreqCode (samplingFrequency, requiredFrequency,romSizeInBits) RESULT(code)
    INTEGER (8), INTENT(IN) :: samplingFrequency
    INTEGER (8), INTENT(IN) :: requiredFrequency
    INTEGER (8), INTENT(IN) :: romSizeInBits
    REAL(8)                 :: code_frac
    INTEGER (8)             :: code

    code_frac =  real(requiredFrequency)/ (real(samplingFrequency)/real(int(2,8)**int(romSizeInBits,8)))
    code = int(code_frac,8)
END FUNCTION


FUNCTION GetPhaseCode (inputPhaseInDegrees, romSizeInBits) RESULT(code)
    REAL (8), INTENT(IN) :: inputPhaseInDegrees
    INTEGER (8), INTENT(IN) :: romSizeInBits
    REAL(8)                 :: phaseStep
    INTEGER (8)             :: code

    phaseStep = 360.0/real(int(2,8)**int(romSizeInBits,8))
    code = int((inputPhaseInDegrees/phaseStep),8)
END FUNCTION

END MODULE