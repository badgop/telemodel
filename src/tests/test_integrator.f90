PROGRAM test_integrator



    IMPLICIT NONE

    WRITE(*,*) '***************************************'
    WRITE(*,*)
    WRITE(*,*) 'this is integrator  test'


    CALL delay_integrator()
    CALL delay_integrator_reset()

    WRITE(*,*)
    WRITE(*,*) '************************************'


CONTAINS

SUBROUTINE delay_integrator()
    USE sample
    USE integrator

    TYPE (integrator_t)             :: integrator
    INTEGER(1), PARAMETER           :: len = 10
    INTEGER(1)                      :: i,cnt
    TYPE (sample_t)                 :: input(2)
    TYPE (sample_t)                 :: output(1)

    INTEGER(8)                      :: inputSample, outSample
    inputSample = 1
    cnt = 1

    CALL input(1)%Constructor(inputSample)



    ! we puttin same sample again and again
    ! so y(n) = x(n) + y(n-1)
    !
    DO i=1,len
        ! CALL integrator%In(input)
        CALL integrator%In(input)
        CALL integrator%Out(output)

        outSample =  output(1)%GetValueInt8()
        WRITE(*,*) 'output', outSample, 'cnt ', i

    END DO

    IF (outSample ==len) THEN
        WRITE(*,*) 'integrator TEST PASSED'
    ELSE
        WRITE(*,*) 'integrator TEST FAILED'
    END IF

END SUBROUTINE

SUBROUTINE delay_integrator_reset()
    USE sample
    USE integrator

    TYPE (integrator_t)             :: integrator
    INTEGER(1), PARAMETER           :: len = 10
    INTEGER(1)                      :: i,cnt

    INTEGER(8), DIMENSION(1:len)    :: inputArr
    INTEGER(8), DIMENSION(1:len+1)  :: outpuArr
    TYPE (sample_t)                 :: input(2)
    TYPE (sample_t)                 :: output(1)

    INTEGER(8)                      :: inputSample, firstSample, secondSample

    inputSample = 1
    cnt = 1

    CALL input(1)%Constructor(inputSample)
    CALL input(2)%Constructor(0)

    CALL integrator%In(input)
    CALL integrator%Out(output)

    firstSample = output(1)%GetValueInt8()

    CALL input(2)%Constructor(1)

    CALL integrator%In(input)
    CALL integrator%Out(output)

    secondSample = output(1)%GetValueInt8()

    IF (firstSample == secondSample) THEN
        WRITE(*,*) 'integrator reset TEST PASSED'
    ELSE
        WRITE(*,*) 'integrator reset TEST FAILED'
    END IF

END SUBROUTINE


END PROGRAM