PROGRAM test_delay

    USE delay
    USE sample
    IMPLICIT NONE

    WRITE(*,*) '***************************************'
    WRITE(*,*)
    WRITE(*,*) 'this is delay  test'




    CALL delay_test()
    CALL delay_test_reset()

    WRITE(*,*)
    WRITE(*,*) '************************************'


CONTAINS


SUBROUTINE delay_test()
    USE delay

    TYPE (delay_t)                  :: d
    INTEGER(1), PARAMETER           :: len = 10
    INTEGER(8)                      :: i,cnt

    INTEGER(8), DIMENSION(1:len)    :: inputArr
    INTEGER(8), DIMENSION(1:len+1)  :: outpuArr
    TYPE (sample_t)                 :: input(1)
    TYPE (sample_t)                 :: output(1)

    INTEGER(8)                      :: inputSample, outSample, firstSample

    inputArr=1

    inputSample = 55
    cnt = 0

    CALL input(1)%Constructor(inputSample)
    CALL d%In(input)
    ! get output of delay , it have to be 0
    CALL d%Out(outPut)
    firstSample = outPut(1)%GetValueInt8()
    WRITE(*,*) 'first sample', firstSample

    DO i=1,len
         ! put a sample at the input of delay
        CALL d%In(input)
        ! get delayed sample
        CALL d%Out(outPut)

        outSample = output(1)%GetValueInt8()

        write (*,*) "d%Out(outPut) ", outSample

        IF (  (firstSample==0).AND.(outSample==inputSample)) THEN
            cnt = cnt + 1
            ! write (*,*) cnt
        ELSE
            STOP 'delay test wrong value!'
        END IF
    END DO

    IF (  CNT == len) THEN
        WRITE (*,*) 'TEST DELAY PASSED', firstSample,outSample
    ELSE
        WRITE (*,*) 'TEST DELAY FAILED', firstSample,outSample
    END IF


END SUBROUTINE



SUBROUTINE delay_test_reset()
    USE delay

    TYPE (delay_t)                  :: d
    INTEGER(1), PARAMETER           :: len = 10
    INTEGER(8)                      :: i,cnt

    INTEGER(8), DIMENSION(1:len)    :: inputArr
    INTEGER(8), DIMENSION(1:len+1)  :: outpuArr
    TYPE (sample_t)                 :: input(2)
    TYPE (sample_t)                 :: output(1)

    INTEGER(8)                      :: inputSample, outSample, firstSample, sampleAfterReset

    inputArr=1

    inputSample = 55
    cnt = 0

    CALL input(1)%Constructor(inputSample)
    CALL input(2)%Constructor(int(0,8))


    CALL d%In(input)
    ! get output of delay , it have to be 0
    CALL d%Out(outPut)
    firstSample = outPut(1)%GetValueInt8()

    CALL d%In(input)
        ! get delayed sample
    CALL d%Out(outPut)

    outSample = output(1)%GetValueInt8()

    CALL input(2)%Constructor(int(1,8))

    CALL d%In(input)
         ! get delayed sample
    CALL d%Out(outPut)

    sampleAfterReset = output(1)%GetValueInt8()


    IF (  (firstSample==0).AND.(outSample==inputSample).AND.(sampleAfterReset==0)) THEN
        WRITE (*,*) 'TEST DELAY REST PASSED', firstSample,outSample,sampleAfterReset
    ELSE
        WRITE (*,*) 'TEST DELAY RESET FAILED', firstSample,outSample,sampleAfterReset
        STOP 'TEST DELAY RESET wrong value!'

    END IF


END SUBROUTINE


END PROGRAM