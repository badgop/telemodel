PROGRAM test_comb

    IMPLICIT NONE

    WRITE(*,*) '***************************************'
    WRITE(*,*)
    WRITE(*,*) 'this is comb  test'

    CALL delay_comb()

    WRITE(*,*)
    WRITE(*,*) '************************************'

CONTAINS


SUBROUTINE delay_comb()
    USE sample
    USE comb

    TYPE (comb_t)                   :: comb
    INTEGER(1), PARAMETER           :: len = 10
    INTEGER(4)                      :: i,cnt

    INTEGER(8), DIMENSION(1:len)    :: inputArr
    INTEGER(8), DIMENSION(1:len+1)  :: outpuArr
    TYPE (sample_t)                 :: input(1)
    TYPE (sample_t)                 :: output(1)

    INTEGER(1)                      :: inputSample, outSample, firstSample

    inputSample = 10
    cnt = 0

    CALL input(1)%Constructor(inputSample)


    ! put  1
    !       ***
    !  1--->*0*
    !       ***
    CALL comb%In(input)
    ! get 0 and  delay have  sample value now is 1
    !       ***
    !   --->*1*--->0
    !       ***
    CALL comb%Out(output)

    !

    ! we puttin same sample again and again
    !       ***
    !   1--->*1*--->1
    !       ***
    ! so y(n) = x(n) - x(n-1) = const - const = 0     !
    DO i=1,len

        CALL comb%In(input)
        CALL comb%Out(output)


        outSample =  output(1)%GetValueInt8()
        WRITE(*,*) 'output ', outSample, 'cnt' , cnt
        IF (outSample == 0) THEN
            cnt = cnt + 1
        END IF
    END DO

    IF (cnt==len) THEN
        WRITE(*,*) 'COMB TEST PASSED'
    ELSE
        WRITE(*,*) 'COMB TEST FAILED'
        STOP 'COMB TEST FAILED'
    END IF
END SUBROUTINE


END PROGRAM