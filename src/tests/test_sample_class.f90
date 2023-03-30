PROGRAM test_sample_class

    IMPLICIT NONE

    INTEGER(1) :: passedTestsGroupCount = 0
    WRITE(*,*) '***************************************'
    WRITE(*,*)
    WRITE(*,*) 'this is sample class  test'
    WRITE(*,*)

    CALL test_constructors()

    CALL test_operators()

    WRITE(*,*) '***************************************'



CONTAINS

    SUBROUTINE test_constructors()
        USE sample

        TYPE (sample_t)       :: s
        INTEGER(8)            :: valueInt  = 10.0
        INTEGER(8)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.00000001
        REAL(8)   , PARAMETER :: valueReal = 0.100004
        REAL(8)   , PARAMETER :: valueReal4 = 0.1560
        LOGICAL               :: res

        res = TestConstructorINT1()
        res = TestConstructorINT2()
        res = TestConstructorINT4()
        res = TestConstructorINT8()
        res = TestConstructorReal4()
        res = TestConstructorReal8()
        res = TestChangeValueType()

    END SUBROUTINE test_constructors


    SUBROUTINE test_operators()

        USE sample

        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(8)            :: valueInt  = 10.0
        INTEGER(8)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560
        LOGICAL               :: res

        res = TestAssignIntToInt()
        res = TestAssignRealToReal()
        res = TestOperatorAddInt()
        res = TestOperatorAddReal()
        res = TestOperatorMultInt()
        res = TestOperatorMultReal()
        res = TestOperatorSubInt()
        res = TestOperatorSubReal()
        res = TestAssignIntToInt1()
        res = TestAssignIntToInt2()
        res = TestAssignIntToInt4()
        res = TestAssignIntToInt8()
        res = TestShiftInt1()
        res = TestShiftInt2()
        res = TestShiftInt4()
        res = TestShiftInt8()

    END SUBROUTINE test_operators


    FUNCTION TestConstructorINT1 () RESULT (res)
        USE sample
        CHARACTER(50)               :: fmt
        LOGICAL ::  res
        TYPE (sample_t)             :: s
        INTEGER(1), PARAMETER       :: kindInt  =   1
        INTEGER(kindInt)            :: valueInt =   10.0
        INTEGER(8)                  :: ret      =   0
        CHARACTER(4)                :: oneTab   = '    '

        fmt = '(A,I1,A,A,A,I2,A,I2,A,L)'

        CALL s%Constructor(int(valueInt,1))
        ret = s%GetValueInt8()
        IF ((ret == valueInt).AND.(.NOT.s%IsAReal())) THEN
            WRITE (*,fmt) 'ConstructorInt',kindInt,oneTab,'TEST PASSED',oneTab,valueInt,oneTab,ret,oneTab,s%IsAReal()
            res = .TRUE.
        ELSE
            WRITE (*,fmt) 'ConstructorInt',kindInt,oneTab,'TEST FAILED',oneTab,valueInt,oneTab,ret,oneTab,s%IsAReal()
            res = .FALSE.
        END IF

    END FUNCTION


    FUNCTION TestConstructorINT2 () RESULT (res)
        USE sample
        CHARACTER(50)               :: fmt
        LOGICAL ::  res
        TYPE (sample_t)             :: s
        INTEGER(1), PARAMETER       :: kindInt  =   2
        INTEGER(kindInt)            :: valueInt =   10.0
        INTEGER(8)                  :: ret      =   0
        CHARACTER(4)                :: oneTab   = '    '

        fmt = '(A,I1,A,A,A,I2,A,I2,A,L)'

        CALL s%Constructor(int(valueInt,1))
        ret = s%GetValueInt8()
        IF ((ret == valueInt).AND.(.NOT.s%IsAReal())) THEN
            WRITE (*,fmt) 'ConstructorInt',kindInt,oneTab,'TEST PASSED',oneTab,valueInt,oneTab,ret,oneTab,s%IsAReal()
            res = .TRUE.
        ELSE
            WRITE (*,fmt) 'ConstructorInt',kindInt,oneTab,'TEST FAILED',oneTab,valueInt,oneTab,ret,oneTab,s%IsAReal()
            res = .FALSE.
        END IF
    END FUNCTION


    FUNCTION TestConstructorINT4 () RESULT (res)
        USE sample
        CHARACTER(50)               :: fmt
        LOGICAL ::  res
        TYPE (sample_t)             :: s
        INTEGER(1), PARAMETER       :: kindInt  =   4
        INTEGER(kindInt)            :: valueInt =   10.0
        INTEGER(8)                  :: ret      =   0
        CHARACTER(4)                :: oneTab   = '    '

        fmt = '(A,I1,A,A,A,I2,A,I2,A,L)'

        CALL s%Constructor(int(valueInt,1))
        ret = s%GetValueInt8()
        IF ((ret == valueInt).AND.(.NOT.s%IsAReal())) THEN
            WRITE (*,fmt) 'ConstructorInt',kindInt,oneTab,'TEST PASSED',oneTab,valueInt,oneTab,ret,oneTab,s%IsAReal()
            res = .TRUE.
        ELSE
            WRITE (*,fmt) 'ConstructorInt',kindInt,oneTab,'TEST FAILED',oneTab,valueInt,oneTab,ret,oneTab,s%IsAReal()
            res = .FALSE.
        END IF
    END FUNCTION


    FUNCTION TestConstructorINT8 () RESULT (res)
        USE sample
        CHARACTER(50)               :: fmt
        LOGICAL ::  res
        TYPE (sample_t)             :: s
        INTEGER(1), PARAMETER       :: kindInt  =   8
        INTEGER(kindInt)            :: valueInt =   10.0
        INTEGER(8)                  :: ret      =   0
        CHARACTER(4)                :: oneTab   = '    '

        fmt = '(A,I1,A,A,A,I2,A,I2,A,L)'

        CALL s%Constructor(int(valueInt,1))
        ret = s%GetValueInt8()
        IF ((ret == valueInt).AND.(.NOT.s%IsAReal())) THEN
            WRITE (*,fmt) 'ConstructorInt',kindInt,oneTab,'TEST PASSED',oneTab,valueInt,oneTab,ret,oneTab,s%IsAReal()
            res = .TRUE.
        ELSE
            WRITE (*,fmt) 'ConstructorInt',kindInt,oneTab,'TEST FAILED',oneTab,valueInt,oneTab,ret,oneTab,s%IsAReal()
            res = .FALSE.
        END IF
    END FUNCTION


    FUNCTION TestConstructorReal4 () RESULT (res)
        USE sample
        CHARACTER(50)               :: fmt
        LOGICAL ::  res
        TYPE (sample_t)             :: s
        INTEGER(1), PARAMETER       :: kindInt  =   4
        CHARACTER(4)                :: oneTab   = '    '

        REAL(8)                     :: retReal
        REAL(8)         , PARAMETER :: eps = 0.00000001
        REAL(kindInt)   , PARAMETER :: valueReal = 0.100004

        fmt = '(A,I1,A,A,A,G,A,G,A,L)'

        CALL s%Constructor(valueReal)
        retReal = s%GetValueReal()
        IF ( (abs(retReal - valueReal)<eps).AND.(s%IsAReal()) ) THEN
            WRITE (*,fmt) 'ConstructorREAL',kindInt,oneTab,'TEST PASSED',oneTab,valueReal,oneTab,retReal,oneTab,s%IsAReal()
        ELSE
            WRITE (*,fmt) 'ConstructorREAL',kindInt,oneTab,'TEST FAILED',oneTab,valueReal,oneTab,retReal,oneTab,s%IsAReal()
        END IF


    END FUNCTION


    FUNCTION TestConstructorReal8 () RESULT (res)
        USE sample
        CHARACTER(50)               :: fmt
        LOGICAL ::  res
        TYPE (sample_t)             :: s
        INTEGER(1), PARAMETER       :: kindInt  =   8
        CHARACTER(4)                :: oneTab   = '    '

        REAL(8)                     :: retReal
        REAL(8)         , PARAMETER :: eps = 0.00000001
        REAL(kindInt)   , PARAMETER :: valueReal = 0.100004

        fmt = '(A,I1,A,A,A,G,A,G,A,L)'

        CALL s%Constructor(valueReal)
        retReal = s%GetValueReal()
        IF ( (abs(retReal - valueReal)<eps).AND.(s%IsAReal()) ) THEN
            WRITE (*,fmt) 'ConstructorREAL',kindInt,oneTab,'TEST PASSED',oneTab,valueReal,oneTab,retReal,oneTab,s%IsAReal()
        ELSE
            WRITE (*,fmt) 'ConstructorREAL',kindInt,oneTab,'TEST FAILED',oneTab,valueReal,oneTab,retReal,oneTab,s%IsAReal()
            STOP
        END IF


    END FUNCTION


    FUNCTION TestChangeValueType () RESULT (res)
        USE sample
        CHARACTER(50)               :: fmt
        LOGICAL ::  res
        TYPE (sample_t)             :: s
        CHARACTER(4)                :: oneTab   = '    '

        INTEGER(1), PARAMETER       :: kindInt  =   8
        INTEGER(kindInt)            :: valueInt =   10.0
        INTEGER(8)                  :: ret      =   0

        REAL(8)                     :: retReal
        REAL(8)         , PARAMETER :: eps = 0.00000001
        REAL(kindInt)   , PARAMETER :: valueReal = 0.100004

        LOGICAL ::  initedByInt, changedToReal, changedToIntAgain

        initedByInt         = .FALSE.
        changedToReal       = .FALSE.
        changedToIntAgain   = .FALSE.

        CALL s%Constructor(valueInt)
        ret = s%GetValueInt8()

        IF ((ret == valueInt).AND.(.NOT.s%IsAReal())) THEN
            initedByInt = .TRUE.
        END IF

        CALL s%Constructor(valueReal)
        retReal = s%GetValueReal()

        IF ( (abs(retReal - valueReal)<eps).AND.(s%IsAReal()) ) THEN
            changedToReal = .TRUE.
        END IF

        CALL s%Constructor(valueInt)
        ret = s%GetValueInt8()
        IF ((ret == valueInt).AND.(.NOT.s%IsAReal())) THEN
            changedToIntAgain = .TRUE.
        END IF

        fmt = '(A,A,L,A,L,A,L)'
        IF (initedByInt.AND.changedToReal.AND.changedToIntAgain) THEN
            WRITE(*,fmt) 'CHANGE TYPE TEST PASSED',oneTab,initedByInt,oneTab,changedToReal,oneTab,changedToIntAgain
        ELSE
            WRITE(*,fmt) 'CHANGE TYPE TEST FAILED',oneTab,initedByInt,oneTab,changedToReal,oneTab,changedToIntAgain
        END IF
    END FUNCTION

    FUNCTION TestAssignIntToInt () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1)            :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560

        CALL xOp%Constructor(valueInt)
        CALL yOp%Constructor(valueInt)

        fmt = '(A,A,I2,A,L)'

        rOp = xOp
        ret = rOp%GetValueInt8()
        IF (ret == valueInt) THEN
            WRITE(*,*) 'AssignSampleToSample int=int1 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        ELSE
            WRITE(*,*) 'AssignSampleToSample int=int1 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        END IF
    END FUNCTION

    FUNCTION TestAssignIntToInt1 () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1), PARAMETER :: kindInt  = 1
        INTEGER(kindInt)      :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560

        CALL xOp%Constructor(valueInt)

        fmt = '(A,A,I2,A,L)'

        rOp = valueInt
        ret = rOp%GetValueInt8()
        IF (ret == valueInt) THEN
            WRITE(*,*) 'TestAssignIntToInt1 int=int2 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        ELSE
            WRITE(*,*) 'TestAssignIntToInt1 int=int2 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        END IF
    END FUNCTION

    FUNCTION TestAssignIntToInt2 () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1), PARAMETER :: kindInt  = 2
        INTEGER(kindInt)      :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560

        CALL xOp%Constructor(valueInt)

        fmt = '(A,A,I2,A,L)'

        rOp = valueInt
        ret = rOp%GetValueInt8()
        IF (ret == valueInt) THEN
            WRITE(*,*) 'TestAssignIntToInt2 int=int4 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        ELSE
            WRITE(*,*) 'TestAssignIntToInt2 int=int4 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        END IF
    END FUNCTION


    FUNCTION TestAssignIntToInt4 () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1), PARAMETER :: kindInt  = 4
        INTEGER(kindInt)      :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560

        CALL xOp%Constructor(valueInt)

        fmt = '(A,A,I2,A,L)'

        rOp = valueInt
        ret = rOp%GetValueInt8()
        IF (ret == valueInt) THEN
            WRITE(*,*) 'TestAssignIntToInt4 int=int5 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        ELSE
            WRITE(*,*) 'TestAssignIntToInt4 int=int5 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        END IF
    END FUNCTION



    FUNCTION TestAssignIntToInt8 () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1), PARAMETER :: kindInt  = 8
        INTEGER(kindInt)      :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560

        CALL xOp%Constructor(valueInt)

        fmt = '(A,A,I2,A,L)'

        rOp = valueInt
        ret = rOp%GetValueInt8()
        IF (ret == valueInt) THEN
            WRITE(*,*) 'TestAssignIntToInt8 int=int TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        ELSE
            WRITE(*,*) 'TestAssignIntToInt8 int=int TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        END IF
    END FUNCTION

    FUNCTION TestShiftInt1 () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1), PARAMETER :: kindInt  = 1
        INTEGER(kindInt)      :: shiftInt
        INTEGER(kindInt)      :: valueInt  = 4
        INTEGER(1)            :: ret = 0


        CALL xOp%Constructor(valueInt)

        fmt = '(A,A,I2,A,L)'

        rOp = valueInt
        shiftInt = 1
        yOp = rOp.SHIFTR.shiftInt
        ret = yOp%GetValueInt8()

        IF (ret == SHIFTA(valueInt,shiftInt)) THEN
            WRITE(*,*) 'TestShiftInt1 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        ELSE
            WRITE(*,*) 'TestShiftInt1 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        END IF
    END FUNCTION


    FUNCTION TestShiftInt2 () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1), PARAMETER :: kindInt  = 2
        INTEGER(kindInt)      :: shiftInt
        INTEGER(kindInt)      :: valueInt  = 4
        INTEGER(1)            :: ret = 0


        CALL xOp%Constructor(valueInt)

        fmt = '(A,A,I2,A,L)'

        rOp = valueInt
        shiftInt = 1
        yOp = rOp.SHIFTR.shiftInt
        ret = yOp%GetValueInt8()

        IF (ret == SHIFTA(valueInt,shiftInt)) THEN
            WRITE(*,*) 'TestShiftInt2 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        ELSE
            WRITE(*,*) 'TestShiftInt2 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        END IF
    END FUNCTION


    FUNCTION TestShiftInt4 () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1), PARAMETER :: kindInt  = 4
        INTEGER(kindInt)      :: shiftInt
        INTEGER(kindInt)      :: valueInt  = 4
        INTEGER(1)            :: ret = 0


        CALL xOp%Constructor(valueInt)

        fmt = '(A,A,I2,A,L)'

        rOp = valueInt
        shiftInt = 1
        yOp = rOp.SHIFTR.shiftInt
        ret = yOp%GetValueInt8()

        IF (ret == SHIFTA(valueInt,shiftInt)) THEN
            WRITE(*,*) 'TestShiftInt2 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        ELSE
            WRITE(*,*) 'TestShiftInt2 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        END IF
    END FUNCTION



    FUNCTION TestShiftInt8 () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1), PARAMETER :: kindInt  = 8
        INTEGER(kindInt)      :: shiftInt
        INTEGER(kindInt)      :: valueInt  = 4
        INTEGER(1)            :: ret = 0

        CALL xOp%Constructor(valueInt)

        fmt = '(A,A,I2,A,L)'

        rOp = valueInt
        shiftInt = 1
        yOp = rOp.SHIFTR.shiftInt
        ret = yOp%GetValueInt8()

        IF (ret == SHIFTA(valueInt,shiftInt)) THEN
            WRITE(*,*) 'TestShiftInt2 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        ELSE
            WRITE(*,*) 'TestShiftInt2 TEST PASSED ',oneTab,ret,oneTab,valueInt,oneTab,rOp%IsAReal()
        END IF
    END FUNCTION

    FUNCTION TestAssignRealToReal () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1)            :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560

        CALL xOp%Constructor(valueReal)
        CALL yOp%Constructor(valueReal)
        rOp = xOp
        retReal = rOp%GetValueReal()
        fmt = '(A,A,G,A,G,A,L)'

        IF (ABS(retReal - valueReal)<eps) THEN
            WRITE(*,*) 'AssignSampleToSample real=real TEST PASSED',oneTab,retReal,oneTab,valueReal,oneTab,  rOp%IsAReal()
        ELSE
            WRITE(*,*) 'AssignSampleToSample real=real FAILED',retReal ,valueReal,  ' | ', rOp%IsAReal()
        END IF
    END FUNCTION


    FUNCTION TestOperatorAddInt () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1)            :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560


        fmt = '(A,A,G,A,G,A,L)'
        CALL xOp%Constructor(valueInt)
        CALL yOp%Constructor(valueInt)

        rOp = xOp + yOp
        ret = rOp%GetValueInt8()
        fmt = '(A,A,G,A,G,A,L)'
        IF (ret == (valueInt+valueInt)) THEN
            WRITE(*,*) 'ADD (int+int)) TEST PASSED',oneTab,ret,oneTab,valueInt+valueInt,oneTab, rOp%IsAReal()
        ELSE
            WRITE(*,*) 'ADD (int+int)) TEST PASSED',oneTab,ret,oneTab,valueInt+valueInt,oneTab, rOp%IsAReal()
        END IF

    END FUNCTION

    FUNCTION TestOperatorAddReal () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1)            :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560


        fmt = '(A,A,G,A,G,A,L)'
        CALL xOp%Constructor(valueReal)
        CALL yOp%Constructor(valueReal)

        rOp = xOp + yOp
        retReal = rOp%GetValueReal()
        IF (ABS(retReal -(valueReal + valueReal))<eps) THEN
            WRITE(*,*) 'ADD (real+real)) TEST PASSED',oneTab,retReal,oneTab,valueReal+valueReal,oneTab, rOp%IsAReal()
        ELSE
            WRITE(*,*) 'ADD (real+real)) TEST FAILED',oneTab,retReal,oneTab,valueReal+valueReal,oneTab, rOp%IsAReal()
        END IF

    END FUNCTION



    FUNCTION TestOperatorMultReal () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1)            :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560


        fmt = '(A,A,G,A,G,A,L)'

        CALL xOp%Constructor(valueReal)
        CALL yOp%Constructor(valueReal)

        rOp = xOp * yOp
        retReal = rOp%GetValueReal()
        IF (ABS(retReal -(valueReal*valueReal))<eps) THEN
            WRITE(*,*) 'MULTIPLAY (real*real)) TEST PASSED',oneTab,retReal,oneTab,valueReal*valueReal,oneTab, rOp%IsAReal()
        ELSE
            WRITE(*,*) 'MULTIPLAY (real*real)) TEST FAILED ',oneTab,retReal,oneTab,valueReal*valueReal,oneTab, rOp%IsAReal()
            STOP
        END IF

    END FUNCTION

    FUNCTION TestOperatorMultInt () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1)            :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560


        fmt = '(A,A,G,A,G,A,L)'

        CALL xOp%Constructor(valueInt)
        CALL yOp%Constructor(valueInt)

        rOp = xOp * yOp
        ret = rOp%GetValueInt8()
        IF (ABS(ret  -(valueInt*valueInt)) == 0) THEN
            WRITE(*,*) 'MULTIPLAY (int*int)) TEST PASSED',oneTab,ret,oneTab,valueInt*valueInt,oneTab, rOp%IsAReal()
        ELSE
            WRITE(*,*) 'MULTIPLAY (int*int)) TEST FAILED ',oneTab,ret,oneTab,valueInt*valueInt,oneTab, rOp%IsAReal()
        END IF

    END FUNCTION

    FUNCTION TestOperatorSubInt () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1)            :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560


        fmt = '(A,A,G,A,G,A,L)'
        CALL xOp%Constructor(valueInt)
        CALL yOp%Constructor(valueInt)

        rOp = xOp - yOp
        ret = rOp%GetValueInt8()
        fmt = '(A,A,G,A,G,A,L)'
        IF (ret == (valueInt+valueInt)) THEN
            WRITE(*,*) 'SUB (int-int)) TEST PASSED',oneTab,ret,oneTab,valueInt-valueInt,oneTab, rOp%IsAReal()
        ELSE
            WRITE(*,*) 'SUB (int-int)) TEST PASSED',oneTab,ret,oneTab,valueInt-valueInt,oneTab, rOp%IsAReal()
        END IF

    END FUNCTION

    FUNCTION TestOperatorSubReal () RESULT (res)
        USE sample
        LOGICAL ::  res
        CHARACTER(4)                :: oneTab   = '    '
        CHARACTER(50)               :: fmt
        TYPE (sample_t)       :: xOp, yOp, rOp
        INTEGER(1)            :: valueInt  = 10.0
        INTEGER(1)            :: ret = 0
        REAL(8)               :: retReal
        REAL(8)   , PARAMETER :: eps = 0.0000000001
        REAL(8)   , PARAMETER :: valueReal = 1.1000004
        REAL(8)   , PARAMETER :: valueRealForSub = 0.0102
        REAL(8)   , PARAMETER :: valueReal4 = 11.1560


        fmt = '(A,A,G,A,G,A,L)'
        CALL xOp%Constructor(valueReal)
        CALL yOp%Constructor(valueReal)

        rOp = xOp - yOp
        retReal = rOp%GetValueReal()
        IF (ABS(retReal -(valueReal - valueReal))<eps) THEN
            WRITE(*,*) 'ADD (real+real)) TEST PASSED',oneTab,retReal,oneTab,valueReal-valueReal,oneTab, rOp%IsAReal()
        ELSE
            WRITE(*,*) 'ADD (real+real)) TEST FAILED',oneTab,retReal,oneTab,valueReal-valueReal,oneTab, rOp%IsAReal()
        END IF

    END FUNCTION


END PROGRAM