MODULE sample

    IMPLICIT NONE

    TYPE, PUBLIC :: sample_t
    PRIVATE

    INTEGER(8)   :: valueInt   =  0
    REAL(8)      :: valueReal  =  0.0
    LOGICAL      :: isReal     = .false.

    CONTAINS
    PROCEDURE :: ConstructorInt1
    PROCEDURE :: ConstructorInt2
    PROCEDURE :: ConstructorInt4
    PROCEDURE :: ConstructorInt8

    PROCEDURE :: ConstructorReal8
    PROCEDURE :: ConstructorReal4
    GENERIC   :: Constructor => ConstructorInt1 , ConstructorInt2, ConstructorInt4, ConstructorInt8, &
                                ConstructorReal8, ConstructorReal4
    PROCEDURE :: Add
    GENERIC   :: operator   (+) =>   Add
    PROCEDURE :: Sub
    GENERIC   :: operator   (-) =>   Sub
    PROCEDURE :: Multiplay
    GENERIC   :: operator   (*) =>   Multiplay

    PROCEDURE :: AssignSampleToSample
    PROCEDURE :: AssignSampleToInt1
    PROCEDURE :: AssignSampleToInt2
    PROCEDURE :: AssignSampleToInt4
    PROCEDURE :: AssignSampleToInt8
    GENERIC   :: assignment (=) =>  AssignSampleToSample ,AssignSampleToInt1 &
                                    , AssignSampleToInt2, AssignSampleToInt4 &
                                    , AssignSampleToInt8


    PROCEDURE :: ShiftRightInt1
    PROCEDURE :: ShiftRightInt2
    PROCEDURE :: ShiftRightInt4
    PROCEDURE :: ShiftRightInt8
     generic :: operator   (.SHIFTR.) =>  ShiftRightInt1, ShiftRightInt2&
                                       , ShiftRightInt4, ShiftRightInt8
    PROCEDURE :: GetValueInt8
    PROCEDURE :: GetValueReal
    PROCEDURE :: GetBoolean
    PROCEDURE :: IsAReal

    END TYPE

    CONTAINS

SUBROUTINE  ConstructorInt1(this, input)
    CLASS(sample_t), INTENT(INOUT) :: this
    INTEGER(1),PARAMETER           :: arrayKind = 1
    INTEGER(arrayKind), INTENT(IN) :: input

    this%valueInt = input
    this%isReal = .false.
    this%valueReal = 0.0

END SUBROUTINE

SUBROUTINE  ConstructorInt2(this, input)
    CLASS(sample_t), INTENT(INOUT) :: this
    INTEGER(1),PARAMETER           :: arrayKind = 2
    INTEGER(arrayKind), INTENT(IN) :: input

    this%valueInt = input
    this%isReal = .false.
    this%valueReal = 0.0

END SUBROUTINE

SUBROUTINE  ConstructorInt4(this, input)
    CLASS(sample_t), INTENT(INOUT) :: this
    INTEGER(1),PARAMETER           :: arrayKind = 4
    INTEGER(arrayKind), INTENT(IN) :: input

    this%valueInt = input
    this%isReal = .false.
    this%valueReal = 0.0

END SUBROUTINE

SUBROUTINE  ConstructorInt8(this, input)
    CLASS(sample_t), INTENT(INOUT) :: this
    INTEGER(1),PARAMETER           :: arrayKind = 8
    INTEGER(arrayKind), INTENT(IN) :: input

    this%valueInt = input
    this%isReal = .false.
    this%valueReal = 0.0

END SUBROUTINE

SUBROUTINE  ConstructorReal8(this, input)
    CLASS(sample_t), INTENT(INOUT) :: this
    REAL(8), INTENT(IN)            :: input

    this%valueReal = input
    this%isReal = .true.
    this%valueInt = 0

END SUBROUTINE

SUBROUTINE  ConstructorReal4(this, input)
    CLASS(sample_t), INTENT(INOUT) :: this
    REAL(4), INTENT(IN)            :: input

    this%valueReal = input
    this%isReal = .true.
    this%valueInt = 0

END SUBROUTINE

SUBROUTINE AssignSampleToSample (leftOp, rightOP)
    CLASS(sample_t), INTENT(INOUT)    :: leftOp
    CLASS(sample_t), INTENT(IN)       :: rightOP


    IF (rightOP%isReal) THEN
        CALL leftOp%Constructor(rightOP%valueReal)
    ELSE
        CALL leftOp%Constructor(rightOP%valueInt)
    END IF

END SUBROUTINE

SUBROUTINE AssignSampleToInt1 (leftOp, rightOP)
    CLASS(sample_t), INTENT(INOUT)     :: leftOp
    INTEGER(1), PARAMETER              :: kindInt = 1
    INTEGER(kindInt), INTENT(IN)       :: rightOP

    CALL leftOp%Constructor(rightOP)

END SUBROUTINE

SUBROUTINE AssignSampleToInt2 (leftOp, rightOP)
    CLASS(sample_t), INTENT(INOUT)     :: leftOp
    INTEGER(1), PARAMETER              :: kindInt = 2
    INTEGER(kindInt), INTENT(IN)       :: rightOP

    CALL leftOp%Constructor(rightOP)

END SUBROUTINE

SUBROUTINE AssignSampleToInt4 (leftOp, rightOP)
    CLASS(sample_t), INTENT(INOUT)     :: leftOp
    INTEGER(1), PARAMETER              :: kindInt = 4
    INTEGER(kindInt), INTENT(IN)       :: rightOP

    CALL leftOp%Constructor(rightOP)

END SUBROUTINE

SUBROUTINE AssignSampleToInt8 (leftOp, rightOP)
    CLASS(sample_t), INTENT(INOUT)     :: leftOp
    INTEGER(1), PARAMETER              :: kindInt = 8
    INTEGER(kindInt), INTENT(IN)       :: rightOP

    CALL leftOp%Constructor(rightOP)

END SUBROUTINE

FUNCTION ShiftRightInt1 (leftOp, rightOP) RESULT (res)
    CLASS(sample_t), INTENT(IN)     :: leftOp
    INTEGER(1), PARAMETER              :: kindInt = 1
    INTEGER(kindInt), INTENT(IN)       :: rightOP
    CLASS(sample_t), ALLOCATABLE       :: res

    ALLOCATE(res)
    IF (.NOT.leftOp%isReal) THEN
        CALL res%Constructor( SHIFTA(leftOp%valueInt,abs(rightOP)))
    END IF

END FUNCTION

FUNCTION ShiftRightInt2 (leftOp, rightOP) RESULT (res)
    CLASS(sample_t), INTENT(IN)     :: leftOp
    INTEGER(1), PARAMETER              :: kindInt = 2
    INTEGER(kindInt), INTENT(IN)       :: rightOP
    CLASS(sample_t), ALLOCATABLE       :: res

    ALLOCATE(res)
    IF (.NOT.leftOp%isReal) THEN
        CALL res%Constructor( SHIFTA(leftOp%valueInt,abs(rightOP)))
    END IF

END FUNCTION

FUNCTION ShiftRightInt4 (leftOp, rightOP) RESULT (res)
    CLASS(sample_t), INTENT(IN)     :: leftOp
    INTEGER(1), PARAMETER              :: kindInt = 4
    INTEGER(kindInt), INTENT(IN)       :: rightOP
    CLASS(sample_t), ALLOCATABLE       :: res

    ALLOCATE(res)
    IF (.NOT.leftOp%isReal) THEN
        CALL res%Constructor( SHIFTA(leftOp%valueInt,abs(rightOP)))
    END IF

END FUNCTION

FUNCTION ShiftRightInt8 (leftOp, rightOP) RESULT (res)
    CLASS(sample_t), INTENT(IN)     :: leftOp
    INTEGER(1), PARAMETER              :: kindInt = 8
    INTEGER(kindInt), INTENT(IN)       :: rightOP
    CLASS(sample_t), ALLOCATABLE       :: res

    ALLOCATE(res)
    IF (.NOT.leftOp%isReal) THEN
        CALL res%Constructor( SHIFTA(leftOp%valueInt,abs(rightOP)))
    END IF

END FUNCTION

FUNCTION   Add(xOp,yOp)  RESULT (ret)
    CLASS(sample_t), INTENT(IN)  :: xOp
    CLASS(sample_t), INTENT(IN)  :: yOp
    CLASS(sample_t), allocatable :: ret

    ALLOCATE(ret)
    !REAL + REAL
    IF (xOp%isReal.AND.yOp%isReal) THEN
        CALL ret%Constructor(xOp%valueReal + yOp%valueReal)
    END IF

    !REAL + INT
    IF (xOp%isReal.NEQV.yOp%isReal) THEN
        STOP 'trying ADD real and int UNSUPPORTED NOW'
    END IF
    ! INT + INT
    IF (.NOT.(xOp%isReal.AND.yOp%isReal)) THEN
        CALL ret%Constructor( xOp%valueInt  + yOp%valueInt)
    END IF

END FUNCTION   Add

FUNCTION   Sub(xOp,yOp)  RESULT (ret)
    CLASS(sample_t), INTENT(IN)  :: xOp
    CLASS(sample_t), INTENT(IN)  :: yOp
    CLASS(sample_t), allocatable :: ret

    ALLOCATE(ret)

    !REAL - REAL
    IF (xOp%isReal.AND.yOp%isReal) THEN
        CALL ret%Constructor(xOp%valueReal - yOp%valueReal)
    END IF

    !REAL - INT
    IF (xOp%isReal.NEQV.yOp%isReal) THEN
        STOP 'trying SUB real and int UNSUPPORTED NOW'
    END IF
    ! INT - INT
    IF (.NOT.(xOp%isReal.AND.yOp%isReal)) THEN
        CALL ret%Constructor( xOp%valueInt  - yOp%valueInt)
    END IF

END FUNCTION   Sub

FUNCTION   Multiplay(xOp,yOp)  RESULT (ret)
    CLASS(sample_t), INTENT(IN)  :: xOp
    CLASS(sample_t), INTENT(IN)  :: yOp
    CLASS(sample_t), allocatable :: ret

    ALLOCATE(ret)

        !REAL + REAL
    IF (xOp%isReal.AND.yOp%isReal) THEN
        CALL ret%Constructor(xOp%valueReal * yOp%valueReal)

    END IF

    !REAL - INT
    IF (xOp%isReal.NEQV.yOp%isReal) THEN
        STOP 'trying MULTIPLAY real and int UNSUPPORTED NOW'
    END IF
    ! INT - INT
    IF (.NOT.(xOp%isReal.AND.yOp%isReal)) THEN

        CALL ret%Constructor( xOp%valueInt  * yOp%valueInt)
    END IF

END FUNCTION    Multiplay

FUNCTION GetValueInt8 (this) RESULT (ret)
    CLASS(sample_t), INTENT(IN)  :: this
    INTEGER(8)                   :: ret

    ret = this%valueInt

END FUNCTION

FUNCTION GetValueReal (this) RESULT (ret)
    CLASS(sample_t), INTENT(IN)  :: this
    REAL(8)                      :: ret

    ret = this%valueReal

END FUNCTION

! Get from integer or real boolean value.
FUNCTION GetBoolean (this) RESULT (ret)
    CLASS(sample_t), INTENT(IN)  :: this
    LOGICAL                      :: ret

    ret = .False.
    IF (this%IsReal) THEN
        IF(this%valueReal.GT.(0.0))  ret = .True.
    ELSE
        IF(this%valueInt.GT.(0))     ret = .True.
    END IF

END FUNCTION

FUNCTION IsAReal (this) RESULT (ret)
    CLASS(sample_t), INTENT(IN)  :: this
    LOGICAL                      :: ret

    ret = this%isReal

END FUNCTION

END MODULE