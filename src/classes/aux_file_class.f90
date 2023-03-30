MODULE aux_file_class

    USE array_alloc
    IMPLICIT NONE

    PRIVATE
    TYPE,ABSTRACT, PUBLIC   :: aux_file_class_t

        INTEGER(1)                              :: intKind
        INTEGER(8)                              :: buffSize
        INTEGER(1),ALLOCATABLE, DIMENSION(:)    :: bufferInt1
        INTEGER(2),ALLOCATABLE, DIMENSION(:)    :: bufferInt2
        INTEGER(4),ALLOCATABLE, DIMENSION(:)    :: bufferInt4
        INTEGER(8),ALLOCATABLE, DIMENSION(:)    :: bufferInt8
        INTEGER(4)                              :: buffer_ptr
        INTEGER(2)                              :: fileUnitNumber
        LOGICAL                                 :: isOpened = .FALSE.
        LOGICAL                                 :: bufferIsFull
        ! only when reading files
        ! fileLength measurtes in bytes
        INTEGER(8)                              :: fileLength

    CONTAINS

    PROCEDURE   :: InnerConstructor
    PROCEDURE   :: OpenFile
    PROCEDURE   :: CheckFile
    PROCEDURE   :: Innerdestructor
    PROCEDURE   :: GetFileLength

    END TYPE

 CONTAINS



 SUBROUTINE InnerConstructor(this, intKind, buffSize, fileUnitNumber, fileName, forWrite )
    CLASS(aux_file_class_t), INTENT(INOUT)     :: this
    INTEGER(1)  , INTENT (IN)                  :: intKind
    INTEGER(8)  , INTENT (IN)                  :: buffSize
    INTEGER(2)  , INTENT (IN)                  :: fileUnitNumber
    CHARACTER(*), INTENT(IN)                   :: fileName
    LOGICAL     , INTENT(IN)                   :: forWrite

    this%buffSize       = buffSize
    this%buffer_ptr     = 0
    this%fileUnitNumber = abs(fileUnitNumber)

    this%fileLength = 0


    SELECT CASE(intKind)
        CASE(1)
            this%intKind        = intKind
        CASE(2)
            this%intKind        = intKind
        CASE(4)
            this%intKind        = intKind
        CASE(8)
            this%intKind        = intKind
        CASE DEFAULT
            WRITE(*,*) 'intKind value is wrong'
            STOP
    END SELECT

    SELECT CASE(this%intKind)
        CASE(1)
           CALL AllocArrayWithCheck(this%bufferInt1,this%buffSize)
        CASE(2)
           CALL AllocArrayWithCheck(this%bufferInt2,this%buffSize)
        CASE(4)
           CALL AllocArrayWithCheck(this%bufferInt4,this%buffSize)
        CASE(8)
           CALL AllocArrayWithCheck(this%bufferInt8,this%buffSize)
    END SELECT

 END SUBROUTINE


SUBROUTINE OpenFile(this, fileName, forWrite)
    CLASS(aux_file_class_t), INTENT(INOUT)     :: this
    CHARACTER(*), INTENT(IN)                   :: fileName
    LOGICAL     , INTENT(IN)                   :: forWrite
    INTEGER(4)                                 :: iostatus_Num
    CHARACTER(10), PARAMETER                   :: actionWrite = "WRITE"
    CHARACTER(10), PARAMETER                   :: actionRead  = "READ"
    CHARACTER(10)                              :: actionForFile


    IF (forWrite) THEN
        actionForFile = actionWrite
    ELSE
        actionForFile = actionRead
        CALL this%CheckFile(fileName)
    END IF

    OPEN(this%fileUnitNumber, FILE = fileName, ACCESS="STREAM",ACTION=actionForFile, FORM="UNFORMATTED",IOSTAT=iostatus_Num)

    IF (iostatus_Num.GT.0) then
       WRITE(*,*) 'Error accessing file ', fileName
       STOP 'exitting program'
    END IF

    this%isOpened = .True.

END SUBROUTINE

! SUBROUTINE CloseFile(this)
!     CLASS(aux_file_class_t), INTENT(INOUT)     :: this

!     CLOSE(this%fileUnitNumber)

! END SUBROUTINE


SUBROUTINE CheckFile(this,fileName)
    CLASS(aux_file_class_t), INTENT(INOUT)     :: this

    CHARACTER(*), INTENT(IN)                   :: fileName

    INTEGER(8):: lengthFile=0
    LOGICAL(1):: existsFile=.FALSE.

    INQUIRE(FILE = fileName, SIZE = lengthFile, EXIST= existsFile)

    this%fileLength = lengthFile

    IF (existsFile) THEN
        IF (lengthFile.EQ.0) THEN
            WRITE(*,*) 'File ', fileName, ' is empty'
            STOP 'Exitting'
        END IF
    ELSE
        WRITE(*,*) 'File ', fileName, ' doesnt exists'
        STOP 'exitting program'
    END IF

END SUBROUTINE

FUNCTION GetFileLength (this) RESULT (length)
    CLASS(aux_file_class_t), INTENT(INOUT)     :: this
    INTEGER (8)                                :: length

    length = this%fileLength

END FUNCTION
SUBROUTINE Innerdestructor(this)
    CLASS(aux_file_class_t), INTENT(INOUT) :: this
    INTEGER(4) :: status
    CHARACTER(len=60) ::errorCode

    SELECT CASE(this%intKind)
        CASE(1)
            DEALLOCATE(this%bufferInt1, STAT=status)
            IF (status/=0) THEN
                WRITE(*,*) 'cant DEallocate  this%bufferInt1'
            END IF
        CASE(2)
            DEALLOCATE(this%bufferInt2, STAT=status)
            IF (status/=0) THEN
                WRITE(*,*) 'cant DEallocate  this%bufferInt2'
            END IF
        CASE(4)
            DEALLOCATE(this%bufferInt4, STAT=status)
            IF (status/=0) THEN
                WRITE(*,*) 'cant DEallocate  this%bufferInt4'
            END IF
        CASE(8)
            DEALLOCATE(this%bufferInt8, STAT=status)
            IF (status/=0) THEN
                WRITE(*,*) 'cant DEallocate  this%bufferInt8'
            END IF
    END SELECT


END SUBROUTINE




END MODULE