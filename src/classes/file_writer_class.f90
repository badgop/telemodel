MODULE file_writer_class

    USE aux_file_class
    USE sample
    IMPLICIT NONE

    PRIVATE
    TYPE, PUBLIC , EXTENDS (aux_file_class_t) :: file_writer_class_t

    CONTAINS

        PROCEDURE :: Constructor
        PROCEDURE :: Write
        PROCEDURE :: CloseFile
        PROCEDURE,PRIVATE :: FinalWrite
        FINAL :: destructor

    END TYPE

 CONTAINS


 SUBROUTINE Constructor(this, intKind, buffSize, fileUnitNumber, fileName)
    CLASS(file_writer_class_t  ), INTENT(INOUT)     :: this
    INTEGER(1)  , INTENT (IN)                  :: intKind
    INTEGER(8)  , INTENT (IN)                  :: buffSize
    INTEGER(2)  , INTENT (IN)                  :: fileUnitNumber
    CHARACTER(*), INTENT(IN)                   :: fileName
    LOGICAL                                    :: forWrite

    forWrite = .True.
    this%bufferIsFull = .False.

    CALL this%InnerConstructor(intKind, buffSize, fileUnitNumber, fileName, forWrite)

    CALL this%OpenFile (fileName, forWrite )

 END SUBROUTINE

SUBROUTINE Write(this,insample)
    CLASS (file_writer_class_t), INTENT(INOUT) :: this
    CLASS (sample_t), INTENT(IN)               :: insample
    INTEGER(8)                                 :: value

    IF (this%isOpened .EQV. .False.) THEN
        WRITE (*,*) "File is NOT opened for write or read"
    END IF

    value =  insample%GetValueInt8 ()

    IF (this%buffer_ptr==this%buffSize) this%bufferIsFull = .True.

    IF (this%bufferIsFull) THEN
        this%buffer_ptr = 0
        SELECT CASE(this%intKind)
            CASE(1)
                WRITE(this%fileUnitNumber) this%bufferInt1
            CASE(2)
                WRITE(this%fileUnitNumber) this%bufferInt2
            CASE(4)
                WRITE(this%fileUnitNumber) this%bufferInt4
            CASE(8)
                WRITE(this%fileUnitNumber) this%bufferInt8
        END SELECT
        this%bufferIsFull = .False.
        this%fileLength = this%fileLength +  this%buffSize*this%intKind
    END IF

    this%buffer_ptr = this%buffer_ptr + 1
    ! WRITE(*,*)  this%buffer_ptr
        SELECT CASE(this%intKind)
            CASE(1)
                this%bufferInt1(this%buffer_ptr) = value
            CASE(2)
                this%bufferInt2(this%buffer_ptr) = value
            CASE(4)
                this%bufferInt4(this%buffer_ptr) = value
            CASE(8)
                this%bufferInt8(this%buffer_ptr) = value
         END SELECT

END SUBROUTINE


SUBROUTINE destructor(this)
    TYPE(file_writer_class_t), INTENT(INOUT) :: this
    INTEGER(4) :: status
    CHARACTER(len=60) ::errorCode

    IF (this%isOpened) THEN
        CALL this%FinalWrite()
        CALL this%Innerdestructor()
        CLOSE(this%fileUnitNumber)
        this%isOpened = .False.
    END IF

END SUBROUTINE

SUBROUTINE FinalWrite(this)
    CLASS (file_writer_class_t), INTENT(INOUT) :: this
    INTEGER(8)                            :: i
    IF (this%isOpened) THEN
        DO i=1,this%buffer_ptr
            SELECT CASE(this%intKind)
                CASE(1)
                    WRITE(this%fileUnitNumber)this%bufferInt1(i)
                CASE(2)
                    WRITE(this%fileUnitNumber)this%bufferInt2(i)
                CASE(4)
                    WRITE(this%fileUnitNumber)this%bufferInt4(i)
                CASE(8)
                    WRITE(this%fileUnitNumber)this%bufferInt8(i)
            END SELECT
        END DO
        this%fileLength = this%fileLength + this%buffer_ptr
        this%buffer_ptr = 1
    END IF
END SUBROUTINE


SUBROUTINE CloseFile(this)
    CLASS(file_writer_class_t), INTENT(INOUT)     :: this

    CALL this%FinalWrite()
    CLOSE(this%fileUnitNumber)
    this%isOpened = .False.

END SUBROUTINE



END MODULE