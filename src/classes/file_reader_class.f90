MODULE file_reader_class

    USE aux_file_class
    USE sample
    IMPLICIT NONE

    PRIVATE
    TYPE, PUBLIC , EXTENDS (aux_file_class_t) :: file_reader_class_t
        INTEGER(8)       :: howManyRecordLeft
        LOGICAL          :: fileIsEmpty
        LOGICAL          :: wasLastRead
    CONTAINS

        PROCEDURE :: Constructor
        PROCEDURE :: Read
        PROCEDURE :: CloseFile
        PROCEDURE,PRIVATE :: FinalRead
        FINAL :: destructor

    END TYPE

 CONTAINS

 SUBROUTINE Constructor(this, intKind, buffSize, fileUnitNumber, fileName)
    CLASS(file_reader_class_t  ), INTENT(INOUT)     :: this
    INTEGER(1)  , INTENT (IN)                       :: intKind
    INTEGER(8)  , INTENT (IN)                       :: buffSize
    INTEGER(2)  , INTENT (IN)                       :: fileUnitNumber
    CHARACTER(*), INTENT(IN)                        :: fileName
    LOGICAL                                         :: forWrite

    forWrite = .False.
    this%fileIsEmpty = .False.
    ! at first buffer is really empty and it needed to get filled
    this%bufferIsFull = .FAlse.
    this%wasLastRead = .False.


    CALL this%InnerConstructor(intKind, buffSize, fileUnitNumber, fileName, forWrite)
    ! file opened for write so in checkFile subbroutine (look faile_aux_class) constructor will get
    ! file length

    CALL this%OpenFile (fileName, forWrite )

    this%howManyRecordLeft = this%fileLength/this%intKind
    WRITE (*,*) "file length ",   this%fileLength

 END SUBROUTINE

SUBROUTINE Read(this,outsample)
    CLASS (file_reader_class_t), INTENT(INOUT)  :: this
    CLASS (sample_t), INTENT(out)               :: outsample
    INTEGER(8)                                  :: value

    IF (this%isOpened .EQV. .False.) THEN
        WRITE (*,*) "File is NOT opened for write or read"
        STOP
    END IF

    IF (this%fileIsEmpty) THEN
        outsample = 0
    ELSE

        IF (this%buffer_ptr==this%buffSize) this%bufferIsFull = .FAlse.


        IF(.NOT.this%bufferIsFull) THEN

            IF (this%howManyRecordLeft<this%buffSize) THEN
                WRITE(*,*) 'Fianl'
                WRITE(*,*) 'this%howManyRecordLeft', this%howManyRecordLeft
                IF (.NOT. this%wasLastRead) THEN
                    CALL this%FinalRead()
                    this%wasLastRead = .True.
                    this%buffer_ptr = 0
                END IF

            ELSE
                WRITE(*,*) 'read hole budffer'
                SELECT CASE(this%intKind)
                    CASE(1)
                        READ(this%fileUnitNumber) this%bufferInt1
                    CASE(2)
                        READ(this%fileUnitNumber) this%bufferInt2
                    CASE(4)
                        READ(this%fileUnitNumber) this%bufferInt4
                    CASE(8)
                        READ(this%fileUnitNumber) this%bufferInt8
                END SELECT
                this%bufferIsFull = .True.
                this%buffer_ptr = 0

                WRITE(*,*) 'this%howManyRecordLeft', this%howManyRecordLeft
            END IF
        END IF

        this%buffer_ptr = this%buffer_ptr + 1

        this%howManyRecordLeft = this%howManyRecordLeft - 1
        SELECT CASE(this%intKind)
            CASE(1)
                outsample =  this%bufferInt1(this%buffer_ptr)
            CASE(2)
                outsample =  this%bufferInt2(this%buffer_ptr)
            CASE(4)
                outsample =  this%bufferInt4(this%buffer_ptr)
            CASE(8)
                outsample =  this%bufferInt8(this%buffer_ptr)
        END SELECT

        IF (this%howManyRecordLeft==0) this%fileIsEmpty = .True.

    END IF
END SUBROUTINE

SUBROUTINE destructor(this)
    TYPE(file_reader_class_t), INTENT(INOUT) :: this
    INTEGER(4) :: status
    CHARACTER(len=60) ::errorCode

    IF (this%isOpened) THEN
        ! CALL this%FinalWrite()
        CALL this%Innerdestructor()
        CLOSE(this%fileUnitNumber)
    END IF

END SUBROUTINE

SUBROUTINE FinalRead (this)
    CLASS (file_reader_class_t), INTENT(INOUT) :: this
    INTEGER(8)                            :: i

    WRITE (*,*) " Final read   ", this%howManyRecordLeft, this%buffSize
    IF (this%isOpened) THEN
        DO i=1,(this%howManyRecordLeft)
            SELECT CASE(this%intKind)
                CASE(1)
                    READ(this%fileUnitNumber)this%bufferInt1(i)
                CASE(2)
                    READ(this%fileUnitNumber)this%bufferInt2(i)
                CASE(4)
                    READ(this%fileUnitNumber)this%bufferInt4(i)
                CASE(8)
                    READ(this%fileUnitNumber)this%bufferInt8(i)
            END SELECT
        END DO
    END IF
END SUBROUTINE

SUBROUTINE CloseFile(this)
    CLASS(file_reader_class_t), INTENT(INOUT)     :: this

    ! CALL this%FinalWrite()
    CLOSE(this%fileUnitNumber)

END SUBROUTINE

END MODULE