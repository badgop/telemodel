MODULE array_alloc

    IMPLICIT NONE

    INTERFACE  AllocArrayWithCheck

    MODULE PROCEDURE      AllocArrayWithCheckInt1
    MODULE PROCEDURE      AllocArrayWithCheckInt2
    MODULE PROCEDURE      AllocArrayWithCheckInt4
    MODULE PROCEDURE      AllocArrayWithCheckInt8

    END INTERFACE


CONTAINS

    SUBROUTINE AllocArrayWithCheckInt1 (arr, len)
        INTEGER(1), PARAMETER          :: intKind = 1
        INTEGER(intKind), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: arr
        INTEGER(8), INTENT(IN)  :: len
        INTEGER(4) :: status
        CHARACTER(len=60) ::errorCode

        IF (ALLOCATED(arr)) THEN
            DEALLOCATE(arr, STAT=status)
            IF (status/=0) THEN
                WRITE(*,*) ' Cant DEallocate memory for array '
                STOP
            END IF
        END IF

        status=0

        ALLOCATE (arr(1:len),STAT=status,ERRMSG = errorCode)

        IF (status/=0) THEN
            WRITE (*,*) 'Cant allocate memory for array,ERRMSG = ',errorCode
            STOP
        END IF
    END SUBROUTINE

    SUBROUTINE AllocArrayWithCheckInt2 (arr, len)
        INTEGER(1), PARAMETER          :: intKind = 2
        INTEGER(intKind), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: arr
        INTEGER(8), INTENT(IN)  :: len
        INTEGER(4) :: status
        CHARACTER(len=60) ::errorCode

        IF (ALLOCATED(arr)) THEN
            DEALLOCATE(arr, STAT=status)
            IF (status/=0) THEN
                WRITE(*,*) ' Cant DEallocate memory for array '
                STOP
            END IF
        END IF

        status=0

        ALLOCATE (arr(1:len),STAT=status,ERRMSG = errorCode)

        IF (status/=0) THEN
            WRITE (*,*) 'Cant allocate memory for array,ERRMSG = ',errorCode
            STOP
        END IF
    END SUBROUTINE


    SUBROUTINE AllocArrayWithCheckInt4 (arr, len)
        INTEGER(1), PARAMETER          :: intKind = 4
        INTEGER(intKind), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: arr
        INTEGER(8), INTENT(IN)  :: len
        INTEGER(4) :: status
        CHARACTER(len=60) ::errorCode

        IF (ALLOCATED(arr)) THEN
            DEALLOCATE(arr, STAT=status)
            IF (status/=0) THEN
                WRITE(*,*) ' Cant DEallocate memory for array '
                STOP
            END IF
        END IF

        status=0

        ALLOCATE (arr(1:len),STAT=status,ERRMSG = errorCode)

        IF (status/=0) THEN
            WRITE (*,*) 'Cant allocate memory for array,ERRMSG = ',errorCode
            STOP
        END IF
    END SUBROUTINE


    SUBROUTINE AllocArrayWithCheckInt8 (arr, len)
        INTEGER(1), PARAMETER          :: intKind = 8
        INTEGER(intKind), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: arr
        INTEGER(8), INTENT(IN)  :: len
        INTEGER(4) :: status
        CHARACTER(len=60) ::errorCode

        IF (ALLOCATED(arr)) THEN
            DEALLOCATE(arr, STAT=status)
            IF (status/=0) THEN
                WRITE(*,*) ' Cant DEallocate memory for array '
                STOP
            END IF
        END IF

        status=0

        ALLOCATE (arr(1:len),STAT=status,ERRMSG = errorCode)

        IF (status/=0) THEN
            WRITE (*,*) 'Cant allocate memory for array,ERRMSG = ',errorCode
            STOP
        END IF
    END SUBROUTINE

END MODULE
