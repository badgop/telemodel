PROGRAM test_sample_reader

    IMPLICIT NONE

    WRITE(*,*) '***************************************'
    WRITE(*,*)
    WRITE(*,*) 'test_sample_reader  test'
    WRITE(*,*) 'test_sample_reader  test_reader_check_file_length test'
    CALL test_reader_check_file_length ()
    WRITE(*,*) 'test_sample_reader  test_reader_read_from_file_full test'
    CALL test_reader_read_from_file_full ()

    WRITE(*,*) 'test_sample_reader  test_reader_read_from_file_with_over_read  '
    CALL test_reader_read_from_file_with_over_read   ()



CONTAINS
!
!  we are testing ChekFile function - it have to get binary file length
!
!
SUBROUTINE test_reader_check_file_length ()
    USE sample
    USE file_reader_class
    USE ModuleWriteReadArrayFromToFile


    TYPE(file_reader_class_t)  :: reader
    TYPE(sample_t)                          :: insample
    INTEGER(1), PARAMETER                   :: intKind = 8
    INTEGER(8)                              :: buffSize
    INTEGER(2)                              :: fileUnitNumber
    CHARACTER(50)                           :: fileName
    LOGICAL                                 :: forWrite = .TRUE.

    INTEGER(8)                              :: i, samples_overall_value, cnt
    INTEGER(intKind), ALLOCATABLE, DIMENSION(:) :: buffer
    INTEGER(intKind), ALLOCATABLE, DIMENSION(:) :: buffer_ethalon

    INTEGER(8):: iostat_Num

    buffSize = 10
    fileUnitNumber = 50

    insample = 0

    samples_overall_value = buffSize*2 + 2

    ALLOCATE(buffer_ethalon(1:samples_overall_value))

    buffer_ethalon = 127

    fileName = 'testInt1Reader.pcm'

    OPEN(fileUnitNumber, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE", FORM="UNFORMATTED",IOSTAT=iostat_Num)

    IF (iostat_Num.GT.0) then
        WRITE(*,*) 'Error accessing file ', fileName
        STOP 'exitting program'
    END IF

    CALL WriteArrayToFile (buffer_ethalon, fileName)

    CLOSE(fileUnitNumber)

    CALL reader%Constructor( intKind  = intKind     &
                            ,buffSize =  buffSize   &
                            ,fileUnitNumber = fileUnitNumber&
                             ,fileName = fileName )

    CALL reader%CloseFile()

    WRITE(*,*) ' records left ', reader%howManyRecordLeft

    IF (reader%howManyRecordLeft == (reader%fileLength/intKind)) THEN
        WRITE(*,*) ' test_reader_check_file_length is PASSED '
    ELSE
        WRITE(*,*) ' test_reader_check_file_length is FAILED '
    END IF

END SUBROUTINE


SUBROUTINE test_reader_read_from_file_full ()
    USE sample
    USE file_reader_class
    USE ModuleWriteReadArrayFromToFile


    TYPE(file_reader_class_t)  :: reader
    TYPE(sample_t)                          :: insample
    INTEGER(1), PARAMETER                   :: intKind = 8
    INTEGER(8)                              :: buffSize
    INTEGER(2)                              :: fileUnitNumber
    CHARACTER(50)                           :: fileName
    LOGICAL                                 :: forWrite = .TRUE.

    INTEGER(8)                              :: i, samples_overall_value, cnt
    INTEGER(intKind), ALLOCATABLE, DIMENSION(:) :: buffer
    INTEGER(intKind), ALLOCATABLE, DIMENSION(:) :: buffer_ethalon

    INTEGER(8):: iostat_Num

    buffSize = 10
    fileUnitNumber = 50
    insample = 0
    samples_overall_value = buffSize*2 + 2

    ALLOCATE(buffer_ethalon(1:samples_overall_value))
    ALLOCATE(buffer(1:samples_overall_value))

    buffer_ethalon = 127
    buffer         = 0
    fileName = 'testInt1Reader.pcm'

    OPEN(fileUnitNumber, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE", FORM="UNFORMATTED",IOSTAT=iostat_Num)

    IF (iostat_Num.GT.0) then
        WRITE(*,*) 'Error accessing file ', fileName
        STOP 'exitting program'
    END IF

    CALL WriteArrayToFile (buffer_ethalon, fileName)
    CLOSE(fileUnitNumber)

    CALL reader%Constructor( intKind  = intKind     &
                            ,buffSize =  buffSize   &
                            ,fileUnitNumber = fileUnitNumber&
                             ,fileName = fileName )

    DO i=1, samples_overall_value
        CALL reader%Read(insample)
        buffer(i) = insample%GetValueInt8()
        WRITE(*,*) buffer(i), i
    END DO

    CALL reader%CloseFile()

    WRITE(*,*) ' records left ', reader%howManyRecordLeft
    cnt = 0
    DO i = 1,samples_overall_value
        IF (buffer_ethalon(i)==buffer(i)) THEN
            cnt = cnt +1
        END IF
    END DO

    IF (cnt == samples_overall_value) THEN
        WRITE(*,*)   "TEST test_reader_read_from_file_full PASSED"
    ELSE
        WRITE(*,*)   "TEST test_reader_read_from_file_full FAILED", cnt, samples_overall_value
    END IF
END SUBROUTINE


SUBROUTINE test_reader_read_from_file_with_over_read ()
    USE sample
    USE file_reader_class
    USE ModuleWriteReadArrayFromToFile


    TYPE(file_reader_class_t)  :: reader
    TYPE(sample_t)                          :: insample
    INTEGER(1), PARAMETER                   :: intKind = 8
    INTEGER(8)                              :: buffSize
    INTEGER(2)                              :: fileUnitNumber
    CHARACTER(50)                           :: fileName
    LOGICAL                                 :: forWrite = .TRUE.

    INTEGER(8)                              :: i, samples_overall_value, cnt
    INTEGER(intKind), ALLOCATABLE, DIMENSION(:) :: buffer
    INTEGER(intKind), ALLOCATABLE, DIMENSION(:) :: buffer_ethalon

    INTEGER(8)                              ::  samples_to_read

    INTEGER(8):: iostat_Num

    buffSize = 10
    fileUnitNumber = 50
    insample = 0
    samples_overall_value = buffSize*2 + 2

    samples_to_read = samples_overall_value +4

    ALLOCATE(buffer_ethalon(1:samples_overall_value))
    ALLOCATE(buffer(1:samples_to_read))

    buffer_ethalon = 127
    buffer         = 0
    fileName = 'testInt1Reader.pcm'

    OPEN(fileUnitNumber, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE", FORM="UNFORMATTED",IOSTAT=iostat_Num)

    IF (iostat_Num.GT.0) then
        WRITE(*,*) 'Error accessing file ', fileName
        STOP 'exitting program'
    END IF

    CALL WriteArrayToFile (buffer_ethalon, fileName)
    CLOSE(fileUnitNumber)

    CALL reader%Constructor( intKind  = intKind     &
                            ,buffSize =  buffSize   &
                            ,fileUnitNumber = fileUnitNumber&
                             ,fileName = fileName )



    DO i=1, samples_to_read
        CALL reader%Read(insample)
        buffer(i) = insample%GetValueInt8()
        WRITE(*,*) buffer(i), i
    END DO

    CALL reader%CloseFile()

    WRITE(*,*) ' records left ', reader%howManyRecordLeft
    cnt = 0


    DO i = 1,samples_overall_value
        IF (buffer_ethalon(i)==buffer(i)) THEN
            cnt = cnt +1
        END IF
    END DO

    IF (cnt == samples_overall_value) THEN
        WRITE(*,*)   "test_reader_read_from_file_with_over_read PASSED"
    ELSE
        WRITE(*,*)   "test_reader_read_from_file_with_over_read FAILED", cnt, samples_overall_value
    END IF
END SUBROUTINE

END PROGRAM