PROGRAM test_sample_writer

    IMPLICIT NONE

    WRITE(*,*) '***************************************'
    WRITE(*,*)
    WRITE(*,*) 'test_sample_writer  test'
    CALL test_writerInt1_buffer_not_full ()
    CALL test_writerInt1_buffer_is_full ()


CONTAINS

SUBROUTINE test_writerInt1_buffer_not_full ()
    USE sample
    USE file_writer_class
    USE ModuleWriteReadArrayFromToFile

    TYPE(sample_t)                          :: insample
    TYPE(file_writer_class_t)               :: testWriterInt1
    INTEGER(1), PARAMETER                   :: intKind = 1
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

    insample = 10

    samples_overall_value = buffSize - 2

    ALLOCATE(buffer_ethalon(1:samples_overall_value))

    fileName = 'testInt1Writer.pcm'

    CALL testWriterInt1%Constructor( intKind  = intKind     &
                                    ,buffSize =  buffSize   &
                                    ,fileUnitNumber = fileUnitNumber&
                                    ,fileName = fileName )


    DO i = 1,samples_overall_value
        CALL testWriterInt1%Write (insample)
         buffer_ethalon(i) = insample%GetValueInt8()
    END DO

    CALL testWriterInt1%CloseFile()
    ! making test binary file
    iostat_Num = 0

    OPEN(fileUnitNumber, FILE = fileName, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

    IF (iostat_Num.GT.0) then
       WRITE(*,*) 'Error accessing file ', fileName
       STOP 'exitting program'
    END IF

    CALL ReadArrayFromFile(buffer, fileName)

    CLOSE(fileUnitNumber)

    cnt = 0

    DO i = 1,samples_overall_value

        IF (buffer_ethalon(i)==buffer(i)) THEN
            ! WRITE(*,*)  buffer_ethalon(i), buffer(i)
            cnt = cnt +1
        END IF

    END DO

    WRITE (*,*) "Writed file length ", testWriterInt1%GetFileLength()

    IF (cnt == samples_overall_value) THEN
        WRITE(*,*)   "TEST test_writerInt1_buffer_not_full PASSED"
    ELSE
        WRITE(*,*)   "TEST test_writerInt1_buffer_not_full FAILED"
    END IF


END SUBROUTINE


SUBROUTINE test_writerInt1_buffer_is_full ()
    USE sample
    USE file_writer_class
    USE ModuleWriteReadArrayFromToFile

    TYPE(sample_t)                          :: insample
    TYPE(file_writer_class_t)               :: testWriterInt1
    INTEGER(1), PARAMETER                   :: intKind = 1
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

    insample = 10

    samples_overall_value = buffSize

    ALLOCATE(buffer_ethalon(1:samples_overall_value))

    fileName = 'testInt1Writer2.pcm'

    CALL testWriterInt1%Constructor( intKind  = intKind     &
                                    ,buffSize =  buffSize   &
                                    ,fileUnitNumber = fileUnitNumber&
                                    ,fileName = fileName )


    DO i = 1,samples_overall_value
        CALL testWriterInt1%Write (insample)
         buffer_ethalon(i) = insample%GetValueInt8()
    END DO

    CALL testWriterInt1%CloseFile()
    ! making test binary file
    iostat_Num = 0

    OPEN(fileUnitNumber, FILE = fileName, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

    IF (iostat_Num.GT.0) then
       WRITE(*,*) 'Error accessing file ', fileName
       STOP 'exitting program'
    END IF

    CALL ReadArrayFromFile(buffer, fileName)

    CLOSE(fileUnitNumber)

    cnt = 0

    DO i = 1,samples_overall_value

        IF (buffer_ethalon(i)==buffer(i)) THEN
            ! WRITE(*,*)  buffer_ethalon(i), buffer(i)
            cnt = cnt +1
        END IF

    END DO

    WRITE (*,*) "Writed file length ", testWriterInt1%GetFileLength()

    IF (cnt == samples_overall_value) THEN
        WRITE(*,*)   "TEST test_writerInt1_buffer_IS_full PASSED"
    ELSE
        WRITE(*,*)   "TEST test_writerInt1_buffer_IS_full FAILED"
    END IF


END SUBROUTINE

END PROGRAM