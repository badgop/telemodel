PROGRAM test_read_write_module
    IMPLICIT NONE

    WRITE(*,*) '***************************************'
    WRITE(*,*)
    WRITE(*,*) 'this read write test'

    CALL TestReadArrayFromFileTypeBinaryInt1()
    CALL TestReadArrayFromFileTypeBinaryInt2()
    CALL TestReadArrayFromFileTypeBinaryInt4()
    CALL TestReadArrayFromFileTypeBinaryInt8()
    CALL TestReadArrayFromFileTypeTxtInt1()
    CALL TestReadArrayFromFileTypeTxtInt2()
    CALL TestReadArrayFromFileTypeTxtInt4()
    CALL TestReadArrayFromFileTypeTxtInt8()
    CALL TestWriteArrayToFileBinaryTypeInt1()
    CALL TestWriteArrayToFileBinaryTypeInt2()
    CALL TestWriteArrayToFileBinaryTypeInt4()
    CALL TestWriteArrayToFileBinaryTypeInt8()

    WRITE(*,*)
    WRITE(*,*) '************************************'

    CONTAINS

    SUBROUTINE TestReadArrayFromFileTypeBinaryInt1()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 1

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        fileName = 'test_bin_file.pcm'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF

        WRITE(10) arrayToWrite
        CLOSE (10)
        CALL ReadArrayFromFile(arrayToRead, fileName)

        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestReadArrayFromFileTypeBinaryInt1 PASSED"
        ELSE
            WRITE(*,*) "TestReadArrayFromFileTypeBinaryInt1 FAILED"
        END IF

    END SUBROUTINE

    SUBROUTINE TestReadArrayFromFileTypeBinaryInt2()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 2

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        fileName = 'test_bin_file.pcm'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF

        WRITE(10) arrayToWrite
        CLOSE (10)
        CALL ReadArrayFromFile(arrayToRead, fileName)

        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestReadArrayFromFileTypeBinaryInt2 PASSED"
        ELSE
            WRITE(*,*) "TestReadArrayFromFileTypeBinaryInt2 FAILED"
        END IF

    END SUBROUTINE

    SUBROUTINE TestReadArrayFromFileTypeBinaryInt4()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 4

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        fileName = 'test_bin_file.pcm'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF

        WRITE(10) arrayToWrite
        CLOSE (10)
        CALL ReadArrayFromFile(arrayToRead, fileName)

        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestReadArrayFromFileTypeBinaryInt4 PASSED"
        ELSE
            WRITE(*,*) "TestReadArrayFromFileTypeBinaryInt4 FAILED"
        END IF

    END SUBROUTINE

    SUBROUTINE TestReadArrayFromFileTypeBinaryInt8()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 8

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        fileName = 'test_bin_file.pcm'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF

        WRITE(10) arrayToWrite
        CLOSE (10)
        CALL ReadArrayFromFile(arrayToRead, fileName)

        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestReadArrayFromFileTypeBinaryInt8 PASSED"
        ELSE
            WRITE(*,*) "TestReadArrayFromFileTypeBinaryInt8 FAILED"
        END IF

    END SUBROUTINE




    SUBROUTINE TestReadArrayFromFileTypeTxtInt1()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 1

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        CHARACTER(10) :: fmt = '(I20)'




        fileName = 'test_bin_file.txt'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="FORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF

        WRITE(10,fmt) arrayToWrite
        CLOSE (10)
        CALL ReadArrayFromFile(arrayToRead, fileName, fmt)

        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestReadArrayFromFileTypeTxtInt1 PASSED"
        ELSE
            WRITE(*,*) "TestReadArrayFromFileTypeTxtInt1 FAILED"
        END IF

    END SUBROUTINE

    SUBROUTINE TestReadArrayFromFileTypeTxtInt2()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 2

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        CHARACTER(10) :: fmt = '(I20)'




        fileName = 'test_bin_file.txt'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="FORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF

        WRITE(10,fmt) arrayToWrite
        CLOSE (10)
        CALL ReadArrayFromFile(arrayToRead, fileName, fmt)

        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestReadArrayFromFileTypeTxtInt2 PASSED"
        ELSE
            WRITE(*,*) "TestReadArrayFromFileTypeTxtInt2 FAILED"
        END IF

    END SUBROUTINE

    SUBROUTINE TestReadArrayFromFileTypeTxtInt4()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 4

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        CHARACTER(10) :: fmt = '(I20)'




        fileName = 'test_bin_file.txt'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="FORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF

        WRITE(10,fmt) arrayToWrite
        CLOSE (10)
        CALL ReadArrayFromFile(arrayToRead, fileName, fmt)

        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestReadArrayFromFileTypeTxtInt4 PASSED"
        ELSE
            WRITE(*,*) "TestReadArrayFromFileTypeTxtInt4 FAILED"
        END IF

    END SUBROUTINE


    SUBROUTINE TestReadArrayFromFileTypeTxtInt8()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 8

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        CHARACTER(10) :: fmt = '(I20)'

        fileName = 'test_bin_file.txt'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="FORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF

        WRITE(10,fmt) arrayToWrite
        CLOSE (10)
        CALL ReadArrayFromFile(arrayToRead, fileName, fmt)

        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestReadArrayFromFileTypeTxtInt8 PASSED"
        ELSE
            WRITE(*,*) "TestReadArrayFromFileTypeTxtInt8 FAILED"
        END IF

    END SUBROUTINE


    SUBROUTINE TestWriteArrayToFileBinaryTypeInt1()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 1

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        fileName = 'test_bin_file.pcm'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        CALL WriteArrayToFile(arrayToWrite,fileName)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF


        ALLOCATE(arrayToRead(1:fileLen))

        READ(10) arrayToRead
        CLOSE (10)


        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestWriteArrayToFileBinaryTypeInt1 PASSED"
        ELSE
            WRITE(*,*) "TestWriteArrayToFileBinaryTypeInt1 FAILED"
        END IF

    END SUBROUTINE


    SUBROUTINE TestWriteArrayToFileBinaryTypeInt2()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 2

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        fileName = 'test_bin_file.pcm'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        CALL WriteArrayToFile(arrayToWrite,fileName)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF


        ALLOCATE(arrayToRead(1:fileLen))

        READ(10) arrayToRead
        CLOSE (10)


        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestWriteArrayToFileBinaryTypeInt2 PASSED"
        ELSE
            WRITE(*,*) "TestWriteArrayToFileBinaryTypeInt2 FAILED"
        END IF

    END SUBROUTINE


    SUBROUTINE TestWriteArrayToFileBinaryTypeInt4()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 4

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        fileName = 'test_bin_file.pcm'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        CALL WriteArrayToFile(arrayToWrite,fileName)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF


        ALLOCATE(arrayToRead(1:fileLen))

        READ(10) arrayToRead
        CLOSE (10)


        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestWriteArrayToFileBinaryTypeInt4 PASSED"
        ELSE
            WRITE(*,*) "TestWriteArrayToFileBinaryTypeInt4 FAILED"
        END IF

    END SUBROUTINE

    SUBROUTINE TestWriteArrayToFileBinaryTypeInt8()
        USE ModuleWriteReadArrayFromToFile

        INTEGER(1), PARAMETER                              :: intKind = 8

        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToWrite
        INTEGER(intKind),DIMENSION(:), ALLOCATABLE         :: arrayToRead
        INTEGER(4), PARAMETER                              :: fileLen = 16384
        CHARACTER(30)                                      :: fileName
        INTEGER(8):: iostat_Num
        INTEGER(4)                                         :: i,cnt
        LOGICAL                                            :: isEq

        fileName = 'test_bin_file.pcm'

        ALLOCATE(arrayToWrite(1:fileLen))
        arrayToWrite = int(65,intKind)

        CALL WriteArrayToFile(arrayToWrite,fileName)

        ! making test binary file
        iostat_Num = 0
        OPEN(10, FILE = fileName, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Error accessing file ', fileName
           STOP 'exitting program'
        END IF


        ALLOCATE(arrayToRead(1:fileLen))

        READ(10) arrayToRead
        CLOSE (10)


        isEq = .FALSE.
        cnt = 0

        DO i=1,fileLen
            IF (arrayToRead(i)==arrayToWrite(1)) THEN
                cnt = cnt + 1
            ELSE
                EXIT
            END IF
        END DO

        IF (cnt == fileLen) THEN
            isEq = .TRUE.
        END IF

        IF (isEq) THEN
            WRITE(*,*) "TestWriteArrayToFileBinaryTypeInt8 PASSED"
        ELSE
            WRITE(*,*) "TestWriteArrayToFileBinaryTypeInt8 FAILED"
        END IF

    END SUBROUTINE






END PROGRAM