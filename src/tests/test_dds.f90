PROGRAM testDDS

    IMPLICIT NONE

    CALL DDSTestBAse()
    ! CALL DDSTestSetPhase()

    CONTAINS

    SUBROUTINE DDSTestBase()

        USE dds
        USE sample
        USE aux_dds
        USE file_writer_class

        TYPE(dds_t) :: ddsGen
        TYPE(sample_t) :: code_in
        TYPE(sample_t) :: sample_out
        TYPE(file_writer_class_t) :: Writer
        TYPE (sample_t)                 :: input(2)
        TYPE (sample_t)                 :: output(1)


        INTEGER(1) :: romLengthInBits
        INTEGER(1) :: romLenthTruncedInBits
        INTEGER(1) :: outputSignalSampleCapacity

        INTEGER(8) :: samplingFrequency
        INTEGER(8) :: centralFrequency

        INTEGER(8) :: signalLengthInSamples

        REAL(8) :: code_frac
        INTEGER(8) :: code_int

        INTEGER(4) ::   simTime = 201
        INTEGER(8) :: i

        INTEGER(1), PARAMETER                   :: intKind = 2
        INTEGER(8)                              :: buffSize
        INTEGER(2)                              :: fileUnitNumber
        CHARACTER(50)                           :: fileName

        buffSize = 10
        fileUnitNumber = 555
        fileName   = 'ddsTest.pcm'

        CALL Writer%Constructor( intKind  = intKind     &
                                        ,buffSize =  buffSize   &
                                        ,fileUnitNumber = fileUnitNumber&
                                        ,fileName = fileName )

        romLengthInBits = 32
        romLenthTruncedInBits  = 14
        outputSignalSampleCapacity = 16
        centralFrequency = 80

        samplingFrequency = 8000

        CALL ddsGen%Constructor( romLengthInBits = romLengthInBits &
                               , romLengthTruncedInBits = romLenthTruncedInBits   &
                               , outputSignalSampleCapacity = outputSignalSampleCapacity)

        code_int = GetFreqCode(samplingFrequency,centralFrequency,int(romLengthInBits,8))

        WRITE(*,*) '  centralFrequency ', centralFrequency
        WRITE(*,*) '  samplingFrequency ', samplingFrequency
        WRITE(*,*)  ' code_int  ', code_int

        code_in = code_int
        input(1)=code_in
        CALL input(2)%Constructor(0)

        DO i=1, simTime
            CALL ddsGen%In(input)
            CALL ddsGen%Out(output)

             CALL Writer%Write(output(1))
        END DO

    END SUBROUTINE


    ! SUBROUTINE DDSTestSetPhase()

    !     USE DDS
    !     USE sample
    !     USE aux_dds
    !     USE file_writer_class

    !     TYPE(dds_t) :: ddsGen
    !     TYPE(sample_t) :: code_in
    !     TYPE(sample_t) :: sample_out
    !     TYPE(file_writer_class_t) :: Writer


    !     INTEGER(1) :: romLengthInBits
    !     INTEGER(1) :: romLenthTruncedInBits
    !     INTEGER(1) :: outputSignalSampleCapacity

    !     INTEGER(8) :: samplingFrequency
    !     INTEGER(8) :: centralFrequency

    !     INTEGER(8) :: signalLengthInSamples

    !     REAL(8) :: code_frac
    !     INTEGER(8) :: code_int
    !     INTEGER(8) :: phaseCode
    !     REAL(8)    :: initPhase

    !     INTEGER(4) ::   simTime = 201
    !     INTEGER(8) :: i

    !     INTEGER(1), PARAMETER                   :: intKind = 2
    !     INTEGER(8)                              :: buffSize
    !     INTEGER(2)                              :: fileUnitNumber
    !     CHARACTER(50)                           :: fileName


    !     buffSize = 10
    !     fileUnitNumber = 555
    !     fileName   = 'ddsTestPhase.pcm'

    !     CALL Writer%Constructor( intKind  = intKind     &
    !                                     ,buffSize =  buffSize   &
    !                                     ,fileUnitNumber = fileUnitNumber&
    !                                     ,fileName = fileName )

    !     romLengthInBits = 32
    !     romLenthTruncedInBits  = 14
    !     outputSignalSampleCapacity = 16
    !     centralFrequency = 80

    !     samplingFrequency = 8000

    !     CALL ddsGen%Constructor( romLengthInBits = romLengthInBits &
    !                            , romLengthTruncedInBits = romLenthTruncedInBits   &
    !                            , outputSignalSampleCapacity = outputSignalSampleCapacity)

    !     code_int = GetFreqCode(samplingFrequency,centralFrequency,int(romLengthInBits,8))


    !     WRITE(*,*) '  centralFrequency ', centralFrequency
    !     WRITE(*,*) '  samplingFrequency ', samplingFrequency
    !     WRITE(*,*)  ' code_int  ', code_int

    !     initPhase = 90
    !     phaseCode = GetPhaseCode(initPhase,int(romLengthInBits,8))
    !     WRITE(*,*)  ' phaseCode  ', phaseCode

    !     CALL  ddsGen%SetPhase(phaseCode)

    !     code_in = code_int
    !     DO i=1, simTime

    !         CALL ddsGen%Out(code_in,sample_out)
    !         CALL Writer%Write(sample_out)
    !     END DO


    ! END SUBROUTINE


END PROGRAM
