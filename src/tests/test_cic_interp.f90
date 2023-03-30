PROGRAM test_interp
    IMPLICIT none;


    CALL test_cic_interp()
    CALL test_cic_interp_sine()


CONTAINS

    SUBROUTINE test_cic_interp()

        USE sample
        USE cic_interp
        USE file_writer_class


        TYPE(cic_interp_t) interpolator;
        TYPE(sample_t)  inSample(1);
        TYPE(sample_t)  outSample(1);
        TYPE(file_writer_class_t) :: Writer


        INTEGER(1)                              :: order
        INTEGER(8)                              :: i, j
        INTEGER(2)                              :: r
        INTEGER(8)                              :: time



        INTEGER(1), PARAMETER                   :: intKind = 2
        INTEGER(8)                              :: buffSize
        INTEGER(2)                              :: fileUnitNumber
        CHARACTER(50)                           :: fileName

        buffSize = 10
        fileUnitNumber = 555
        fileName   = 'cic_interp_testt.pcm'


        CALL Writer%Constructor( intKind  = intKind     &
        ,buffSize =  buffSize   &
        ,fileUnitNumber = fileUnitNumber&
        ,fileName = fileName )


        order = 4
        r = 1000

        time = 100

        CALL interpolator%Constructor(order, r)

        CALL inSample(1)%Constructor(1024)

        DO i = 1, time

            CALL interpolator%In(inSample)

            DO  j = 1, r
                CALL interpolator%Out(outSample)
                outSample(1) = outSample(1).SHIFTR.25
                CALL Writer%Write(outSample(1))
            END DO

        END DO



    END SUBROUTINE


    SUBROUTINE test_cic_interp_sine()

        USE dds
        USE sample
        USE aux_dds
        USE file_writer_class
        USE cic_interp

        TYPE(dds_t) :: ddsGen
        TYPE(sample_t) :: code_in
        TYPE(sample_t) :: sample_out
        TYPE(file_writer_class_t) :: Writer
        TYPE (sample_t)                 :: input(2)
        TYPE (sample_t)                 :: output(1)

        TYPE(cic_interp_t) interpolator;
        TYPE(sample_t)  inSample(1);
        TYPE(sample_t)  outSample(1);



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



        INTEGER(1)                              :: order
        INTEGER(8)                              :: j
        INTEGER(2)                              :: r
        INTEGER(8)                              :: time


        INTEGER(1), PARAMETER                   :: intKind = 2
        INTEGER(8)                              :: buffSize
        INTEGER(2)                              :: fileUnitNumber
        CHARACTER(50)                           :: fileName

        buffSize = 10
        fileUnitNumber = 555
        fileName   = ' test_cic_interp_sine.pcm'

        CALL Writer%Constructor( intKind  = intKind     &
                                        ,buffSize =  buffSize   &
                                        ,fileUnitNumber = fileUnitNumber&
                                        ,fileName = fileName )

        romLengthInBits = 32
        romLenthTruncedInBits  = 14
        outputSignalSampleCapacity = 16
        centralFrequency = 800

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


        order = 4
        r = 10

        simTime = 100

        CALL interpolator%Constructor(order, r)

        CALL inSample(1)%Constructor(1024)

        DO i=1, simTime
            CALL ddsGen%In(input)
            CALL ddsGen%Out(output)

            CALL interpolator%In(output)

            DO  j = 1, r
                CALL interpolator%Out(outSample)
                outSample(1) = outSample(1).SHIFTR.10
                CALL Writer%Write(outSample(1))
            END DO
         END DO

    END SUBROUTINE


END PROGRAM