MODULE dds
    USE sample
    USE base_block_class
    USE delay
    IMPLICIT NONE

    PRIVATE

    TYPE, EXTENDS  (base_block_class_t), PUBLIC   :: dds_t

            PRIVATE
            ! ПЗУ таблицы с отчетами синуса
            TYPE(sample_t),DIMENSION(:),ALLOCATABLE :: romSinusTable
            TYPE(delay_t)                      :: feedbackAcc
            TYPE(sample_t) :: internalWires(2)
            !мгновенное значение аккамулятора фазы
            INTEGER(8) :: phaseAccState
            !разрядность аккамулятора фазы
            INTEGER(1) :: romLengthInBits
            !число бит до которых усекатется таблица ПЗУ
            INTEGER(1) :: romLengthTruncedInBits
            !разрядность выходного сигнала с УЧЕТОМ ЗНАКА
            INTEGER(1) :: outputSignalSampleCapacity
            INTEGER(1)      :: neededShift
            !размеры  ПЗУ - число
            INTEGER(8) :: romLengthInNumber
            !размеры усеченнной ПЗУ - число
            INTEGER(8) :: truncedRomLengthInNumber

    CONTAINS

        PROCEDURE :: Constructor
        PROCEDURE :: In => InDDS
        PROCEDURE :: Out => OutDDS
        ! PROCEDURE :: SetPhase
        FINAL :: destructor

    END TYPE dds_t

    CONTAINS

    SUBROUTINE Constructor(this, romLengthInBits, romLengthTruncedInBits, outputSignalSampleCapacity)
        USE sample

        IMPLICIT NONE

        CLASS(DDS_t), INTENT(INOUT) :: this
        INTEGER(1), INTENT(IN)    :: romLengthInBits
        INTEGER(1), INTENT(IN)    :: romLengthTruncedInBits

        INTEGER(1), INTENT(IN)    :: outputSignalSampleCapacity
        INTEGER (8)               :: i
        ! Максимальное выходное значение генератора
        INTEGER (2) :: dacMaxOutputValue
        REAL(8)     :: arg
        REAL(8)     :: PI=3.14159265358979323846264
        INTEGER (2) :: value

        this%romLengthInBits               = romLengthInBits
        this%romLengthTruncedInBits        = romLengthTruncedInBits
        this%outputSignalSampleCapacity    = outputSignalSampleCapacity

        this%neededShift=this%romLengthInBits-this%romLengthTruncedInBits

        this%romLengthInNumber                      = int(2,8)**int(this%romLengthInBits,8)
        this%truncedRomLengthInNumber               = int(2,8)**int(this%romLengthTruncedInBits,8)
        WRITE(*,*) 'this%romLengthInNumber ',this%romLengthInNumber

        WRITE(*,*) ' this%truncedRomLengthInNumber ', this%truncedRomLengthInNumber

        ALLOCATE(this%romSinusTable(0:this%truncedRomLengthInNumber -1))

        dacMaxOutputValue=int(  (int(2,2)**(outputSignalSampleCapacity-1)-1),2  )

        arg = 2*PI*(1/float(this%truncedRomLengthInNumber))

        DO i=0,this%truncedRomLengthInNumber-1
            value = int((sin(arg*i)*dacMaxOutputValue),2)
            ! WRITE(*,*) 'value ', value
            CALL this%romSinusTable(i)%Constructor(value)
        END DO

        this%phaseAccState = 0
    END SUBROUTINE Constructor

    ! InputSamples(1)  - frequency code
    ! InputSamples(2)  - phase accumulator reset signal input
    SUBROUTINE InDDS (this, inputSamples)
        CLASS (dds_t), INTENT(INOUT)               :: this
        TYPE(sample_t), INTENT(IN), DIMENSION(1:)  :: InputSamples

        this%internalWires(1)=InputSamples(1)

    END SUBROUTINE


    SUBROUTINE OutDDS (this, OutputSamples)
        CLASS (dds_t), INTENT(INOUT) :: this
        TYPE(sample_t), INTENT(OUT), DIMENSION(1:) :: OutputSamples
        INTEGER(8)                          :: resultPhase

        IF (this%phaseAccState < 0) THEN
            this%phaseAccState = this%phaseAccState + this%romLengthInNumber
        END IF
        IF (this%phaseAccState >= (this%romLengthInNumber-1)) THEN
            this%phaseAccState = this%phaseAccState - this%romLengthInNumber
        END IF

        resultPhase = SHIFTA(this%phaseAccState,this%neededShift)

        OutputSamples(1) = this%romSinusTable(resultPhase)

        this%phaseAccState = this%phaseAccState + this%internalWires(1)%GetValueInt8()

    END SUBROUTINE


    ! деструкторы запускаются автоматически, после того как
    ! созданный обьект выйдет из области видимости.
    SUBROUTINE destructor(this)
        TYPE(DDS_t), INTENT(INOUT) :: this
           IF (ALLOCATED(this%romSinusTable) )  DEALLOCATE(this%romSinusTable)
!           WRITE(*,*) 'DDS_t destructor завершил работу!'
    END SUBROUTINE


END MODULE