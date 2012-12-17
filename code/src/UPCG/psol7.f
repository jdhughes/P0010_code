      MODULE PSOLMODULE
        DOUBLEPRECISION, PARAMETER :: PI = 3.141592653589793
        DOUBLEPRECISION, DIMENSION(5) :: SCAL = 
     2    (/ 1.0D0, 0.1D0, 0.1D0, 0.1D0, 1.0D0 /)
        TYPE TGLSPOLY
          INTEGER :: NDEGREE
          INTEGER :: NLANSTEP
          INTEGER :: NLAN2
          INTEGER :: NINTV = 1
          INTEGER :: IEIGCALC
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: D_LANCZOS
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: ALPHA
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: BETA
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: GAMMA
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: D_V1
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: D_V0
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: D_V
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: D_E
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: INTV
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: P0
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: PPOL
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: APPOL
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: QPOL
        END TYPE TGLSPOLY
        INTEGER,SAVE,POINTER  :: NMETH,ITER1C,NPC,NOPT,NTRD,NTRDV
        INTEGER,SAVE,POINTER  :: NITERC,NNZC,NIAC
        INTEGER,SAVE,POINTER  :: NIAPC,NIWC,NPOL,NEIG
        REAL   ,SAVE,POINTER  :: HCLOSEPSOL,RCLOSEPSOL
        DOUBLE PRECISION, SAVE, POINTER :: PSOLTOTT, PSOLFMAT
        DOUBLE PRECISION, SAVE, POINTER :: PSOLPCUT, PSOLPCAT
        DOUBLE PRECISION, SAVE, POINTER :: PSOLDPT, PSOLMVT
        DOUBLE PRECISION, SAVE, POINTER :: PSOLAXPYT,PSOLVVPT,PSOLMISCT
        DOUBLE PRECISION, SAVE, POINTER :: PSOLGPUTT
        INTEGER,SAVE,POINTER  :: IPSOLO,IPSOLI
        INTEGER,          SAVE, POINTER, DIMENSION(:,:,:) :: NODEC
        DOUBLE PRECISION, SAVE, POINTER, DIMENSION(:)     :: BC
        DOUBLE PRECISION, SAVE, POINTER, DIMENSION(:)     :: XC
        DOUBLE PRECISION, SAVE, POINTER, DIMENSION(:)     :: AC
        DOUBLE PRECISION, SAVE, POINTER, DIMENSION(:)     :: APC
        INTEGER,          SAVE, POINTER, DIMENSION(:)     :: IAC
        INTEGER,          SAVE, POINTER, DIMENSION(:)     :: JAC
        INTEGER,          SAVE, POINTER, DIMENSION(:)     :: IUC
        INTEGER,          SAVE, POINTER, DIMENSION(:)     :: IXMAP
C         WORKING ARRAYS        
        INTEGER,         SAVE, POINTER, DIMENSION(:)      :: IWC
        DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: DC
        DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: PC
        DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: QC
        DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: ZC
C         DIAGONAL SCALING VECTOR
        INTEGER,SAVE,POINTER  :: ISCL
        DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: SCL
        DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: SCLI
C         POLYNOMIAL PRECONDITIONER        
        TYPE (TGLSPOLY),SAVE, POINTER :: GLSPOLY
!C         GPU POINTERS 
!        INTEGER(KIND=8), SAVE, POINTER  :: CU_HDL,CU_STAT,CU_DES
!        INTEGER(KIND=8), SAVE, POINTER  :: CU_IAC,CU_JAC
!        INTEGER(KIND=8), SAVE, POINTER  :: CU_AC,CU_APC,CU_XC
!        INTEGER(KIND=8), SAVE, POINTER  :: CU_DC,CU_ZC,CU_PC,CU_QC
!        INTEGER(KIND=8), SAVE, POINTER  :: CU_SCL,CU_SCLI
!        INTEGER(KIND=8), SAVE, POINTER  :: CU_V,CU_V0,CU_V1
!        INTEGER(KIND=8), SAVE, POINTER  :: PL_DC,PL_ZC
!
      TYPE PSOLTYPE
        INTEGER,POINTER  :: NMETH,ITER1C,NPC,NOPT,NTRD,NTRDV
        INTEGER,POINTER  :: NITERC,NNZC,NIAC
        INTEGER,POINTER  :: NIAPC,NIWC,NPOL,NEIG
        REAL   ,POINTER  :: HCLOSEPSOL,RCLOSEPSOL
        DOUBLE PRECISION, POINTER :: PSOLTOTT, PSOLFMAT
        DOUBLE PRECISION, POINTER :: PSOLPCUT, PSOLPCAT
        DOUBLE PRECISION, POINTER :: PSOLDPT, PSOLMVT
        DOUBLE PRECISION, POINTER :: PSOLAXPYT,PSOLVVPT,PSOLMISCT
        DOUBLE PRECISION, POINTER :: PSOLGPUTT
        INTEGER,POINTER  :: IPSOLO,IPSOLI
        INTEGER,          POINTER, DIMENSION(:,:,:) :: NODEC
        DOUBLE PRECISION, POINTER, DIMENSION(:)     :: BC
        DOUBLE PRECISION, POINTER, DIMENSION(:)     :: XC
        DOUBLE PRECISION, POINTER, DIMENSION(:)     :: AC
        DOUBLE PRECISION, POINTER, DIMENSION(:)     :: APC
        INTEGER,          POINTER, DIMENSION(:)     :: IAC
        INTEGER,          POINTER, DIMENSION(:)     :: JAC
        INTEGER,          POINTER, DIMENSION(:)     :: IUC
        INTEGER,          POINTER, DIMENSION(:)     :: IXMAP
C         WORKING ARRAYS        
        INTEGER,         POINTER, DIMENSION(:)      :: IWC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: DC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: PC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: QC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: ZC
C         DIAGONAL SCALING VECTOR
        INTEGER,POINTER  :: ISCL
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: SCL
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: SCLI
C         POLYNOMIAL PRECONDITIONER        
        TYPE (TGLSPOLY),POINTER :: GLSPOLY
!C         GPU POINTERS 
!        INTEGER(KIND=8),POINTER  :: CU_HDL,CU_STAT,CU_DES
!        INTEGER(KIND=8),POINTER  :: CU_IAC,CU_JAC
!        INTEGER(KIND=8),POINTER  :: CU_AC,CU_APC,CU_XC
!        INTEGER(KIND=8),POINTER  :: CU_DC,CU_ZC,CU_PC,CU_QC
!        INTEGER(KIND=8),POINTER  :: CU_SCL,CU_SCLI
!        INTEGER(KIND=8),POINTER  :: CU_V,CU_V0,CU_V1
!        INTEGER(KIND=8),POINTER  :: PL_DC,PL_ZC
      END TYPE
      TYPE(PSOLTYPE), SAVE ::PSOLDAT(10)
      DOUBLEPRECISION, POINTER :: DTDP
      DOUBLEPRECISION, POINTER :: DTMV
      DOUBLEPRECISION, POINTER :: DTAXPY
      DOUBLEPRECISION, POINTER :: DTVVP
      DOUBLEPRECISION, POINTER :: DTMISC
      END MODULE PSOLMODULE


      SUBROUTINE PSOL7AR(IN,MXITER,IGRID)
C     ******************************************************************
C     ALLOCATE STORAGE FOR PSOL ARRAYS AND READ PSOL DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND
      USE PSOLMODULE
      USE OMP_LIB 
C
      CHARACTER*200 LINE
      INTEGER IN,MXITER
      INTEGER         :: ntc, ntdp, ntmv
      DOUBLEPRECISION :: tserialc,  tompc,  ttc,  suc
      DOUBLEPRECISION :: tserialdp, tompdp, ttdp, sudp
      DOUBLEPRECISION :: tserialmv, tompmv, ttmv, sumv
C
C-------FUNCTIONS      
      DOUBLEPRECISION :: SPSOLDP
C     ------------------------------------------------------------------
      ALLOCATE( NMETH,ITER1C,NPC,NOPT,NTRD,NTRDV )
      ALLOCATE( NITERC,NNZC,NIAC,NIAPC,NIWC )
      ALLOCATE( NPOL,NEIG )
      ALLOCATE( HCLOSEPSOL, RCLOSEPSOL )
      ALLOCATE( PSOLTOTT, PSOLFMAT )
      ALLOCATE( PSOLPCUT, PSOLPCAT, PSOLDPT, PSOLMVT )
      ALLOCATE( PSOLAXPYT, PSOLVVPT, PSOLMISCT )
      ALLOCATE( PSOLGPUTT )

      ALLOCATE( IPSOLO, IPSOLI )
      ALLOCATE( GLSPOLY )
      ALLOCATE( DTDP, DTMV, DTAXPY, DTVVP, DTMISC )
      
      ALLOCATE( ISCL )

      ALLOCATE( NODEC(NCOL,NROW,NLAY) )
      NODESC = NCOL*NROW*NLAY
      NODEC = 0
      
      PSOLTOTT   = 0.0D0
      PSOLFMAT   = 0.0D0
      PSOLPCUT   = 0.0D0
      PSOLPCAT   = 0.0D0
      PSOLDPT    = 0.0D0
      PSOLMVT    = 0.0D0
      PSOLAXPYT  = 0.0D0
      PSOLVVPT   = 0.0D0
      PSOLMISCT  = 0.0D0
      PSOLGPUTT  = 0.0D0
      IPSOLO     = 0
      IPSOLI     = 0
      DTDP       = 0.0D0
      DTMV       = 0.0D0
      DTAXPY     = 0.0D0
      DTVVP      = 0.0D0
      DTMISC     = 0.0D0
      
      NPC        = 0
      ISCL       = 0
C
C-------PRINT A MESSAGE IDENTIFYING PSOL PACKAGE
      WRITE (IOUT,500)
  500 FORMAT (1X,/1X,'PSOL -- UNSTRUCTURED CONJUGATE-GRADIENT SOLUTION',
     &        ' PACKAGE, VERSION 7.01, 02/09/2012',
     &        /1X,8X,'INCLUDES CPU, CPU-OPENMP, AND GPU-CUDA SUPPORT')
C
C-------READ AND PRINT COMMENTS, MXITER,ITER1 AND NPCOND
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NMETH,R,IOUT,IN)
      IF ( NMETH.NE.1 ) THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXITER,R,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITER1C,R,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPC,R,IOUT,IN)
      END IF
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NOPT,R,IOUT,IN)
      IF ( NMETH.NE.1 ) THEN
          IF ( NPC.LT.0 ) THEN
            NPC  = ABS( NPC )
            ISCL = 1
          END IF
          IF ( NPC.LT.0 .OR. NPC.GT.4 ) THEN
              WRITE (IOUT,'(//,A)') 'PSOL7AR: NPC MUST BE >= 0 AND < 5'
              CALL USTOP('PSOL7AR: NPC MUST BE >= 0 AND < 5')
          END IF
          IF ( NPC.EQ.4 ) THEN
            ISCL = 1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,N,R,IOUT,IN)
            GLSPOLY%NDEGREE  = N
            IF ( N.LT.1 ) THEN
              WRITE (IOUT,'(//,A)') 
     2          'PSOL7AR: POLYNOMIAL DEGREE MUST BE > 0'
              CALL USTOP('PSOL7AR: POLYNOMIAL DEGREE MUST BE > 0')
            END IF
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,N,R,IOUT,IN)
            GLSPOLY%NLANSTEP = N
            GLSPOLY%IEIGCALC = 1
            IF ( N.EQ.-2 ) THEN
              N = ABS( N )
              GLSPOLY%IEIGCALC = 0
              GLSPOLY%NLANSTEP = 0
            END IF
            IF ( N.LT.0 ) THEN
              WRITE (IOUT,'(//,A)') 
     2          'PSOL7AR: NLANSTEP MUST BE > 0 OR = -2'
              CALL USTOP('PSOL7AR: POLYNOMIAL NLANSTEP MUST BE > 0 '
     2                   //'OR = -2')
            END IF
          END IF
      END IF
      IF ( NOPT.LT.1 .OR. NOPT.GT.3 ) THEN
          WRITE (IOUT,'(//,A)') 'PSOL7AR: NOPT MUST BE > 0 AND < 4'
          CALL USTOP('PSOL7AR: NOPT MUST BE > 0 AND < 4')
      END IF
      NTRD  = 1
      NTRDV = 1
      IF ( NOPT.EQ.2 ) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NTRD,R,-IOUT,IN)
        IF ( NTRD.LT.0 ) THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NTRDV,R,-IOUT,IN)
          IF ( NTRDV.LT.1 ) THEN
              WRITE (IOUT,'(//,A)') 'PSOL7AR: NTRDV MUST BE > 0'
              CALL USTOP('PSOL7AR: NTRDV MUST BE > 0')
          END IF
        END IF
      END IF
C
C-------READ HCLOSEPCG,RCLOSEPCG,RELAXPCG,NBPOL,IPRPCG,MUTPCG
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HCLOSEPSOL,IOUT,IN)
      IF ( HCLOSEPSOL.LE.0.0 ) THEN
          WRITE (IOUT,'(//,A)') 'PSOL7AR: HCLOSE MUST BE > 0.0'
          CALL USTOP('PSOL7AR: HCLOSE MUST BE > 0.0')
      END IF
      IF ( NMETH.NE.1 ) THEN
		CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,RCLOSEPSOL,IOUT,IN)
		IF ( RCLOSEPSOL.LE.0.0 ) THEN
			WRITE (IOUT,'(//,A)') 'PSOL7AR: RCLOSE MUST BE > 0.0'
			CALL USTOP('PSOL7AR: RCLOSE MUST BE > 0.0')
          END IF
	END IF
C
C-------PRINT MXITER,ITER1C,NPC,HCLOSEPCG,RCLOSEPCG,NPC,NOPT
C-------MUTPCG,DAMPPCG
	IF ( NMETH.NE.1 ) THEN
		WRITE (IOUT,505)
	    WRITE (IOUT,510) MXITER
	    WRITE (IOUT,515) ITER1C
		WRITE (IOUT,520) NPC
		WRITE (IOUT,525) RCLOSEPSOL
	ELSE
		WRITE (IOUT,530)
	END IF
  505	FORMAT (1X,/,18X,'SOLUTION BY THE CONJUGATE-GRADIENT METHOD',
     &               /,1X,75('-'))
  510 FORMAT (1X,1X,'MAXIMUM NUMBER OF CALLS TO PCG ROUTINE =',I9)
  515 FORMAT (1X,5X,'MAXIMUM ITERATIONS PER CALL TO PCG =',I9)
  520 FORMAT (1X,12X,'MATRIX PRECONDITIONING TYPE =',I9)
  525 FORMAT (1X,2X,'RESIDUAL CHANGE CRITERION FOR CLOSURE =',E15.5)
  530	FORMAT (1X,/,18X,'EXPLICIT SOLUTION WITH CONVERGENCE CONTROL',
     &               /,1X,75('-'))
      
C-------
  	WRITE (IOUT,540) HCLOSEPSOL
  540 FORMAT (1X,6X,'HEAD CHANGE CRITERION FOR CLOSURE =',E15.5)
C
      WRITE (IOUT,545) NPC, NOPT
  545 FORMAT (/1X,75('-'),/,
     &        19X,' MATRIX PRECONDITIONING TYPE :',I5,/,
     &        19X,'   NONE      : NPC = 0',/,
     &        19X,'   JACOBI    : NPC = 1',/,
     &        19X,'   ILU0      : NPC = 2',/,
     &        19X,'   MILU0     : NPC = 3',/,
     &        19X,'   GLS POLY. : NPC = 4',/,
     &        19X,' HARDWARE SOLUTION OPTION    :',I5,/,
     &        19X,'   CPU       : NOPT = 1',/,
     &        19X,'   CPU - OMP : NOPT = 2',/,
     &        19X,'   GPU - CUDA: NOPT = 3',/,
     &        1X,75('-'))
C
C-------INITIALIZE NITERC  
      NITERC = 0
C
C-------CALCULATE NUMBER OF NON-ZERO ENTRIES IN MODEL GRID
      NNZC  = 0
      NIAC  = 0
      NIAPC = 0
      NIWC  = 0
      NPOL  = 0
      NEIG  = 0
      IC    = 0
      ieq   = 0
      NRC = NROW * NCOL
      DO K = 1, NLAY
        DO I = 1, NROW
          DO J = 1, NCOL
            IC = IC + 1
            IF ( IBOUND(J,I,K).GT.0 ) THEN
              NIAC = NIAC + 1
              NNZC = NNZC + 1
              ieq  = ieq  + 1
              NODEC(J,I,K) = ieq
C               TOP FACE
              IF ( K.GT.1 ) THEN
                IF ( IBOUND(J,I,K-1).GT.0 ) NNZC = NNZC + 1
              END IF
C               UPPER FACE
              IF ( I.GT.1 ) THEN
                IF ( IBOUND(J,I-1,K).GT.0 ) NNZC = NNZC + 1
              END IF
C               LEFT FACE
              IF ( J.GT.1 ) THEN
                IF ( IBOUND(J-1,I,K).GT.0 ) NNZC = NNZC + 1
              END IF
C               RIGHT FACE
              IF ( J.LT.NCOL ) THEN
                IF ( IBOUND(J+1,I,K).GT.0 ) NNZC = NNZC + 1
              END IF
C               LOWER FACE
              IF ( I.LT.NROW ) THEN
                IF ( IBOUND(J,I+1,K).GT.0 ) NNZC = NNZC + 1
              END IF
C               BOTTOM FACE
              IF ( K.LT.NLAY ) THEN
                IF ( IBOUND(J,I,K+1).GT.0 ) NNZC = NNZC + 1
              END IF
            END IF
          END DO
        END DO
      END DO
C
C-------ALLOCATE AND INITIALIZE COMPRESSED ROW STORAGE VECTORS
C       COEFFICIENT MATRIX AND PRECONDITIONER MATRIX
      ALLOCATE(AC(NNZC))
      NIAPC = NNZC
      IF ( NPC.EQ.0 ) THEN
        NIAPC = 1
      ELSE IF ( NPC.EQ.1 ) THEN
        NIAPC = NIAC
      ELSE IF ( NPC.EQ.4 ) THEN
        NIAPC = 1
      END IF
      ALLOCATE(APC(NIAPC))
      ALLOCATE(IAC(NIAC+1),JAC(NNZC),IUC(NIAC),IXMAP(NIAC))
C       ALLOCATE WORKING VECTORS FOR PSOL SOLVER      
      ALLOCATE(BC(NIAC),XC(NIAC))
      ALLOCATE(DC(NIAC),PC(NIAC))
      ALLOCATE(QC(NIAC),ZC(NIAC))
C       INITIALIZE PCG WORKING ARRAYS
      DO n = 1, NNZC
        AC(n)  = 0.0D0
        JAC(n) = 0
      END DO
      DO n = 1, NIAPC
        APC(n) = 0.0D0
      END DO
      DO n = 1, NIAC+1
        IAC(n) = 0
      END DO
      DO n = 1, NIAC
        IUC(n)   = 0
        IXMAP(n) = 0
        BC(n)    = 0.0D0
        XC(n)    = 0.0D0
C         WORKING ARRAYS
        DC(n)    = 0.0D0
        PC(n)    = 0.0D0
        QC(n)    = 0.0D0
        ZC(n)    = 0.0D0
      END DO
C       ALLOCATE SPACE FOR ILU0 AND MILU0 NON-ZERO ROW ENTRY VECTOR
      IF ( NPC.EQ.2 .OR. NPC.EQ.3 ) THEN
        NIWC = NIAC
      ELSE
        NIWC = 1
      END IF
      ALLOCATE(IWC(NIWC))
C       INITIALIZE ILU0 AND MILU0 PRECONDITIONER WORKING VECTOR      
      DO n = 1, NIWC
        IWC(n)   = 0
      END DO
C       ALLOCATE DIAGONAL SCALING VECTOR
      ALLOCATE( SCL(NIAC)  )
      ALLOCATE( SCLI(NIAC) )
C       INITIALIZE DIAGONAL SCALING VECTOR
      DO n = 1, NIAC
        SCL(n)               = 1.0D0
        SCLI(n)              = 1.0D0
      END DO
C       ALLOCATE WORKING VECTORS FOR POLYNOMIAL PRECONDITIONER
      IF ( NPC.EQ.4 ) THEN
        NPOL = NIAC
        IF ( GLSPOLY%NLANSTEP.GT.NIAC ) THEN
          GLSPOLY%NLANSTEP = NIAC
        END IF
!        m    = 512 / 8
!        GLSPOLY%NLAN2 = ( NIAC + m - 1 ) / ( m * m )
        GLSPOLY%NLAN2 = NIAC
        NEIG = NPOL
        IF ( GLSPOLY%IEIGCALC.EQ.0 ) THEN
          NEIG             = 1
          GLSPOLY%NLANSTEP = 0
          GLSPOLY%NLAN2    = 1
        END IF
      ELSE
        NPOL             = 1
        NEIG             = 1
        GLSPOLY%NDEGREE  = 1
        GLSPOLY%NLANSTEP = 0
        GLSPOLY%NLAN2    = 1
      END IF
      nlansize = ( GLSPOLY%NLANSTEP + 1 ) * GLSPOLY%NLAN2
      ALLOCATE( GLSPOLY%D_LANCZOS(nlansize)                 )
      ALLOCATE( GLSPOLY%ALPHA(GLSPOLY%NDEGREE)              )
      ALLOCATE( GLSPOLY%BETA(GLSPOLY%NDEGREE+1)             )
      ALLOCATE( GLSPOLY%GAMMA(GLSPOLY%NDEGREE+1)            )
      ALLOCATE( GLSPOLY%D_V1(NPOL)                          )
      ALLOCATE( GLSPOLY%D_V0(NPOL)                          )
      ALLOCATE( GLSPOLY%D_V(NPOL)                           )
      ALLOCATE( GLSPOLY%D_E(NEIG)                           )
      ALLOCATE( GLSPOLY%INTV(2)                             )
      ALLOCATE( GLSPOLY%P0(2)                               )
      ALLOCATE( GLSPOLY%PPOL(GLSPOLY%NDEGREE+3)             )
      ALLOCATE( GLSPOLY%APPOL(GLSPOLY%NDEGREE+3)            )
      ALLOCATE( GLSPOLY%QPOL(GLSPOLY%NDEGREE+2)             )
C       INITIALIZE POLYNOMIAL PRECONDITIONER WORKING ARRAYS
      DO n = 1, nlansize
        GLSPOLY%D_LANCZOS(n) = 0.0D0
      END DO
      DO n = 1, NPOL
        GLSPOLY%D_V1(n)      = 0.0D0
        GLSPOLY%D_V0(n)      = 0.0D0
        GLSPOLY%D_V(n)       = 0.0D0
      END DO
      DO n = 1, NEIG
        GLSPOLY%D_E(n)       = 0.0D0
      END DO
      DO n = 1, 2
        IF ( GLSPOLY%IEIGCALC.NE.0 ) THEN
          GLSPOLY%INTV(n)      = 0.0D0
        ELSE
          IF ( n.EQ.1 ) THEN
            GLSPOLY%INTV(n) = 0.0D+00
          ELSE
            GLSPOLY%INTV(n) = 2.0D+00
          END IF
        END IF
        GLSPOLY%P0(n)       = 0.0D0
      END DO
      DO n = 1, GLSPOLY%NDEGREE+3
        IF ( n.LE.GLSPOLY%NDEGREE ) THEN
          GLSPOLY%ALPHA(n)   = 0.0D0
        END IF
        IF ( n.LE.GLSPOLY%NDEGREE+1 ) THEN
          GLSPOLY%BETA(n)    = 0.0D0
          GLSPOLY%GAMMA(n)   = 0.0D0
        END IF
        IF ( n.LE.GLSPOLY%NDEGREE+2 ) THEN
          GLSPOLY%QPOL(n)    = 0.0D0
        END IF
        GLSPOLY%PPOL(n)      = 0.0D0
        GLSPOLY%APPOL(n)     = 0.0D0
      END DO
C
C-------PRINT POLYNOMIAL PRECONDITIONER INFORMATION
      IF ( NPC.EQ.4 ) THEN
        WRITE (IOUT,550) GLSPOLY%NDEGREE
        IF ( GLSPOLY%IEIGCALC.NE.0 ) THEN
          WRITE (IOUT,555) GLSPOLY%NLANSTEP
        ELSE
          WRITE (IOUT,560)
        END IF
        WRITE (IOUT,565)
      END IF  
  550 FORMAT (/1X,75('-'),/,
     &        13X,' GENERAL LEAST-SQUARES POLYNOMIAL PRECONDITIONER',/,
     &        13X,'   DEGREE POLYNOMIAL = ',I5,/,
     &        13X,'   DIAGONAL RESCALING APPLIED')
  555 FORMAT (13X,'   LANCZOS STEPS     = ',I5,/,
     &        13X,'     MAY HAVE BEEN REDUCED TO PROBLEM SIZE')
  560 FORMAT (13X,'   MAX. AND MIN. EIGENVALUES ASSUMED TO BE',/,
     &        13X,'     2.0 AND 0.0, RESPECTIVELY.')
  565 FORMAT (1X,75('-'))
C
C-------FILL IA AND JA
      IND = 0
      IC  = 0
      ieq = 0
      DO K = 1, NLAY
        DO I = 1, NROW
          DO J = 1, NCOL
            IND = IND + 1
            IF ( IBOUND(J,I,K).GT.0 ) THEN
              IC = IC + 1
              ieq = ieq + 1
              IAC(ieq) = IC
              IXMAP(ieq) = IND
              JAC(IC) = NODEC(J,I,K)
C               TOP FACE
              IF ( K.GT.1 ) THEN
                IF ( IBOUND(J,I,K-1).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J,I,K-1)
                END IF
              END IF
C               UPPER FACE
              IF ( I.GT.1 ) THEN
                IF ( IBOUND(J,I-1,K).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J,I-1,K)
                END IF
              END IF
C               LEFT FACE
              IF ( J.GT.1 ) THEN
                IF ( IBOUND(J-1,I,K).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC)  = NODEC(J-1,I,K)
                END IF
              END IF
C               RIGHT FACE
              IF ( J.LT.NCOL ) THEN
                IF ( IBOUND(J+1,I,K).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J+1,I,K)
                END IF
              END IF
C               LOWER FACE
              IF ( I.LT.NROW ) THEN
                IF ( IBOUND(J,I+1,K).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J,I+1,K)
                END IF
              END IF
C               BOTTOM FACE
              IF ( K.LT.NLAY ) THEN
                IF ( IBOUND(J,I,K+1).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J,I,K+1)
                END IF
              END IF
              
            END IF
          END DO
        END DO
      END DO
C
C-------SET LAST POSITION IN IAC
      IAC(NIAC+1) = IC + 1
C
C-------SET IUC
      DO N = 1, NIAC
        DO I = IAC(N), IAC(N+1)-1
          IF ( JAC(I).GT.N .AND. IUC(N).EQ.0 ) IUC(N) = I
        END DO
C         SET IUC TO FIRST ELEMENT OF THE NEXT EQUATION IF DIAGONAL
C         IS LARGEST NODE NUMBER IN AC FOR CURRENT EQUATION              
        IF ( IUC(N).EQ.0 ) IUC(N) = IAC(N+1)
      END DO
C
C-------TIME SERIAL AND OPEN MP VECTOR OPERATIONS
      TSTOMP: IF ( NOPT.EQ.2 ) THEN
C         DETERMINE IF SPECIFIED NUMBER OF THREADS 
C         EXCEEDS THE MAXIMUM NUMBER OF THREADS AVAILABLE
C         MINUS ONE. IF SO, REDUCE TO THE MAXIMUM.
        NTRDMAX = OMP_GET_MAX_THREADS() - 1
        IF ( NTRD.NE.0 ) THEN
          IF ( ABS( NTRD ).GT.NTRDMAX ) THEN
            NTRD = ( NTRD / ABS( NTRD ) ) * NTRDMAX
          END IF
          !CALL OMP_SET_NUM_THREADS( ABS( NTRD ) )
        ELSE
          NTRD = NTRDMAX
        END IF
C         
C         TIME OPEN MP OPERATIONS IF A POSITIVE NTRD VALUE
C         IS SPECIFIED (WHICH MEANS NTRDV IS NOT SPECIFIED)
        TIMEOMP: IF ( NTRD.GT.0 ) THEN
          NTRDV = NTRD
          nop   = 100
C           VECTOR COPY OPERATIONS
          tserialc  = 0.0D0
          tompc     = 0.0D0
C           SERIAL
          DTMISC = 0.0D0
          DO i = 1, nop
            CALL SPSOLDCOPY(1,NTRDV,NIAC,DC,ZC)
          END DO
          tserialc = DTMISC
C           OPENMP
          ntc    = 1
          tompc  = tserialc
          DO np = 2, NTRDMAX
            !CALL OMP_SET_NUM_THREADS( np )
            DTMISC = 0.0D0
            ttc    = 0.0D0
            DO i = 1, nop
              CALL SPSOLDCOPY(NOPT,np,NIAC,DC,ZC)
            END DO
            ttc    = DTMISC
            IF ( ttc.LT.tserialc .AND.
     2           ttc.LT.tompc ) THEN
              ntc    = np
              tompc  = ttc
            END IF       
          END DO
          tserialc  = tserialc  * 1.0D+3 / REAL( nop, 8 )
          tompc     = tompc     * 1.0D+3 / REAL( nop, 8 )
          suc       = 0.0D0
          IF ( tompc.GT.0.0D0 ) suc = tserialc / tompc
C           DOT PRODUCT        
          tserialdp = 0.0D0
          tompdp    = 0.0D0
C           SERIAL
          DTDP = 0.0D0
          DO i = 1, nop
            v = SPSOLDP(1,NTRDV,NIAC,DC,ZC)
          END DO
          tserialdp = DTDP
C           OPENMP
          ntdp   = 1
          tompdp = tserialdp
          DO np = 2, NTRDMAX
           !CALL OMP_SET_NUM_THREADS( np )
           DTDP = 0.0D0
            ttdp = 0.0D0
            DO i = 1, nop
              v = SPSOLDP(NOPT,np,NIAC,DC,ZC)
            END DO
            ttdp    = DTDP
            IF ( ttdp.LT.tserialdp .AND.
     2           ttdp.LT.tompdp ) THEN
              ntdp   = np
              tompdp = ttdp
            END IF       
          END DO
          tserialdp = tserialdp * 1.0D+3 / REAL( nop, 8 )
          tompdp    = tompdp    * 1.0D+3 / REAL( nop, 8 )
          sudp      = 0.0D0
          IF ( tompdp.GT.0.0D0 ) sudp = tserialdp / tompdp
C           SPARSE MATRIX VECTOR PRODUCT        
          tserialmv = 0.0D0
          tompmv    = 0.0D0
C           SERIAL
          DTMV = 0.0D0
          DO i = 1, nop
            CALL SPSOLMV(1,NTRD,NNZC,NIAC,AC,DC,ZC,IAC,JAC)
          END DO
          tserialmv = DTMV
C           OPENMP
          ntmv   = 1
          tompmv = tserialmv
          DO np = 2, NTRDMAX
            !CALL OMP_SET_NUM_THREADS( np )
            DTMV = 0.0D0
            ttmv = 0.0D0
              DO i = 1, nop
                CALL SPSOLMV(NOPT,np,NNZC,NIAC,AC,DC,ZC,IAC,JAC)
              END DO
            ttmv    = DTMV
            IF ( ttmv.LT.tserialmv .AND.
     2           ttmv.LT.tompmv ) THEN
              ntmv   = np
              tompmv = ttmv
            END IF       
          END DO
          tserialmv = tserialmv * 1.0D+3 / REAL( nop, 8 )
          tompmv    = tompmv    * 1.0D+3 / REAL( nop, 8 )
          sumv      = 0.0D0
          IF ( tompmv.GT.0.0D0 ) sumv = tserialmv / tompmv
C           USE OPENMP THREADS THAT RESULT IN THE SMALLEST
C           EXECUTION TIMES
          NTRD  = ntmv
          NTRDV = ntdp
C
C         WRITE SUMMARY OF THE AVERAGE TIME TO COMPLETE OPENMP OPERATIONS
          WRITE (IOUT,570) nop,  
     2                     tserialc,  tompc,  suc,
     3                     tserialdp, tompdp, sudp,
     4                     tserialmv, tompmv, sumv,
     5                     NIAC, NNZC
        ELSE
          NTRD = ABS( NTRD )
        END IF TIMEOMP
C
C---------WRITE SUMMARY OF THREADS BEING USED FOR OPEN MP 
C         SMV AND VECTOR OPERATIONS
        WRITE (IOUT,575) NTRDV, NTRD
C
C---------SET MAXIMUM NUMBER OF THREADS
        NTRDMAX = MAX( NTRD, NTRDV )
        !CALL OMP_SET_NUM_THREADS( NTRDMAX )
      END IF TSTOMP
C      
  570 FORMAT (/1X,75('-'),
     &        /34X,'OPEN MP',
     &        /23X,'AVERAGE TIME FOR',1X,I3,1X,'OPERATIONS',
     &        /47X,'TIME',
     &        /12X,'OPERATION',22X,'MILLISECONDS',6X,'SPEEDUP',
     &        /1X,75('-')
     &        /12X,' SERIAL VECTOR COPY TIME      :',G15.7,
     &        /12X,' OPENMP VECTOR COPY TIME      :',G15.7,1X,F10.3,
     &        /12X,' SERIAL DOT PRODUCT TIME      :',G15.7,
     &        /12X,' OPENMP DOT PRODUCT TIME      :',G15.7,1X,F10.3,
     &        /12X,' SERIAL SMV PRODUCT TIME      :',G15.7,
     &        /12X,' OPENMP SMV PRODUCT TIME      :',G15.7,1X,F10.3,
     &        /1X,75('.'),
     &        /12X,' NUMBER OF ACTIVE CELLS       :',I10,
     &        /12X,' NUMBER OF NON-ZERO ENTRIES   :',I10,
     &        /1X,75('-'))
  575 FORMAT (/1X,75('-'),
     &        /19X,' NUMBER OF OPENMP   V THREADS :',I5,
     &        /19X,' NUMBER OF OPENMP SMV THREADS :',I5,
     &        /1X,75('-'))

!C
!C-------ALLOCATE GPU POINTERS
!      ALLOCATE(CU_HDL,CU_STAT,CU_DES)
!      ALLOCATE(CU_IAC,CU_JAC)
!      ALLOCATE(CU_AC,CU_APC,CU_XC)
!      ALLOCATE(CU_DC,CU_ZC,CU_PC,CU_QC)
!      ALLOCATE(CU_SCL,CU_SCLI,CU_V,CU_V0,CU_V1)
!      ALLOCATE(PL_DC,PL_ZC)
!C
!C-------INITIALIZE GPU MEMORY
!      IF ( NOPT.EQ.3 ) THEN
!C         INITIALIZE MEMORY ON THE GPU 
!        CALL PSOLC7_INIT(CU_HDL,CU_STAT,CU_DES,
!     &                   NNZC,NIAC,NIAPC,
!     &                   NPC,GLSPOLY%NDEGREE,
!     &                   CU_IAC,IAC,CU_JAC,JAC,
!     &                   CU_AC,CU_APC,CU_XC,
!     &                   CU_DC,CU_ZC,CU_PC,CU_QC,
!     &                   CU_SCL,CU_SCLI,CU_V,CU_V0,CU_V1,
!     &                   PL_DC,PL_ZC)      
!      END IF
!C
C-------SET POINTERS FOR GRID
      CALL PSOL7PSV(IGRID)
C
C-------RETURN
      RETURN
      END SUBROUTINE PSOL7AR
      
      SUBROUTINE PSOL7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,
     &                 ICNVG,KSTP,KPER,MXITER,KITER,
     &                 NCOL,NROW,NLAY,NODES,HNOFLO,IOUT,
     &                 NMETH,NPC,NOPT,NTRD,NTRDV,NITER,ITER1,NNZC,NIAC,
     &                 NIAPC,NIWC,NPOL,NEIG,
     &                 HCLOSE,RCLOSE,
     &                 PSOLTOTT,PSOLFMAT,
     &                 PSOLPCUT,PSOLPCAT,PSOLDPT,PSOLMVT,
     &                 PSOLAXPYT,PSOLVVPT,PSOLMISCT,PSOLGPUTT,
     &                 IPSOLO,IPSOLI,
     &                 NODEC,BC,XC,AC,APC,IAC,JAC,IUC,IXMAP,IWC,
     &                 DC,ZC,PC,QC,ISCL,SCL,SCLI,GLSPOLY) !,
!     &                 CU_HDL,CU_STAT,CU_DES,CU_JAC,CU_IAC,
!     &                 CU_AC,CU_APC,CU_XC,
!     &                 CU_DC,CU_ZC,CU_PC,CU_QC,
!     &                 CU_SCL,CU_SCLI,CU_V,CU_V0,CU_V1,
!     &                 PL_DC,PL_ZC)
!C
C     ******************************************************************
C     SOLUTION BY THE CONJUGATE GRADIENT METHOD -
C                                          UP TO ITER1 ITERATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PSOLMODULE, ONLY: TGLSPOLY, 
     2                      DTDP, DTMV, DTAXPY, DTVVP, DTMISC
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      DOUBLEPRECISION, DIMENSION(NODES), INTENT(INOUT) :: HNEW
      INTEGER, DIMENSION(NODES), INTENT(INOUT)         :: IBOUND
      REAL, DIMENSION(NODES), INTENT(IN)               :: CR
      REAL, DIMENSION(NODES), INTENT(IN)               :: CC
      REAL, DIMENSION(NODES), INTENT(IN)               :: CV
      REAL, DIMENSION(NODES), INTENT(IN)               :: HCOF
      REAL, DIMENSION(NODES), INTENT(IN)               :: RHS
      INTEGER, INTENT(INOUT)                           :: ICNVG
      INTEGER, INTENT(IN)                              :: KSTP
      INTEGER, INTENT(IN)                              :: KPER
      INTEGER, INTENT(IN)                              :: MXITER
      INTEGER, INTENT(IN)                              :: KITER
      INTEGER, INTENT(IN)                              :: NCOL
      INTEGER, INTENT(IN)                              :: NROW
      INTEGER, INTENT(IN)                              :: NLAY
      INTEGER, INTENT(IN)                              :: NODES
      REAL, INTENT(IN)                                 :: HNOFLO
      INTEGER, INTENT(IN)                              :: IOUT
      INTEGER, INTENT(IN)                              :: NMETH
      INTEGER, INTENT(IN)                              :: NPC
      INTEGER, INTENT(IN)                              :: NOPT
      INTEGER, INTENT(IN)                              :: NTRD
      INTEGER, INTENT(IN)                              :: NTRDV
      INTEGER, INTENT(IN)                              :: NITER
      INTEGER, INTENT(INOUT)                           :: ITER1
      INTEGER, INTENT(IN)                              :: NNZC
      INTEGER, INTENT(IN)                              :: NIAC
      INTEGER, INTENT(IN)                              :: NIAPC
      INTEGER, INTENT(IN)                              :: NIWC
      INTEGER, INTENT(IN)                              :: NPOL
      INTEGER, INTENT(IN)                              :: NEIG
      REAL, INTENT(IN)                                 :: HCLOSE
      REAL, INTENT(IN)                                 :: RCLOSE
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLTOTT
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLFMAT
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLPCUT
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLPCAT
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLDPT
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLMVT
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLAXPYT
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLGPUTT
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLVVPT
      DOUBLEPRECISION, INTENT(INOUT)                   :: PSOLMISCT
      INTEGER, INTENT(INOUT)                           :: IPSOLO
      INTEGER, INTENT(INOUT)                           :: IPSOLI
      INTEGER, DIMENSION(NCOL,NROW,NLAY)               :: NODEC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)  :: BC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)  :: XC
      DOUBLEPRECISION, DIMENSION(NNZC), INTENT(INOUT)  :: AC
      DOUBLEPRECISION, DIMENSION(NIAPC), INTENT(INOUT) :: APC
      INTEGER, DIMENSION(NIAC+1), INTENT(IN)           :: IAC
      INTEGER, DIMENSION(NNZC), INTENT(IN)             :: JAC
      INTEGER, DIMENSION(NIAC), INTENT(IN)             :: IUC
      INTEGER, DIMENSION(NIAC), INTENT(IN)             :: IXMAP
C       WORKING ARRAYS
      INTEGER, DIMENSION(NIWC), INTENT(INOUT)          :: IWC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)  :: DC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)  :: PC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)  :: QC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)  :: ZC
C       DIAGONAL SCALING VECTOR
      INTEGER, INTENT(IN) :: ISCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)  :: SCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)  :: SCLI
C       POLYNOMIAL PRECONDITIONER
      TYPE (TGLSPOLY), INTENT(INOUT)                   :: GLSPOLY
!C     GPU VARIABLES      
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_HDL
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_STAT
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_DES
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_JAC
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_IAC
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_AC
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_APC      
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_XC
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_DC
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_ZC
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_PC
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_QC
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_SCL    
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_SCLI    
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_V      
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_V0      
!      INTEGER(KIND=8),                    INTENT(IN)   :: CU_V1
!      INTEGER(KIND=8),                    INTENT(IN)   :: PL_DC
!      INTEGER(KIND=8),                    INTENT(IN)   :: PL_ZC
!C     + + + LOCAL DEFINITIONS + + +
      DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
      DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
      INTEGER :: i, j, k, n
      INTEGER :: ii
      INTEGER :: iapos
      INTEGER :: ieq
      INTEGER :: nrc
      INTEGER :: iinactive
      INTEGER :: iadiag
      INTEGER :: nrn, nrl, ncn, ncl, nln, nll
      INTEGER :: ncf, ncd, nrb, nrh, nls, nlz
      INTEGER :: ncount
      INTEGER :: iiter
      INTEGER :: irc
      INTEGER :: iicnvg
      INTEGER :: nx, nr

      DOUBLE PRECISION :: dhclose, drclose
      DOUBLE PRECISION :: rrhs, hhcof, rsq, fbar, fmax
      DOUBLE PRECISION :: z, b, d, e, f, h, s
      DOUBLE PRECISION :: zhnew, bhnew, dhnew, fhnew, hhnew, shnew

      DOUBLEPRECISION :: t, ta
      DOUBLEPRECISION :: deltax
      DOUBLEPRECISION :: rmax
      DOUBLEPRECISION :: alpha, beta
      DOUBLEPRECISION :: rho, rho0
      
      DOUBLEPRECISION :: ttf, tt1, tt2
      DOUBLEPRECISION :: tpcu1, tpcu2
      DOUBLEPRECISION :: tpca1, tpca2
      DOUBLEPRECISION :: tdp1, tdp2
      DOUBLEPRECISION :: tmv1, tmv2
      
C       GPU LOCALS DEFINITIONS
      DOUBLEPRECISION :: cu_rho, cu_rho0        

C       FUNCTIONS      
      DOUBLEPRECISION :: SPSOLDP
C
C-------CODE
C
C-------START PSOL TOTAL TIMER
      CALL SPSOLTIMER(0,tt1,PSOLTOTT)
C
C       SET TEMPORARY TIMER VARIABLES TO CURRENT VALUE OF 
C       DUMMY ARGUMENT TIMER VARIABLES
      DTDP   = PSOLDPT
      DTMV   = PSOLMVT
      DTAXPY = PSOLAXPYT
      DTVVP  = PSOLVVPT
      DTMISC = PSOLMISCT
C
C-------START FORMULATE TIMER
      CALL SPSOLTIMER(0,ttf,PSOLFMAT)
C
C       SET LOCAL VARIABLES
      nrc     = NROW * NCOL
      iapos   = 0
      ieq     = 0
      fmax    = dzero
      ncount  = 0
      dhclose = REAL( hclose, 8 )
      drclose = REAL( rclose, 8 )
      DO n = 1, NNZC
        AC(n) = DZERO
      END DO
C
C-------LOOP THROUGH ALL NODES IN THE GRID AND SET UP MATRIX EQUATIONS.
C-------NOTE THAT THE FORMULATION OF THESE EQUATIONS IS OPPOSITE IN SIGN
C-------FROM WHAT IS GIVEN IN THE MODFLOW MANUAL SO THAT THE DIAGONAL
C-------AND RHS ARE BOTH POSITIVE (LHS AND RHS ARE MULTIPLIED BY -1)
C-------THIS LOOP STRUCTURE AND INDEXING IS IDENTICAL TO THAT OF PCG2 
C-------AND IS BLATANTLY COPIED FROM HILL, 1990.
      LFILL: DO k = 1, NLAY
        RFILL: DO i = 1, NROW
          CFILL: DO j = 1, NCOL
C
C---------------CALCULATE 1 DIMENSIONAL SUBSCRIPT OF CURRENT CELL AND
C---------------INITIALIZE MATRIX COEFFICIENTS TO ZERO. CHECK IF CELL IS ACTIVE 
C---------------SKIP COEFFICIENT CALCULATIONS IF CELL IS INACTIVE
            n = j + (i-1) * NCOL + (k-1) * nrc
            e = dzero
            z = dzero
            b = dzero
            d = dzero
            f = dzero
            h = dzero
            s = dzero
            iinactive = 1
            IF( IBOUND(n).GT.0 ) THEN
              iinactive = 0
              ieq       = ieq + 1
C
C---------------CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR LOCATING THE 6
C---------------SURROUNDING CELLS
              nrn = n + NCOL
              nrl = n - NCOL
              ncn = n + 1
              ncl = n - 1
              nln = n + nrc
              nll = n - nrc
C
C---------------CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR CONDUCTANCE TO THE 6
C---------------SURROUNDING CELLS.
              ncf = n
              ncd = n - 1
              nrb = n - NCOL
              nrh = n
              nls = n
              nlz = n - nrc
C
C---------------STORE DOUBLE PRECISION VALUE OF RHS FOR CALCULATION OF RESIDUALS
              rrhs    =  RHS(n)
              BC(ieq) = -rrhs
C
C---------------GET CONDUCTANCES TO NEIGHBORING CELLS.  
C---------------ACCUMULATE CONTRIBUTIONS TO DIAGONAL COEFFICIENT. IF NEIGHBOR IS 
C---------------CONSTANT HEAD, MODIFY RHS AND SET OFF-DIAGONAL COEFFICIENT TO 0
C
C
!              iapos  = iapos + 1
              iadiag = IAC(ieq)
              iapos  = iadiag
C
C               TOP FACE
C---------------NEIGHBOR IS 1 LAYER BEHIND
              zhnew = dzero
              IF ( k.NE.1 ) THEN
                z = CV(nlz)
                e = e + z 
                zhnew = z*(HNEW(nll) - HNEW(n))
                IF ( IBOUND(nll).GT.0 ) THEN
                  iapos = iapos + 1
                  AC(iapos) = -z
                END IF
                IF( IBOUND(nll).LT.0 ) THEN
                  BC(ieq) = BC(ieq) + z*HNEW(nll) 
                  z = DZERO
                END IF
              END IF
C
C               UPPER FACE
C---------------NEIGHBOR IS 1 ROW BACK
              bhnew = dzero
              IF ( i.NE.1 ) THEN
                b = CC(nrb)
                e = e + b
                bhnew = b*(HNEW(nrl) - HNEW(n))
                IF( IBOUND(nrl).GT.0 ) THEN
                  iapos = iapos + 1
                  AC(iapos) = -b
                END IF 
                IF( IBOUND(NRL).LT.0 ) THEN
                  BC(ieq) = BC(ieq) + b*HNEW(nrl) 
                  b = dzero
                END IF 
              END IF
C
C               LEFT FACE
C---------------NEIGHBOR IS 1 COLUMN BACK
              dhnew = dzero
              IF ( j.NE.1 ) THEN
                d = CR(ncd)
                e = e + d
                dhnew = d*(HNEW(ncl) - HNEW(n))
                IF( IBOUND(ncl).GT.0 ) THEN
                  iapos = iapos + 1
                  AC(iapos) = -d
                END IF
                IF( IBOUND(ncl).LT.0 ) THEN
                  BC(ieq) = BC(ieq) + d*HNEW(ncl) 
                  d = dzero
                END IF 
              END IF
C
C               RIGHT FACE
C---------------NEIGHBOR IS 1 COLUMN AHEAD
              fhnew = dzero
              IF ( j.NE.NCOL ) THEN
                f = CR(ncf)
                e = e + f
                fhnew = f*(HNEW(ncn) - HNEW(n))
                IF( IBOUND(ncn).GT.0 ) THEN
                  iapos = iapos + 1
                  AC(iapos) = -f
                END IF
                IF( IBOUND(ncn).LT.0 ) THEN
                  BC(ieq) = BC(ieq) + f*HNEW(ncn) 
                  f = dzero 
                END IF
              END IF
C
C               LOWER FACE
C---------------NEIGHBOR IS 1 ROW AHEAD
              hhnew = dzero
              IF ( i.NE.NROW ) THEN
                h = CC(nrh)
                e = e + h
                hhnew = h*(HNEW(nrn) - HNEW(n))
                IF( IBOUND(nrn).GT.0 ) THEN
                  iapos = iapos + 1
                  AC(iapos) = -h
                END IF
                IF( IBOUND(nrn).LT.0 ) THEN
                  BC(ieq) = BC(ieq) + h*HNEW(nrn) 
                  h = dzero
                END IF 
              END IF
C
C               BOTTOM FACE
C---------------NEIGHBOR IS 1 LAYER AHEAD
              shnew = dzero
              IF ( k.NE.NLAY ) THEN
                s = CV(nls)
                e = e + s
                shnew = s*(HNEW(nln) - HNEW(n))
                IF( IBOUND(nln).GT.0 ) THEN
                  iapos = iapos + 1
                  AC(iapos) = -s
                END IF
                IF( IBOUND(nln).LT.0 ) THEN
                  BC(ieq) = BC(ieq) + s*HNEW(nln) 
                  s = dzero
                END IF
              END IF
C    
C---------------CHECK IF SURROUNDING CELLS ARE ACTIVE (E > 0).  IF SO, CALCULATE 
C---------------L2 NORM.  ACCUMULATE THE AVERAGE ABSOLUTE VALUE  OF THE RHS 
C---------------VECTOR FOR ALL ACTIVE CELLS.  THIS IS USED TO SCALE THE THE 
C---------------CLOSURE CRITERIA. 
C---------------IF SURROUNDING CELLS ARE INACTIVE BUT CURRENT CELL IS ACTIVE,
C---------------SET HNEW TO HNOFLO, IBOUND TO 0, AND CHANGE INACTIVE FLAG TO 1
              IF ( e.GT.dzero ) THEN
                hhcof = HNEW(n)*HCOF(n)
                rsq = rsq + (rrhs - zhnew - bhnew - dhnew - hhcof - 
     &             fhnew - hhnew - shnew)**2
                e = e - HCOF(n)
                fbar = fbar + ABS( BC(ieq) )
                ncount = ncount + 1
              ELSE
                HNEW(n)   = HNOFLO
                IBOUND(n) = 0
                iinactive = 1
C                 IF INACTIVE OR CONSTANT HEAD, SET DIAGONAL TO 1.0, AND ADJUST RHS ACCORDINGLY.  
                e = done
                BC(ieq) = HNEW(n)
              END IF
C
C-------------FIND THE MAXIMUM VALUE OF THE RHS VECTOR FOR ALL CELLS (ACTIVE
C-------------AND INACTIVE) FOR CLOSURE SCALING USED BY THE UNSTRUCTURED PCG SOLVER
              fmax = MAX( fmax, ABS( BC(ieq) ) )
C
C---------------STORE THE COEFFICENTS OF THE DIAGONAL IN A
              AC(iadiag) = e
C---------------STORE INITIAL GUESS OF HEADS
              XC(ieq) = HNEW(n)     
C
C-------------END IBOUND(N) .GT. 0
            END IF
          END DO CFILL
        END DO RFILL
      END DO LFILL
C
C-------END FORMULATE TIMER
      CALL SPSOLTIMER(1,ttf,PSOLFMAT)
C
C-------UPDATE PSOL OUTER ITERATION COUNTER
      IPSOLO = IPSOLO + 1
C
C-------UPDATE PRECONDITIONER
      CALL SPSOLTIMER(0,tpcu1,PSOLPCUT)
C       SCALE MATRIX FOR POLYNOMIAL PRECONDITIONER
!      IF ( NPC.EQ.4 ) THEN
      IF ( ISCL.NE.0 ) THEN
        CALL SPSOLSCL(1,NNZC,NIAC,AC,XC,BC,SCL,SCLI,IAC,JAC)
      END IF
      CALL SPSOLPCU(NOPT,NTRD,NTRDV,NNZC,NIAC,NIAPC,NIWC,NPC,
     2              AC,APC,IAC,JAC,IUC,IWC,
     3              GLSPOLY)
      CALL SPSOLTIMER(1,tpcu1,PSOLPCUT)
C
C-------WRITE SUMMARY OF INITIAL EIGENVALUES FOR
C       THE POLYNOMIAL PRECONDITIONER
      IF ( NPC.EQ.4 ) THEN
        IF ( KPER.EQ.1 ) THEN
          IF ( KSTP.EQ.1 ) THEN
            IF ( KITER.EQ.1 ) THEN
              WRITE (IOUT,2000) GLSPOLY%INTV(1), GLSPOLY%INTV(2)
            END IF
          END IF
        END IF
      END IF
2000  FORMAT(//1X,'INITIAL EIGENVALUE RANGE FOR STRESS PERIOD 1, ',
     2            'TIME STEP 1, AND ITERATION 1',/1X,80('-'),
     3        /1X,'MINIMUM EIGENVALUE:',G15.7,            
     4        /1X,'MAXIMUM EIGENVALUE:',G15.7,/1X,80('-'))            
C-------INITIALIZE SOLUTION VARIABLE AND ARRAYS
      iiter = 0
      IF ( KITER.EQ.1 ) ITER1 = 0
      irc    = 1
      ICNVG  = 0
      iicnvg = 0
      alpha  = dzero
      beta   = dzero
      rho    = dzero
      rho0   = dzero
      DO n = 1, NIAC
        DC(n) = DZERO
        PC(n) = DZERO
        QC(n) = DZERO
        ZC(n) = DZERO
      END DO
C-------CALCULATE INITIAL RESIDUAL
      CALL SPSOLMV(NOPT,NTRD,NNZC,NIAC,AC,XC,DC,IAC,JAC)
      DO n = 1, NIAC
        t     = DC(n)
        DC(n) = BC(n) - t
      END DO
C       CPU
      CPUGPUT: IF ( NOPT.EQ.1 .OR. NOPT.EQ.2 ) THEN
C---------INNER ITERATION          
        INNER: DO iiter = 1, NITER
           IPSOLI = IPSOLI + 1
           ITER1  = ITER1  + 1
C-----------APPLY PRECONDITIONER
          CALL SPSOLTIMER(0,tpca1,PSOLPCAT)
          SELECT CASE (NPC)
C             NO PRECONDITIONER
            CASE (0)
              DO n = 1, NIAC
                ZC(n) = DC(n)
              END DO
C             JACOBI PRECONDITIONER
            CASE (1)
              CALL SPSOLJACA(NOPT,NTRDV,NIAC,APC,DC,ZC)
C             ILU0 AND MILU0 PRECONDITIONERS
            CASE (2,3)
              CALL SPSOLILU0A(NNZC,NIAC,NIAPC,
     2                        APC,IAC,JAC,IUC,DC,ZC)
C             POLYNOMIAL PRECONDITIONER
            CASE (4)
              CALL SPSOLPOLYA(NOPT,NTRD,NTRDV,NNZC,NIAC,
     2                        AC,IAC,JAC,GLSPOLY,DC,ZC)
          END SELECT
          CALL SPSOLTIMER(1,tpca1,PSOLPCAT)

          rho = SPSOLDP( NOPT,NTRDV,NIAC,DC,ZC )
C-----------COMPUTE DIRECTIONAL VECTORS
          IF (iiter.EQ.1) THEN
            DO n = 1, NIAC
              PC(n) = ZC(n)
            END DO
          ELSE
            beta = rho / rho0
            DO n = 1, NIAC
              PC(n) = ZC(n) + beta * PC(n)
            END DO
          END IF
C-----------COMPUTE ITERATES
C           UPDATE qc
          CALL SPSOLMV(NOPT,NTRD,NNZC,NIAC,AC,PC,QC,IAC,JAC)

          alpha = rho / SPSOLDP( NOPT,NTRDV,NIAC,PC,QC)
C-----------UPDATE X AND RESIDUAL
          deltax = DZERO
          rmax   = DZERO
C-----------UNSCALE HEAD CHANGE AND RESIDUAL FOR POLYNOMIAL PRECONDITION
          DO n = 1, NIAC
            t      = alpha * PC(n)
            XC(n)  = XC(n) + t
            ta     = t * SCL(n)
            IF ( ABS( ta ).GT.deltax ) THEN
              nx     = n
              deltax = ABS( ta )
            END IF
            t      = DC(n)
            t      = t - alpha * QC(n)
            DC(n)  = t
            ta     = t * SCLI(n)
            IF ( ABS( ta ).GT.rmax ) THEN
              nr   = n
              rmax = ABS( ta )
            END IF
          END DO
          IF ( deltax.LE.dhclose .AND. rmax.LE.drclose ) THEN
            iicnvg = 1
          END IF
          IF ( MXITER.EQ.1 ) THEN
            IF ( iicnvg.EQ.1 ) ICNVG = 1
          ELSE
            IF ( iiter.EQ.1 .AND. iicnvg.EQ.1 ) ICNVG = 1
          ENDIF
          IF ( iicnvg.EQ.1 ) EXIT INNER
C-----------SAVE CURRENT INNER ITERATES
          rho0 = rho
        END DO INNER
!C         GPU
!      ELSE
!        CALL PSOLC7(iiter,CU_HDL,CU_STAT,CU_DES,
!     &              NNZC,NIAC,NIAPC,
!     &              NPC,GLSPOLY%NDEGREE,
!     &              CU_IAC,IAC,CU_JAC,JAC,IUC,
!     &              CU_AC,AC,CU_APC,APC,CU_XC,XC,
!     &              CU_DC,DC,CU_ZC,ZC,
!     &              CU_PC,CU_QC,
!     &              GLSPOLY%ALPHA,GLSPOLY%BETA,GLSPOLY%GAMMA,
!     &              CU_SCL,SCL,CU_SCLI,SCLI,
!     &              CU_V,GLSPOLY%D_V,
!     &              CU_V0,GLSPOLY%D_V0,
!     &              CU_V1,GLSPOLY%D_V1,
!     &              PL_DC,PL_ZC,
!     &              cu_rho0,cu_rho,
!     &              MXITER,ICNVG,NITER,dhclose,drclose,
!     &              deltax,rmax,
!     &              PSOLPCAT,PSOLDPT,PSOLMVT,
!     &              PSOLAXPYT,PSOLVVPT,PSOLMISCT,PSOLGPUTT)
!         IPSOLI = IPSOLI + iiter
!         ITER1  = ITER1  + iiter
!
      END IF CPUGPUT
C
C       UNSCALE XC
!      IF ( NPC.EQ.4 ) THEN
      IF ( ISCL.NE.0 ) THEN
        CALL SPSOLSCL(0,NNZC,NIAC,AC,XC,BC,SCL,SCLI,IAC,JAC)
      END IF
C
C-------FILL HNEW WITH NEW ESTIMATE
      DO n = 1, NIAC
        HNEW(IXMAP(n)) = XC(n)
      END DO
C
C-------IF END OF TIME STEP, PRINT # OF ITERATIONS THIS STEP
      IF ( ICNVG.NE.0 .OR. KITER.EQ.MXITER ) THEN
        WRITE (IOUT,510)
  510   FORMAT (1X,/1X)
        WRITE (IOUT,515) KITER, KSTP, KPER, ITER1
  515   FORMAT (I6,' CALLS TO PCG ROUTINE FOR TIME STEP',I4,
     &          ' IN STRESS PERIOD ',I4,/,I6,' TOTAL ITERATIONS')
        ITER1 = 0
      ENDIF
C
C       SET DUMMY ARGUMENT TIMER VARIABLES TO CURRENT VALUE OF
C       TEMPORARY TIMER VARIABLES
      PSOLDPT     = DTDP
      PSOLMVT     = DTMV
      PSOLAXPYT   = DTAXPY
      PSOLVVPT    = DTVVP
      PSOLMISCT   = DTMISC
C
C-------END PSOL TIMER
      CALL SPSOLTIMER(1,tt1,PSOLTOTT)
C
C-------RETURN
      RETURN
C
      END SUBROUTINE PSOL7AP

      SUBROUTINE PSOL7OT(IGRID)
C     ******************************************************************
C     OUTPUT PSOL TIMER RESULTS - FLOATING POINT OPERATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT
      USE PSOLMODULE
      IMPLICIT NONE
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: IGRID
C     + + + LOCAL DEFINITIONS + + +
      DOUBLEPRECISION :: tPSOL
!      INTEGER :: i
C     + + + FUNCTIONS + + +
C     + + + FORMATS + + +
2000  FORMAT(//,1X,'SUMMARY OF PSOL EXECUTION TIME',
     &        /,1X,'TOTAL NUMBER OF OUTER PSOL ITERATIONS: ',I10,
     &        /,1X,'TOTAL NUMBER OF INNER PSOL ITERATIONS: ',I10,
     &       //,1X,'TIMER ITEM',32X,'TIME (SEC.)',4X,' PERCENTAGE',
     &        /,1X,69('-'),
     &        /,1X,'TOTAL PSOL EXECUTION TIME:             ',G16.7,
     &          1X,F10.3,
     &        /,1X,'TOTAL PSOL A MATRIX FORMULATE TIME:    ',G16.7,
     &          1X,F10.3,
     &        /,1X,'TOTAL PSOL PRECOND. FORMULATE TIME:    ',G16.7,
     &          1X,F10.3,
     &        /,1X,'TOTAL PSOL PRECOND. SOLUTION TIME:     ',G16.7,
     &          1X,F10.3,
     &        /,1X,'TOTAL PSOL DOT PROD. EXECUTION TIME:   ',G16.7,
     &          1X,F10.3,
     &        /,1X,'TOTAL PSOL MV PROD. EXECUTION TIME:    ',G16.7,
     &          1X,F10.3,
     &        /,1X,'TOTAL PSOL AXPY EXECUTION TIME:        ',G16.7,
     &          1X,F10.3,
     &        /,1X,'TOTAL PSOL VV PROD. EXECUTION TIME:    ',G16.7,
     &          1X,F10.3,
     &        /,1X,'TOTAL PSOL MISC. EXECUTION TIME:       ',G16.7,
     &          1X,F10.3,
     &        /,1X,'TOTAL PSOL GPU MEMORY TRANSFER TIME:   ',G16.7,
     &          1X,F10.3,
     &        /,1X,69('-'))
C     + + + CODE + + +
C
C         SET POINTERS
        CALL PSOL7PNT(IGRID)
C
        tPSOL = 1.0D0
        IF ( PSOLTOTT.GT.0.0D0 ) tPSOL = tPSOL / PSOLTOTT
        WRITE (IOUT,2000) IPSOLO, IPSOLI, 
     &                    PSOLTOTT,  100.0D0 * PSOLTOTT * tPSOL,
     &                    PSOLFMAT,  100.0D0 * PSOLFMAT * tPSOL,
     &                    PSOLPCUT,  100.0D0 * PSOLPCUT * tPSOL, 
     &                    PSOLPCAT,  100.0D0 * PSOLPCAT * tPSOL, 
     &                    PSOLDPT,   100.0D0 * PSOLDPT  * tPSOL,
     &                    PSOLMVT,   100.0D0 * PSOLMVT  * tPSOL,
     &                    PSOLAXPYT, 100.0D0 * PSOLAXPYT* tPSOL, 
     &                    PSOLVVPT,  100.0D0 * PSOLVVPT * tPSOL,
     &                    PSOLMISCT, 100.0D0 * PSOLMISCT* tPSOL,
     &                    PSOLGPUTT, 100.0D0 * PSOLGPUTT* tPSOL
     
!C
!C-------SAVE FINAL A MATRIX IN FULL N X N FORM
!      OPEN(UNIT=99,FILE='AMATRIX.BIN',FORM='BINARY',STATUS='REPLACE')
!      WRITE(99) NIAC, NNZC, IAC, JAC, AC
!      CLOSE( 99 )
!C
!C-------SAVE FINAL A MATRIX IN FULL N X N FORM
!      OPEN(UNIT=99,FILE='APCMATRIX.DAT',STATUS='REPLACE')
!      DO i = 1, NIAPC
!        WRITE(99,*) APC(i)
!      END DO
!      CLOSE( 99 )
C
C-------RETURN
      RETURN
C
      END SUBROUTINE PSOL7OT
C
C
      SUBROUTINE PSOL7DA(IGRID)
C  Deallocate PSOL DATA
        USE PSOLMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IGRID
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C         SET POINTERS
        CALL PSOL7PNT(IGRID)
!C
!C         FREE GPU MEMORY
!        IF ( NOPT.EQ.3 ) THEN
!          CALL PSOLC7_FINAL(NPC,CU_HDL,
!     &                      CU_JAC,CU_IAC,
!     &                      CU_AC,CU_APC,CU_XC,
!     &                      CU_DC,CU_ZC,CU_PC,CU_QC,
!     &                      PL_DC,PL_ZC)
!        END IF
!C
C         DEALLOCATE PSOL MEMORY
        DEALLOCATE(ITER1C,NPC,NOPT,NTRD,NTRDV)
        DEALLOCATE(NITERC,NNZC,NIAC)
        DEALLOCATE(NIAPC,NIWC,NPOL,NEIG)
        DEALLOCATE(HCLOSEPSOL,RCLOSEPSOL)
        DEALLOCATE(PSOLTOTT,PSOLFMAT)
        DEALLOCATE(PSOLPCUT,PSOLPCAT,PSOLDPT,PSOLMVT)
        DEALLOCATE(PSOLAXPYT,PSOLVVPT,PSOLMISCT)
        DEALLOCATE(PSOLGPUTT)
        DEALLOCATE(DTDP,DTMV,DTAXPY,DTVVP,DTMISC)
        DEALLOCATE(IPSOLO,IPSOLI)
        DEALLOCATE(NODEC)
        DEALLOCATE(BC)
        DEALLOCATE(XC)
        DEALLOCATE(AC)
        DEALLOCATE(APC)
        DEALLOCATE(IAC)
        DEALLOCATE(JAC)
        DEALLOCATE(IUC)
        DEALLOCATE(IXMAP)
C       WORKING ARRAYS
        DEALLOCATE(IWC)
        DEALLOCATE(DC)
        DEALLOCATE(PC)
        DEALLOCATE(QC)
        DEALLOCATE(ZC)
        DEALLOCATE(ISCL)
        DEALLOCATE(SCL)
        DEALLOCATE(SCLI)
        DEALLOCATE(GLSPOLY)
!C       GPU POINTERS
!        DEALLOCATE(CU_HDL,CU_STAT,CU_DES)
!        DEALLOCATE(CU_IAC,CU_JAC)
!        DEALLOCATE(CU_AC,CU_APC,CU_XC)
!        DEALLOCATE(CU_DC,CU_ZC,CU_PC,CU_QC)
!        DEALLOCATE(CU_SCL,CU_SCLI,CU_V,CU_V0,CU_V1)
!        DEALLOCATE(PL_DC,PL_ZC)        
!C
C---------RETURN
      RETURN
      END SUBROUTINE PSOL7DA
      
      SUBROUTINE PSOL7PNT(IGRID)
C  Set pointers to PSOL data for a grid
      USE PSOLMODULE
C
      ITER1C=>PSOLDAT(IGRID)%ITER1C
      NPC=>PSOLDAT(IGRID)%NPC
      NOPT=>PSOLDAT(IGRID)%NOPT
      NTRD=>PSOLDAT(IGRID)%NTRD
      NTRDV=>PSOLDAT(IGRID)%NTRDV
      NITERC=>PSOLDAT(IGRID)%NITERC
      NNZC=>PSOLDAT(IGRID)%NNZC
      NIAC=>PSOLDAT(IGRID)%NIAC
      NIAPC=>PSOLDAT(IGRID)%NIAPC
      NIWC=>PSOLDAT(IGRID)%NIWC
      NPOL=>PSOLDAT(IGRID)%NPOL
      NEIG=>PSOLDAT(IGRID)%NEIG
      HCLOSEPSOL=>PSOLDAT(IGRID)%HCLOSEPSOL
      RCLOSEPSOL=>PSOLDAT(IGRID)%RCLOSEPSOL
      PSOLTOTT=>PSOLDAT(IGRID)%PSOLTOTT
      PSOLFMAT=>PSOLDAT(IGRID)%PSOLFMAT
      PSOLPCUT=>PSOLDAT(IGRID)%PSOLPCUT
      PSOLPCAT=>PSOLDAT(IGRID)%PSOLPCAT
      PSOLDPT=>PSOLDAT(IGRID)%PSOLDPT
      PSOLMVT=>PSOLDAT(IGRID)%PSOLMVT
      PSOLAXPYT=>PSOLDAT(IGRID)%PSOLAXPYT
      PSOLVVPT=>PSOLDAT(IGRID)%PSOLVVPT
      PSOLMISCT=>PSOLDAT(IGRID)%PSOLMISCT
      PSOLGPUTT=>PSOLDAT(IGRID)%PSOLGPUTT
      IPSOLO=>PSOLDAT(IGRID)%IPSOLO
      IPSOLI=>PSOLDAT(IGRID)%IPSOLI
      NODEC=>PSOLDAT(IGRID)%NODEC
      BC=>PSOLDAT(IGRID)%BC
      XC=>PSOLDAT(IGRID)%XC
      AC=>PSOLDAT(IGRID)%AC
!      TAPC=>PSOLDAT(IGRID)%TAPC
      APC=>PSOLDAT(IGRID)%APC
      IAC=>PSOLDAT(IGRID)%IAC
      JAC=>PSOLDAT(IGRID)%JAC
      IUC=>PSOLDAT(IGRID)%IUC
      IXMAP=>PSOLDAT(IGRID)%IXMAP
C       WORKING ARRAYS
      IWC=>PSOLDAT(IGRID)%IWC
      DC=>PSOLDAT(IGRID)%DC
      PC=>PSOLDAT(IGRID)%PC
      QC=>PSOLDAT(IGRID)%QC
      ZC=>PSOLDAT(IGRID)%ZC
C       POLYNOMIAL PRECONDITIONER
      ISCL=>PSOLDAT(IGRID)%ISCL
      SCL=>PSOLDAT(IGRID)%SCL
      SCLI=>PSOLDAT(IGRID)%SCLI
      GLSPOLY=>PSOLDAT(IGRID)%GLSPOLY
!C       GPU POINTERS
!      CU_HDL=>PSOLDAT(IGRID)%CU_HDL
!      CU_STAT=>PSOLDAT(IGRID)%CU_STAT
!      CU_DES=>PSOLDAT(IGRID)%CU_DES
!      CU_IAC=>PSOLDAT(IGRID)%CU_IAC
!      CU_JAC=>PSOLDAT(IGRID)%CU_JAC
!      CU_AC=>PSOLDAT(IGRID)%CU_AC
!      CU_APC=>PSOLDAT(IGRID)%CU_APC
!      CU_XC=>PSOLDAT(IGRID)%CU_XC
!      CU_DC=>PSOLDAT(IGRID)%CU_DC
!      CU_ZC=>PSOLDAT(IGRID)%CU_ZC
!      CU_PC=>PSOLDAT(IGRID)%CU_PC
!      CU_QC=>PSOLDAT(IGRID)%CU_QC
!      CU_SCL=>PSOLDAT(IGRID)%CU_SCL
!      CU_SCLI=>PSOLDAT(IGRID)%CU_SCLI
!      CU_V=>PSOLDAT(IGRID)%CU_V
!      CU_V0=>PSOLDAT(IGRID)%CU_V0
!      CU_V1=>PSOLDAT(IGRID)%CU_V1
!      PL_DC=>PSOLDAT(IGRID)%PL_DC
!      PL_ZC=>PSOLDAT(IGRID)%PL_ZC
!C
      RETURN
      END SUBROUTINE PSOL7PNT

      SUBROUTINE PSOL7PSV(IGRID)
C  Save pointers to PSOL data
      USE PSOLMODULE
C
      PSOLDAT(IGRID)%ITER1C=>ITER1C
      PSOLDAT(IGRID)%NPC=>NPC
      PSOLDAT(IGRID)%NOPT=>NOPT
      PSOLDAT(IGRID)%NTRD=>NTRD
      PSOLDAT(IGRID)%NTRDV=>NTRDV
      PSOLDAT(IGRID)%NITERC=>NITERC
      PSOLDAT(IGRID)%NNZC=>NNZC
      PSOLDAT(IGRID)%NIAC=>NIAC
      PSOLDAT(IGRID)%NIAPC=>NIAPC
      PSOLDAT(IGRID)%NIWC=>NIWC
      PSOLDAT(IGRID)%NPOL=>NPOL
      PSOLDAT(IGRID)%NEIG=>NEIG
      PSOLDAT(IGRID)%HCLOSEPSOL=>HCLOSEPSOL
      PSOLDAT(IGRID)%RCLOSEPSOL=>RCLOSEPSOL
      PSOLDAT(IGRID)%PSOLTOTT=>PSOLTOTT
      PSOLDAT(IGRID)%PSOLFMAT=>PSOLFMAT
      PSOLDAT(IGRID)%PSOLPCUT=>PSOLPCUT
      PSOLDAT(IGRID)%PSOLPCAT=>PSOLPCAT
      PSOLDAT(IGRID)%PSOLDPT=>PSOLDPT
      PSOLDAT(IGRID)%PSOLMVT=>PSOLMVT
      PSOLDAT(IGRID)%PSOLAXPYT=>PSOLAXPYT
      PSOLDAT(IGRID)%PSOLVVPT=>PSOLVVPT
      PSOLDAT(IGRID)%PSOLMISCT=>PSOLMISCT
      PSOLDAT(IGRID)%PSOLGPUTT=>PSOLGPUTT
      PSOLDAT(IGRID)%IPSOLO=>IPSOLO
      PSOLDAT(IGRID)%IPSOLI=>IPSOLI
      PSOLDAT(IGRID)%NODEC=>NODEC
      PSOLDAT(IGRID)%BC=>BC
      PSOLDAT(IGRID)%XC=>XC
      PSOLDAT(IGRID)%AC=>AC
!      PSOLDAT(IGRID)%TAPC=>TAPC
      PSOLDAT(IGRID)%APC=>APC
      PSOLDAT(IGRID)%IAC=>IAC
      PSOLDAT(IGRID)%JAC=>JAC
      PSOLDAT(IGRID)%IUC=>IUC
      PSOLDAT(IGRID)%IXMAP=>IXMAP
C       WORKING ARRAYS
      PSOLDAT(IGRID)%IWC=>IWC
      PSOLDAT(IGRID)%DC=>DC
      PSOLDAT(IGRID)%PC=>PC
      PSOLDAT(IGRID)%QC=>QC
      PSOLDAT(IGRID)%ZC=>ZC
C       POLYNOMIAL PRECONDITIONER
      PSOLDAT(IGRID)%ISCL=>ISCL
      PSOLDAT(IGRID)%SCL=>SCL
      PSOLDAT(IGRID)%SCLI=>SCLI
      PSOLDAT(IGRID)%GLSPOLY=>GLSPOLY
!C       GPU POINTERS
!      PSOLDAT(IGRID)%CU_HDL=>CU_HDL
!      PSOLDAT(IGRID)%CU_STAT=>CU_STAT
!      PSOLDAT(IGRID)%CU_DES=>CU_DES
!      PSOLDAT(IGRID)%CU_IAC=>CU_IAC
!      PSOLDAT(IGRID)%CU_JAC=>CU_JAC
!      PSOLDAT(IGRID)%CU_AC=>CU_AC
!      PSOLDAT(IGRID)%CU_APC=>CU_APC
!      PSOLDAT(IGRID)%CU_XC=>CU_XC
!      PSOLDAT(IGRID)%CU_DC=>CU_DC
!      PSOLDAT(IGRID)%CU_ZC=>CU_ZC
!      PSOLDAT(IGRID)%CU_PC=>CU_PC
!      PSOLDAT(IGRID)%CU_QC=>CU_QC
!      PSOLDAT(IGRID)%CU_SCL=>CU_SCL
!      PSOLDAT(IGRID)%CU_SCLI=>CU_SCLI
!      PSOLDAT(IGRID)%CU_V=>CU_V
!      PSOLDAT(IGRID)%CU_V0=>CU_V0
!      PSOLDAT(IGRID)%CU_V1=>CU_V1
!      PSOLDAT(IGRID)%PL_DC=>PL_DC
!      PSOLDAT(IGRID)%PL_ZC=>PL_ZC
C
      RETURN
      END SUBROUTINE PSOL7PSV
C
C-------ROUTINE TO SCALE THE COEFFICIENT MATRIX
      SUBROUTINE SPSOLSCL(ISCALE,NNZC,NIAC,AC,XC,BC,SCL,SCLI,IAC,JAC)
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: ISCALE
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(INOUT)  :: AC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: XC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: BC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: SCL
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: SCLI
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAC
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n, i
        INTEGER :: ic
        INTEGER :: i0, i1
        DOUBLEPRECISION :: c1, c2, v
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------SCALE AC, XC, AND BC
        IF ( ISCALE.NE.0 ) THEN
          DO n = 1, NIAC
            ic = IAC(n)
            SCL(n)  = 1.0D0 / SQRT( ABS( AC(ic) ) )
            SCLI(n) = SQRT( ABS( AC(ic) ) )
          END DO
          DO n = 1, NIAC
            c1 = SCL(n)
            i0 = IAC(n)
            i1 = IAC(n+1) - 1
            DO i = i0, i1
              ic    = JAC(i)
              c2    = SCL(ic)
              v     = c1 * AC(i) * c2
              AC(i) = v
            END DO
          END DO
C-----------SCALE XC AND BC
          DO n = 1, NIAC
            c1     = SCL(n)
            XC(n)  = XC(n) / c1
            BC(n)  = BC(n) * c1
          END DO
C---------UNSCALE XC -- NO NEED TO UNSCALE AC AND BC BECAUSE THEY ARE NOT REUSED
        ELSE
          DO n = 1, NIAC
            c1 = SCL(n)
!            c2 = SCLI(n)
            i0 = IAC(n)
            i1 = IAC(n+1) - 1
!C             UNSCALE AC
!            DO i = i0, i1
!              jc = JAC(i)
!              c2 = SCL(jc)
!              AC(i) = ( 1.0D0 / c1 ) * AC(i) * ( 1.0D0 / c2 ) 
!            END DO
C             UNSCALE XC
            XC(n) = XC(n) * c1
!            BC(n) = BC(n) / c1
!            BC(n) = BC(n) * c2
          END DO     
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLSCL
C
C-------ROUTINE TO UPDATE THE PRECONDITIONER
      SUBROUTINE SPSOLPCU(NOPT,NTRD,NTRDV,NNZC,NIAC,NIAPC,NIWC,NPC,
     2                    AC,APC,IAC,JAC,IUC,IWC,
     3                    GLSPOLY)
        USE PSOLMODULE, ONLY: TGLSPOLY
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRD
        INTEGER, INTENT(IN) :: NTRDV
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NIAPC
        INTEGER, INTENT(IN) :: NIWC
        INTEGER, INTENT(IN) :: NPC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)     :: AC
!        DOUBLEPRECISION, DIMENSION(NIAPC), INTENT(INOUT)  :: TAPC
        DOUBLEPRECISION, DIMENSION(NIAPC), INTENT(INOUT)  :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAC
        INTEGER, DIMENSION(NIAC), INTENT(IN)     :: IUC
        INTEGER, DIMENSION(NIWC), INTENT(INOUT)  :: IWC
        TYPE (TGLSPOLY), INTENT(INOUT)           :: GLSPOLY
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        SELECT CASE(NPC)
C           NO PRE-CONDITIONER
          CASE (0)
C           JACOBI PRE-CONDITIONER
          CASE (1)
            CALL SPSOLPCJ(NNZC,NIAC,AC,APC,IAC)
C           ILU0
          CASE (2,3)
            CALL SPSOLPCILU0(NPC,NNZC,NIAC,NIAPC,NIWC,
     2                       AC,APC,IAC,JAC,IUC,IWC)
C           NEUMAN POLYNOMIAL
          CASE (4)
            CALL SPSOLGLSPOL(NOPT,NTRD,NTRDV,NNZC,NIAC,AC,IAC,JAC,
     2                       GLSPOLY)
C           ADDITIONAL PRECONDITIONERS - ILU, etc.
        END SELECT
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLPCU
C
C-------JACOBI PRECONDITIONER - INVERSE OF DIAGONAL 
      SUBROUTINE SPSOLPCJ(NNZC,NIAC,AC,APC,IAC)
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)      :: AC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)   :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IAC
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: ic0, ic1
        INTEGER :: id
        DOUBLEPRECISION :: t
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, NIAC
            id = IAC(n)
            t  = AC(id)
            IF ( ABS( t ).EQ.DZERO ) THEN
              CALL USTOP('SPSOLPCJ ERROR: ABS(AC)=0.0')
            END IF
            APC(n) = DONE / t
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLPCJ

      SUBROUTINE SPSOLJACA(NOPT,NTRDV,NIAC,A,D1,D2)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRDV
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: A
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: D1
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D2
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: t, djac
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        CALL SPSOLVVP(NOPT,NTRDV,NIAC,A,D1,D2)
!        SELECT CASE ( NOPT )
!C           CPU
!          CASE (1)
!            DO n = 1, NIAC
!              t     = A(n) * D1(n)
!              D2(n) = t
!            END DO
!C           CPU - OPEN MP          
!          CASE (2)
!!$OMP  PARALLEL
!!$OMP& NUM_THREADS(NTRDV)
!!$OMP& DEFAULT(SHARED)
!!$OMP& PRIVATE(n, t)
!!$OMP  DO
!            DO n = 1, NIAC
!C               MULTIPLY INVERSE OF DIAGONAL AND D1
!              t     = A(n) * D1(n)
!              D2(n) = t
!            END DO
!!$OMP  END DO
!!$OMP  END PARALLEL
!        END SELECT
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLJACA

      SUBROUTINE SPSOLPCILU0(NPC,NNZC,NIAC,NIAPC,NIWC,
     2                       AC,APC,IAC,JAC,IUC,IWC)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NPC
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NIAPC
        INTEGER, INTENT(IN) :: NIWC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)     :: AC
        DOUBLEPRECISION, DIMENSION(NIAPC), INTENT(INOUT)  :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAC
        INTEGER, DIMENSION(NIAC), INTENT(IN)     :: IUC
        INTEGER, DIMENSION(NIWC), INTENT(INOUT)  :: IWC
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1, id0, iu1
        INTEGER :: iic0, iic1
        INTEGER :: j, n
        INTEGER :: jj, nn
        INTEGER :: jpos, jcol, jw
        INTEGER :: id
        INTEGER :: izero
        DOUBLEPRECISION :: tl
        DOUBLEPRECISION :: t
        DOUBLEPRECISION :: rs
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        izero = 0
        DO n = 1, NIAPC
          APC(n) = AC(n)
        END DO
        DO n = 1, NIAC
          IWC(n)  = 0
        END DO
        MAIN: DO n = 1, NIAC
          ic0 = IAC(n)
          ic1 = IAC(n+1)-1
          DO j = ic0, ic1
            jcol = JAC(j)
            IWC(jcol) = j
          END DO
          rs    = DZERO
          jpos = IAC(n)
          iu1 = IUC(n) - 1
          LOWER: DO j = ic0+1, iu1
            jpos = j
            jcol = JAC(j)
            id0  = IAC(jcol)
            tl   = APC(j) * APC(id0)
            APC(j) = tl
            iic0 = IUC(jcol)
            iic1 = IAC(jcol+1) - 1
            DO jj = iic0, iic1
              jw = IWC(JAC(jj))
              IF ( jw.NE.0 ) THEN
                APC(jw) = APC(jw) - tl * APC(jj)
              ELSE
                IF ( NPC.EQ.3 ) rs = rs + tl * APC(jj)
                !rs = rs + tl * APC(jj)
              END IF
            END DO
          END DO LOWER
C           DIAGONAL - CALCULATE INVERSE OF DIAGONAL FOR SOLUTION
          id0 = IAC(n)
          tl  = APC(id0) - rs
          IF ( tl.GT.DZERO ) THEN
            APC(id0) = DONE / tl
          ELSE
            CALL USTOP('SPSOLPCILU0: tl <= 0.0')
            !izero = 1
            !EXIT MAIN
            !APC(id0) = 1.0D+20
          END IF
C           RESET POINTER FOR IW TO ZERO
          DO j = ic0, ic1
            jcol = JAC(j)
            IWC(jcol) = 0
          END DO
        END DO MAIN
C---------REVERT TO A IF ZERO ON DIAGONAL ENCOUNTERED
        IF ( izero.NE.0 ) THEN
          DO n = 1, NIAPC
            APC(n) = AC(n)
          END DO
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLPCILU0

      SUBROUTINE SPSOLILU0A(NNZC,NIAC,NIAPC,
     2                      APC,IAC,JAC,IUC,R,D)
!        USE OMP_LIB
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NIAPC
        DOUBLEPRECISION, DIMENSION(NIAPC),  INTENT(INOUT)  :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)   :: JAC
        INTEGER, DIMENSION(NIAC), INTENT(IN)   :: IUC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)     :: R
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: D
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1, id0
        INTEGER :: jcol
        INTEGER :: j, n
        DOUBLEPRECISION :: t
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C         FORWARD SOLVE - APC * D = R
        FORWARD: DO n = 1, NIAC
          t   = R(n)
          ic0 = IAC(n) + 1
          ic1 = IUC(n) - 1
          LOWER: DO j = ic0, ic1
            jcol = JAC(j)
            t    = t - APC(j) * D(jcol)
          END DO LOWER
          D(n) = t
        END DO FORWARD
C         BACKWARD SOLVE - D = D / U
        BACKWARD: DO n = NIAC, 1, -1
          id0 = IAC(n)
          ic0 = IUC(n)
          ic1 = IAC(n+1)-1
          t   = D(n)
          UPPER: DO j = ic0, ic1
            jcol = JAC(j)
            t    = t - APC(j) * D(jcol)
          END DO UPPER
C           COMPUTE D FOR DIAGONAL - D = D / U
          D(n) = APC(id0) * t
        END DO BACKWARD
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLILU0A

C-------POLYNOMIAL PRECONDITIONER
      SUBROUTINE SPSOLGLSPOL(NOPT,NTRD,NTRDV,NNZC,NIAC,AC,IAC,JAC,
     2                       GLSPOLY)
        USE PSOLMODULE, ONLY: TGLSPOLY
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRD
        INTEGER, INTENT(IN) :: NTRDV
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)     :: AC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAC
        TYPE (TGLSPOLY), INTENT(INOUT)           :: GLSPOLY 
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------ESTIMATE MAXIMUM AND MINIMUM EIGENVALUES
C         ACTUAL MAXIMUM AND MINIMUM EIGENVALUES ARE CALCULATED IF
C         GLSPOLY%NLANSTEP EQUALS NIAC
        IF ( GLSPOLY%IEIGCALC.NE.0 ) THEN
          CALL PSOLLANCZOS(NOPT,NTRD,NTRDV,NIAC,NNZC,IAC,JAC,AC,
     2      GLSPOLY%NLANSTEP,GLSPOLY%NLAN2,GLSPOLY%D_LANCZOS,
     3      GLSPOLY%D_V1,GLSPOLY%D_V0,GLSPOLY%D_V,
     4      GLSPOLY%D_E,GLSPOLY%INTV)
        END IF
C
C---------CALCULATE ALPHA, BETA, AND GAMMA VALUES FOR POLYNOMIAL
C         PRECONDITIONER
        CALL PSOLUPDPOLY(GLSPOLY)        
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLGLSPOL


      SUBROUTINE SPSOLMV(NOPT,NTRD,NNZC,NIAC,A,D1,D2,IAC,JAC)
!        USE OMP_LIB
        USE PSOLMODULE, ONLY: DTMV
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRD
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)    :: A
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: D1
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D2
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)   :: JAC
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
        INTEGER :: istart, iend
        INTEGER :: jstart, jend, jlen
        INTEGER :: jcol
        INTEGER :: n0
        INTEGER :: iblksize
        DOUBLEPRECISION :: tv
        DOUBLEPRECISION :: t
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------START TIMER
        CALL SPSOLTIMER(0,tv,DTMV)
C
        SELECT CASE ( NOPT )
          CASE (1,3)
            DO i = 1, NIAC
C               ADD DIAGONAL AND OFF-DIAGONAL TERMS
              t   = DZERO
              jstart = IAC(i)
              jend   = IAC(i+1)-1
              DO j = jstart, jend
                jcol = JAC(j) 
                t  = t + A(j) * D1(jcol)
              END DO
              D2(i) = t
            END DO
          CASE (2)
            n0 = ( ( NIAC - 1 ) / NTRD ) + 1
!$OMP  PARALLEL 
!$OMP& SHARED(NIAC,A,D1,D2)
!$OMP& PRIVATE(iblksize,istart,iend,jstart,jend,jlen)
!$OMP& NUM_THREADS(NTRD)
!$OMP  DO SCHEDULE(STATIC) 
            do i = 1, NTRD
              iblksize = min( n0, NIAC - ( i - 1 ) * n0 )
              istart   = ( i - 1 ) * n0 + 1
              iend     = istart + iblksize
              jstart   = IAC(istart)
              jend     = IAC(iend)
              jlen     = jend - jstart + 1
              IF ( iblksize.GT.0 ) THEN 
                CALL SPSOLSGEMV(iblksize,NIAC,jlen,jstart,
     2                          IAC(istart),JAC(jstart),
     3                          A(jstart),D1,D2(istart))
              END IF
            END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
!!$OMP  PARALLEL DO
!!$OMP& NUM_THREADS(NTRD)
!!$OMP& DEFAULT(SHARED)
!!$OMP& PRIVATE(i, ic0, ic1, j, t)
!!!$OMP& PRIVATE(i, ic0, ic1, j)
!!!$OMP& REDUCTION(+:t)
!            DO i = 1, NIAC
!C               ADD DIAGONAL AND OFF-DIAGONAL TERMS
!              t   = 0.0D0
!              ic0 = IAC(i)
!              ic1 = IAC(i+1) - 1
!              DO j = ic0, ic1
!!                WRITE (*,1070) OMP_GET_THREAD_NUM(), i, JAC(j), 
!!     2                            j, ic0, ic1, t
!                t = t + A(j) * D1( JAC(j) )
!              END DO
!              D2(i) = t
!            END DO
!!$OMP  END PARALLEL DO
!!            WRITE (*,*)
        END SELECT
1070    FORMAT('TID:',I5,1X,'Row:',I5,1X,'Col:',I5,1X,
     2         'j:',I5,1X,'ic0:',I5,1X,'ic1',I5,1X't:'G15.7)
C
C         END TIMER
        CALL SPSOLTIMER(1,tv,DTMV)
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLMV

      SUBROUTINE SPSOLSGEMV(IBLKSIZE,NIAC,JLEN,JSTART,IA,JA,A,D1,D2)
        IMPLICIT NONE
C         + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IBLKSIZE
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: JLEN
        INTEGER, INTENT(IN) :: JSTART
        INTEGER, DIMENSION(IBLKSIZE+1),       INTENT(IN)    :: IA
        INTEGER, DIMENSION(JLEN),             INTENT(IN)    :: JA
        DOUBLEPRECISION, DIMENSION(JLEN),     INTENT(IN)    :: A
        DOUBLEPRECISION, DIMENSION(NIAC),     INTENT(IN)    :: D1
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(INOUT) :: D2
C         + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
        INTEGER :: j0, j1, jcol
        DOUBLEPRECISION, PARAMETER :: dzero = 0.0d0
C         + + + FUNCTIONS + + +
C         + + + CODE + + +
        DO i = 1, IBLKSIZE
          j0 = IA(i)   - JSTART + 1
          j1 = IA(i+1) - JSTART
          D2(i) = dzero
          DO j = j0, j1
            jcol = JA(j)
            D2(i) = D2(i) + A(j) * D1(jcol)
          END DO
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLSGEMV

      SUBROUTINE SPSOLAXPY(NOPT,NTRDV,NIAC,C,D1,D2)
        USE PSOLMODULE, ONLY: DTAXPY
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRDV
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION,  INTENT(IN)                     :: C
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: D1
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D2
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: tv
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C---------START TIMER
        CALL SPSOLTIMER(0,tv,DTAXPY)
C
        SELECT CASE ( NOPT )
          CASE ( 1 )
            DO n = 1, NIAC
              D2(n) = D2(n) + C * D1(n)
            END DO
          CASE ( 2 )
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NTRDV)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(n)
!$OMP  DO SCHEDULE(STATIC)
            DO n = 1, NIAC
              D2(n) = D2(n) + C * D1(n)
            END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        END SELECT
C---------END TIMER
        CALL SPSOLTIMER(1,tv,DTAXPY)
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLAXPY

      SUBROUTINE SPSOLSETX(NOPT,NTRDV,NIAC,D1,C)
        USE PSOLMODULE, ONLY: DTMISC
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRDV
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D1
        DOUBLEPRECISION,  INTENT(IN)                     :: C
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: tv
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C---------START TIMER
        CALL SPSOLTIMER(0,tv,DTMISC)
C
        SELECT CASE ( NOPT )
          CASE ( 1 )
            DO n = 1, NIAC
              D1(n) = C
            END DO
          CASE ( 2 )
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NTRDV)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(n)
!$OMP  DO SCHEDULE(STATIC)
            DO n = 1, NIAC
              D1(n) = C
            END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        END SELECT
C---------END TIMER
        CALL SPSOLTIMER(1,tv,DTMISC)
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLSETX

      SUBROUTINE SPSOLDCOPY(NOPT,NTRDV,NIAC,D1,D2)
        USE PSOLMODULE, ONLY: DTMISC
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRDV
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: D1
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D2
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: tv
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C---------START TIMER
        CALL SPSOLTIMER(0,tv,DTMISC)
C
        SELECT CASE ( NOPT )
          CASE ( 1 )
            DO n = 1, NIAC
              D2(n) = D1(n)
            END DO
          CASE ( 2 )
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NTRDV)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(n)
!$OMP  DO SCHEDULE(STATIC)
            DO n = 1, NIAC
              D2(n) = D1(n)
            END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        END SELECT
C---------END TIMER
        CALL SPSOLTIMER(1,tv,DTMISC)
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLDCOPY

      SUBROUTINE SPSOLDSCAL(NOPT,NTRDV,NIAC,C,D1)
        USE PSOLMODULE, ONLY: DTMISC
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRDV
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, INTENT(IN) :: C
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D1
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: tv
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C---------START TIMER
        CALL SPSOLTIMER(0,tv,DTMISC)
C
        SELECT CASE ( NOPT )
          CASE ( 1 )
            DO n = 1, NIAC
              D1(n) = C * D1(n)
            END DO
          CASE ( 2 )
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NTRDV)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(n)
!$OMP  DO SCHEDULE(STATIC)
           DO n = 1, NIAC
              D1(n) = C * D1(n)
            END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        END SELECT
C---------END TIMER
        CALL SPSOLTIMER(1,tv,DTMISC)
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLDSCAL

      SUBROUTINE SPSOLVVP(NOPT,NTRDV,NIAC,D1,D2,D3)
        USE PSOLMODULE, ONLY: DTVVP
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRDV
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: D1
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: D2
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D3
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: tv
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C---------START TIMER
        CALL SPSOLTIMER(0,tv,DTVVP)
C
        SELECT CASE ( NOPT )
          CASE ( 1 )
            DO n = 1, NIAC
              D3(n) = D1(n) * D2(n)
            END DO
          CASE ( 2 )
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NTRDV)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(n)
!$OMP  DO SCHEDULE(STATIC)
            DO n = 1, NIAC
              D3(n) = D1(n) * D2(n)
            END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        END SELECT
C---------END TIMER
        CALL SPSOLTIMER(1,tv,DTVVP)
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLVVP

      DOUBLEPRECISION FUNCTION SPSOLDP(NOPT,NTRDV,NIAC,A,B) RESULT(C)
        USE PSOLMODULE, ONLY: DTDP
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NOPT
        INTEGER, INTENT(IN) :: NTRDV
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: A
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: B
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: tv
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C---------START TIMER
        CALL SPSOLTIMER(0,tv,DTDP)
C
        C = DZERO
        SELECT CASE ( NOPT )
          CASE ( 1 )
            DO n = 1, NIAC
              C = C + A(n) * B(n)
            END DO
          CASE ( 2 )
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NTRDV)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(n)
!$OMP& REDUCTION(+: C)
!$OMP  DO SCHEDULE(STATIC)
            DO n = 1, NIAC
              C = C + A(n) * B(n)
            END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        END SELECT
C---------END TIMER
        CALL SPSOLTIMER(1,tv,DTDP)
C---------RETURN
        RETURN
      END FUNCTION SPSOLDP
C
C-------INFINITY NORM
      DOUBLEPRECISION FUNCTION SPSOLLINFNORM(NIAC,A) RESULT(B)
        USE PSOLMODULE, ONLY: DTMISC
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: A
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: tv
        DOUBLEPRECISION :: babs
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C---------START TIMER
        CALL SPSOLTIMER(0,tv,DTMISC)
C
        B    = DZERO
        babs = DZERO
        DO n = 1, NIAC
          IF ( ABS( A(n) ).GT.babs ) THEN
            B    = A(n)
            babs = ABS( B )
          END IF
        END DO
C---------END TIMER
        CALL SPSOLTIMER(1,tv,DTMISC)
C---------RETURN
        RETURN
      END FUNCTION SPSOLLINFNORM
C
C-------TIMER FOR PSOL CALCULATIONS
      SUBROUTINE SPSOLTIMER(It,T1,Ts)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: It
        DOUBLEPRECISION, INTENT(INOUT) :: T1
        DOUBLEPRECISION, INTENT(INOUT) :: Ts
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: tt
        DOUBLEPRECISION :: dt
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( It.EQ.0 ) THEN
          T1 = SECNDS(0.0)
        ELSE
          dt = SECNDS(T1)
          Ts = Ts + dt
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPSOLTIMER
      
      
