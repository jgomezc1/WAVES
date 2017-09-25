CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   --------P R O G R A M   D A M I A N   E X P L I C I T------------  C
C                       Finite Element Method                          C
C                                                                      C
C   EXPLICIT TIME DOMAIN PLANE STRAIN ANALYSIS FOR FINITE DOMAINS.     C
C                                                                      C
C  DYNAMIC MEMORY ALLOCATION                                           C
C  EAFIT UNIVERSITY                                                    C
C  APPLIED MECHANICS LAB                                               C
C  JANUARY 14/2017                                                     C
C                                                                      C
C  UNIX VERSION                                                        C
C                                                                      C
C            P R O B L E M   P A R A M E T E R S                       C
C                                                                      C
C  NUMNP     :NUMBER OF NODAL POINTS                                   C
C  NUMEL     :UMBER OF ELEMENTS                                        C
C  NUMAT     :NUMBER OF MATERIAL PROFILES                              C
C  TM        :SIZE OF THE TIME WINDOW.                                 C
C  NINCR     :NUMBER OF INCREMENTS                                     C
C  NDOFDIM   :PROBLEM DIMENSIONALITY                                   C
C  NMNE      :MAXIMUM NUMBER OF NODES PER ELEMENT                      C
C  NMDOFEL   :MAXIMUM NUMBER OF DOF PER ELEMENT                        C
C  NMPR      :MAXIMUM NUMBER OF MATERIAL PROPERTIES IN A PROFILE       C
C  NIPR      :MAXIMUM NUMBER OF INTEGER MAT PROPERTIES IN A PROFILE    C
C  NPL       :NUMBER OF POINT LOADS                                    C
C  NIMP      :NUMBER OF POINTS FOR OUTPUT                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
***********************************************************************C
C                                                                      C
C                      MAIN PROGRAM STARTS                             C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C
      PROGRAM DAMIAN_EXPLICIT

      USE OMP_LIB
      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION AWAVE(5)
C
      ALLOCATABLE ID(:,:),NDOFN(:),COORD(:,:),MATP(:),NMATP(:),NIMTP(:),
     1            AMATE(:,:),IMTEI(:,:),IELT(:),NNE(:),NDOFEL(:),
     2            IELCON(:,:),LM(:,:),ILIST(:,:),LPLIST(:,:),NIEL(:),
     3            AG(:),VG(:),UTMINT(:),UT(:),UTMAST(:),ISTOUT(:)

      ALLOCATABLE LINCO(:)

      ALLOCATABLE IDPL(:)

      CHARACTER*80 HED
      CHARACTER*20 FILENAME
      
      REAL*8 TIME, ELAPSED_TIME
      INTEGER TCLOCK1, TCLOCK2, CLOCK_RATE
      INTEGER TCLOCK3

C     *****************************************************************C
C     ***          P R O B L E M  F I L E S                          **C
C     *****************************************************************C

      IIN=5
      IOUT=4
      IDIS=7
      ILOAD=9999
      INCOMI=7777
      IDISP= 5555
      IVEL=	 6666 
      IACEL= 8888
      ICOORD=4444
      WRITE(*,*) 'INPUT THE JOB NAME(max 10 characters):'
      READ(*,*) FILENAME
      LST=LEN_TRIM(FILENAME)
      OPEN(UNIT=IIN,FILE =FILENAME(1:LST)//".inp",FORM='FORMATTED')
      OPEN(UNIT=IOUT,FILE=FILENAME(1:LST)//".dat",FORM='FORMATTED')
      OPEN(UNIT=IDIS,FILE=FILENAME(1:LST)//".dis",FORM='FORMATTED')

C     *****************************************************************C
C     ***                 I N P U T   P H A S E                      **C
C     *****************************************************************C
C
C     READS PROBLEM DEFINITION PARAMETERS.

      READ(IIN,*,IOSTAT=INOUTSTATUS) HED
      IF(INOUTSTATUS.LT.0) STOP "***COULD NOT OPEN FILE"

      CALL SYSTEM_CLOCK(TCLOCK1)
      CALL CPU_TIME(TIME)
      WRITE(*,'(1A23)') 'Begin the analysis: '
      WRITE(*,*)

      READ(IIN,     *) NUMNP,NUMEL,NUMAT,TM,NINCR,NDOFDIM,NMNE,NMDOFEL,
     1                 NMPR,NIPR,NPL,ITPL,NIMP
      READ(IIN,     *) IPHOTO, NTH

      IF (ITPL==1) THEN
        OPEN(UNIT=ILOAD,FILE=FILENAME(1:LST)//".load")
      END IF

      IF (NIMP.NE.0) THEN
        ALLOCATE(ISTOUT(NIMP))
        ISTOUT=0
      END IF

      WRITE(IOUT,1900) HED
      WRITE(IOUT,2000) NUMNP,NUMEL,NUMAT,TM,NINCR,NDOFDIM,NMNE,NMDOFEL,
     1                 NMPR,NIPR,NPL,NIMP

      ALLOCATE(ID(NDOFDIM,NUMNP),NDOFN(NUMNP),COORD(NDOFDIM,NUMNP),
     1         MATP(NUMEL),NMATP(NUMAT),NIMTP(NUMAT),AMATE(NMPR,NUMAT),
     2         IMTEI(NIPR,NUMAT),IELT(NUMEL),NNE(NUMEL),
     3         NDOFEL(NUMEL),IELCON(NMNE,NUMEL),LM(NMDOFEL,NUMEL),
     4         NIEL(NUMNP))

      ALLOCATE(IDPL(NPL))

C$ CALL OMP_SET_NUM_THREADS(NTH)
C$OMP PARALLEL

C$OMP SINGLE
      IDPL=0
      ID=0
      NDOFN=0
      COORD=0.0D0
C
      CALL NODINP(NUMNP,ID,NDOFDIM,COORD,NDOFN,NEQ,IIN,IOUT)
C$OMP END SINGLE

C$OMP SECTIONS

C     SOLUTION ARRAYS.

C$OMP SECTION
      ALLOCATE(AG(NEQ),VG(NEQ))

      AG=0.0D0
      VG=0.0D0

C$OMP SECTION
      ALLOCATE(UTMINT(NEQ),UT(NEQ))
      UT    =0.0D0
      UTMINT=0.0D0
      
C     CLEARS STORAGE.

C$OMP SECTION
      MATP=0
      NMATP=0
      NIMTP=0
      AMATE=0.0D0
      IMTEI=0
      IELT=0
      NNE=0
      NDOFEL=0
      IELCON=0
      LM=0
      NIEL=0

C     READS MODEL

      CALL MATINP(NUMAT,NMPR,NMATP,AMATE,NIPR,NIMTP,IMTEI,AWAVE,IWAVE,
     1            IIN,IOUT)

      IF (IWAVE==3) THEN
      	OPEN(UNIT=INCOMI,FILE=FILENAME(1:LST)//".inco",FORM='FORMATTED')
            READ(INCOMI,*) NPINCO
            ALLOCATE(LINCO(NPINCO))
            LINCO=0
            DO I=1, NPINCO
                READ(INCOMI,*) LINCO(I)
            END DO
      END IF

      CALL ELEINP(NUMNP,NUMEL,NNE,IELT,NDOFEL,NMNE,MATP,IELCON,
     1            NUMPARA,IIN,IOUT)

	  CALL POINTLOAD(NUMNP,NDOFDIM,NPL,ID,IDPL,IIN)

      CALL IMPHIST(NIMP,ISTOUT,IIN)

C     Print nodal coordinates of points at which output is required.

      IF (NIMP.NE.0) THEN

	  	OPEN(UNIT=IDISP ,FILE="../Results/"//FILENAME(1:LST)//".disp")
	  	OPEN(UNIT=IVEL  ,FILE="../Results/"//FILENAME(1:LST)//".vel")
	  	OPEN(UNIT=IACEL ,FILE="../Results/"//FILENAME(1:LST)//".acel")
	  	OPEN(UNIT=ICOORD,FILE="../Results/"//FILENAME(1:LST)//".coord")

        DO I=1, NIMP
            J=ISTOUT(I)
            WRITE(ICOORD,'(6F11.6)') COORD(:,J)
        END DO
        CLOSE(ICOORD)
      END IF

C$OMP END SECTIONS
C$OMP END PARALLEL

C     CREATES ASSEMBLY LIST (DME OPERATOR) AND LIST
C     OF INCIDENT ELEMENTS FOR EACH NODE.

      CALL ASSEMLIS(NUMNP,NUMEL,NMNE,NDOFDIM,NNE,NDOFN,IELCON,
     1              LM,ID,NTH)    

      CALL MAXELENODE(NUMNP,NUMEL,NMNE,IELCON,NIEL,NTH,NMELNOD)

      ALLOCATE(ILIST(NUMNP,NMELNOD),LPLIST(NUMNP,NMELNOD))

      CALL INCLIST(NUMNP,NUMEL,NMNE,IELCON,ILIST,LPLIST,NIEL,
     1             NTH,NMELNOD)
     
C
C     *****************************************************************C
C     ***            I N C    S L N   P H A S E                      **C
C     *****************************************************************C

      DT=TM/(NINCR-1)

C     INITIALIZE ACCELERATION
C     KINC=2 equivalent to initial conditions

      KINC=2
C
      CALL NODSTFFASEMINI(UT,NUMNP,NUMEL,NUMAT,NNE,NMNE,
     1                 NDOFDIM,NMDOFEL,IELT,IELCON,ILIST,LPLIST,
     2                 NIEL,NDOFN,NDOFEL,MATP,NMATP,NMPR,NIMTP,
     3                 NIPR,AMATE,IMTEI,AWAVE,IWAVE,COORD,LM,ID,
     4                 IDPL,NPL,ITPL,NEQ,IOUT,DT,NINCR,KINC,AG,VG,NTH,
     5				   NMELNOD,NPINCO,LINCO,INCOMI,ILOAD)

	  OPEN(UNIT=ILOAD,FILE=FILENAME(1:LST)//".load")
C$ CALL OMP_SET_NUM_THREADS(NTH)
C$OMP PARALLEL
C$OMP DO
      DO I=1, NEQ
        UTMINT(I)=UT(I)-DT*VG(I)+(DT*DT/2.0D0)*AG(I)
      END DO
C$OMP END DO

C$OMP SECTIONS

C$OMP SECTION
      ALLOCATE(UTMAST(NEQ))
      UTMAST=0.0D0

C$OMP SECTION
      DEALLOCATE(AG)

C$OMP SECTION
      DEALLOCATE(VG)

C$OMP END SECTIONS

C$OMP END PARALLEL
C     INCREMENTATION BEGINS

      III=0
      JJJ=1

	  CALL SYSTEM_CLOCK(TCLOCK3)

      DO KINC=2,NINCR-1
      	
        CALL NODSTFFASEM(UTMINT,UT,UTMAST,NUMNP,NUMEL,NUMAT,NNE,NMNE,
     1                   NDOFDIM,NMDOFEL,IELT,IELCON,ILIST,LPLIST,
     2                   NIEL,NDOFN,NDOFEL,MATP,NMATP,NMPR,NIMTP,
     3                   NIPR,AMATE,IMTEI,AWAVE,IWAVE,COORD,LM,ID,
     4                   IDPL,NPL,ITPL,NEQ,IOUT,DT,NINCR,KINC,NTH,
     5					 NMELNOD,NPINCO,LINCO,INCOMI,ILOAD)

        III=III+1
        IF (III==IPHOTO) THEN
            CALL ESCVTK(NMNE,NUMNP,NUMPARA,NEQ,NDOFDIM,JJJ,COORD,
     1                  IELCON,IELT,ID,NUMEL,UTMAST)
            JJJ=JJJ+1
            III=0
        END IF

        IF (NIMP.NE.0) THEN
        	IF(NDOFDIM==2) THEN
	  		CALL IMPISTOUT2D(NIMP,ISTOUT,NEQ,NDOFDIM,NUMNP,ID,
     1						 UTMINT,UT,UTMAST,IDISP,IVEL,IACEL,DT)
        	END IF
        	
        	IF(NDOFDIM==3) THEN
            CALL IMPISTOUT3D(NIMP,ISTOUT,NEQ,NDOFDIM,NUMNP,ID,
     1						 UTMINT,UT,UTMAST,IDISP,IVEL,IACEL,DT)
            END IF
        END IF

	  	CALL ELAPSED(TCLOCK3, KINC)

      END DO

      WRITE(*,3050) FILENAME
      
      IF (NIMP.NE.0)THEN
        CLOSE(IDISP)
        CLOSE(IVEL)
        CLOSE(IACEL)
      END IF

      IF (NPL.NE.0) THEN
        CLOSE(ILOAD)
      END IF

      IF (IWAVE==3) THEN
	  	CLOSE(INCOMI)
      END IF

      WRITE(*,*)
      CALL SYSTEM_CLOCK(TCLOCK2, CLOCK_RATE)
      ELAPSED_TIME = FLOAT(TCLOCK2 - TCLOCK1) / FLOAT(CLOCK_RATE)
      PRINT 11, ELAPSED_TIME
   11 FORMAT("Elapsed time = ",f12.2, " seconds")

      STOP

C     *****************************************************************C
C     ***         S L N  O U T P U T  P H A S E                      **C
C     *****************************************************************C

 1900 FORMAT(//,10X,'P R O B L E M   N A M E',10X,A80,//)
 2000 FORMAT (///,
     1    ' C O N T R O L  I N F O R M A T I O N',//,
     2    '   NUMBER OF NODAL POINTS',10(' .'),' (NUMNP)=',I5,//,
     3    '   NUMBER OF ELEMENTS',12(' .'),'(NUMEL)=',I5,//,
     4    '   NUMBER OF MATERIALS       ',9(' .'),'(NUMAT)=',I5,//,
     5    '   SIZE OF THE TIME WINDOW',9(' .'),'(TM)=',F10.5,//,
     6    '   NUMBER OF LOAD INCREMENTS',9(' .'),'(NINCR)=',I5,//,
     7    '   DEGREE OF FREEDOM DIMENSION',6(' .'),'(NDOFDIM)=',I1,//,
     8    '   MAX.NUMBER OF NODES IN AN ELE.',6(' .'),'(NMNE)=',I2,//,
     9    '   MAX.NUMBER OF DOF IN AN ELE.',6(' .'),'(NMDOFEL)=',I2,//,
     1    '   MAX.NUMBER OF MAT PROPERTIES.',6(' .'),'(NMPR)=',I2,//,
     2    '   MAX.NUMBER OF INT MATP ROPERTIES.',6(' .'),'(NIPR)=',I2,//,
     3    '   MAX.NUMBER OF POINT LOADS.',6(' .'),'(NPL)=',I2)
 
 3000 FORMAT(1X,'INCREMENT=',I5,5X,'ITERATION=',I5,3X,'ERROR E1=',
     1       3X,F12.7)
 3005 FORMAT(1X,'INCREMENT=',I5,5X,'ITERATION=',I5,3X,'ERROR E3=',
     1       3X,F12.7)
 3016 FORMAT(1X,'TIME STEP COMPLETED=',2X,F5.2,2X,'TOT.TIME COMPLETED
     1       =',2X,F5.2,/)
 3020 FORMAT(5X,I5,10X,I5)
 4000 FORMAT(2X,'INC',6X,'TOT-TIME',6X,'INC-TIME')
 4010 FORMAT(2X,I3,3X,I3,7x,F6.3,9X,F6.3)
 3050 FORMAT(/,'DAMIAN JOB=',A8,1X,'COMPLETED')

      END PROGRAM
