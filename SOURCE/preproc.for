C      MODULE PREPROC
C      CONTAINS
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C SUBROUTINE NODINP                                                    C
C                                                                      C
C   NUMNP      :Numberof nodal points                                  C 
C   N          :Nodal identifier                                       C
C   NDOFN(N)   :Number of degrees of freedom at the current node       C
C   ID(II,I)   :Boundary condition identifiers at the current node     C
C   COORD(JJ,I):Coordinates of the current node                        C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE NODINP(NUMNP,ID,MXDOFDIM,COORD,NDOFN,NEQ,IIN,IOUT)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DIMENSION ID(MXDOFDIM,NUMNP),COORD(MXDOFDIM,NUMNP),NDOFN(NUMNP)
C
C     READ AND GENERATE NODAL POINT DATA
C
      WRITE(IOUT,1000)
      WRITE(IOUT,1005)
C
      DO I=1,NUMNP
        READ(IIN,     *) N,NDOFN(N),(ID(II,I),II=1,NDOFN(N)),
     1  (COORD(JJ,I),JJ=1,MXDOFDIM)
C       WRITE(IOUT,1010)N,NDOFN(N),(ID(II,I),II=1, MXDOFDIM),
C    1  (COORD(JJ,I),JJ=1,MXDOFDIM)
      END DO
C
C     ASSIGN EQUATION NUMBERS TO ACTIVE DOF
C
      ICOUNT=1
      DO K1=1,NUMNP
        DO K2=1,NDOFN(K1)
          IF(ID(K2,K1).EQ.0) THEN
            ID(K2,K1)=ICOUNT
            ICOUNT=ICOUNT+1
          ELSE
            ID(K2,K1)=0
          END IF
        END DO
      END DO
      NEQ=ICOUNT-1
C
C     WRITE EQUATION NUMBERS
C
      WRITE(IOUT,1020)
      WRITE(IOUT,1025)
      WRITE(IOUT,1030) (N,(ID(I,N),I=1,MXDOFDIM),N=1,NUMNP)
C
 1000 FORMAT(//,6X,' N O D A L  D A T A',//)
 1005 FORMAT('ID',2X,'NDOF',2X,'BC-X',2X,'BC-Y',2X,6X,'COORD-X',6X,
     1       'COORD-Y',/)
 1010 FORMAT(I3,X,I2,4X,I2,4X,I2,4X,I2,6X,F5.2,6X,F5.2,6X,F5.2)
 1020 FORMAT(/,'E Q U A T I O N  N U M B E R S',/)
 1025 FORMAT(/,'NODE',6X,'DEGREES OF FREEDOM',/,
     1      'NUMBER',8X,'U',9X,'V'/)
 1030 FORMAT(I5,5X,I5,5X,I5,5X,I5)
C
      RETURN
C
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C SUBROUTINE MATINP                                                    C
C                                                                      C
C   N       :ID of material profile                                    C
C   NMATP() :Number of real material parameters at the current profile C
C   NIMTP() :Number of integer material parmt´s at the current profile C
C   AMATE(,):Array with real material parameters for each profile      C
C   IMTEI(,):Array with integer material parameters for each profile   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE MATINP(NUMAT,NMPR,NMATP,AMATE,NIPR,NIMTP,IMTEI,AWAVE,
     1                  IWAVE,IIN,IOUT)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DIMENSION AMATE(NMPR,NUMAT),NMATP(NUMAT),IMTEI(NIPR,NUMAT),
     1          NIMTP(NUMAT),AWAVE(5)
C
C     READS AND GENERATES MATERIAL DATA
C
      WRITE (IOUT,1000)
      WRITE (IOUT,3000)
      DO I=1,NUMAT
        READ  (IIN, *) N,NMATP(N),NIMTP(N),(AMATE(JJ,N),JJ=1,NMATP(N)),
     1                                     (IMTEI(JJ,N),JJ=1,NIMTP(N))
        WRITE (IOUT,3020) N,NMATP(N),NIMTP(N),(AMATE(JJ,N),JJ=1,
     1                    NMATP(N))
      END DO
C
      WRITE(IOUT,3025)
      WRITE(IOUT,3030)
      READ(IIN,     *) IWAVE, AWAVE(1),AWAVE(2),AWAVE(3),AWAVE(4),
     1                 AWAVE(5)
      WRITE (IOUT,3035) IWAVE,AWAVE(1),AWAVE(2),AWAVE(3),AWAVE(4),
     1                  AWAVE(5)
C
 1000 FORMAT(//,4X,'M A T E R I A L  D A T A',/)
 3000 FORMAT('Mat-Id',2X,'R-Prop',5X,'I-Prop',20X,
     1'Vp',20X,'Vs',12X,'Density',12X,'Alfa',16X,'Beta'//)
 3020 FORMAT(I5,3X,I5,3X,I5,12X,5(5X,F13.3))
 3025 FORMAT(//,4X,'W A V E  D A T A',/)
 3030 FORMAT('Wav-Id',2X,'W-P1',5X,'W-P2',5X,'W-P3',5X,'W-P4',5X,
     1       'W-P5'//)
 3035 FORMAT(I5,3X,5(5X,F10.5))
C
      RETURN
C
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE ELEINP                                                C
C                                                                      C
C     NUMNP     :Number of nodal points                                C
C     NUMEL     :Number of elements                                    C
C     NNE()     :Number of nodes at the current element                C
C     IELT()    :Array of element type identifiers                     C
C     NDOFEL()  :Number of degrees of freedom at the current element   C
C     NMNE      :Maximum number of nodes per element                   C
C     MATP()    :Material pofile at the current element                C
C     IELCON(,) :Element connectivity array                            C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE ELEINP(NUMNP,NUMEL,NNE,IELT,NDOFEL,NMNE,MATP,IELCON,
     1                  NUMPARA,IIN,IOUT)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DIMENSION NNE(NUMEL),IELT(NUMEL),MATP(NUMEL),IELCON(NMNE,NUMEL),
     1          NDOFEL(NUMEL)
C
C     Creates IELCON()
C
      WRITE(IOUT,1000)
      WRITE(IOUT,1001)
      WRITE(IOUT,1002)

      NUMPARA=0
      DO I=1,NUMEL
        READ(IIN,*) M,IELT(M),NDOFEL(M),MATP(M),NNE(M),
     1  (IELCON(J,M),J=1,NNE(M))
     
C       IF(IELT(I).NE.7 .OR. IELT(I).NE.8 .OR. IELT(I).NE.10) THEN
C           NUMPARA=NUMPARA+1
C       END IF
        
        IF(IELT(I).NE.7) THEN
        	IF(IELT(I).NE.8) THEN
        		IF(IELT(I).NE.10) THEN
            		NUMPARA=NUMPARA+1
            	END IF
            END IF
        END IF
        
        WRITE(IOUT,1010) M,IELT(M),NDOFEL(M),MATP(M),NNE(M),
     1  (IELCON(J,M),J=1,NNE(M))
      END DO
C
 1000 FORMAT(///,8X,'E L E M E N T  I N F O R M A T I O N',//)
 1001 FORMAT('ID  TYPE NDOF MAT NNEL NODE NODE NODE NODE NODE NODE
     1 NODE NODE')
 1002 FORMAT(25X'1    2    3    4    5    6   7    8',/)
 1010 FORMAT(5(2X,I4),X,8(2X,I3))
C
      RETURN
C
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C    SUBROUTINE POINTLOAD(NUMNP,NDOFDIM,NDOFN,ID,IDPL,IIN,IOUT)        C
C                                                                      C
C     NUMNP     :Number of nodal points                                C
C     NDOFDIM   :Problem dimension                                     C
C     NPL       :Number of point loads                                 C
C     ID(,)     :Arrays with BC identifiers                            C
C     IDPL      :Equation number where the load is applied             C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE POINTLOAD(NUMNP,NDOFDIM,NPL,ID,IDPL,IIN)

      IMPLICIT REAL*8 (A-H,O-Z)

      DIMENSION ID(NDOFDIM,NUMNP), IDPL(NPL)

      IDPL=0

      IF(NPL.NE.0)THEN

        DO I=1, NPL
            READ(IIN,*) IDNOD, IDIR
            IDPL(I)=ID(IDIR,IDNOD)
        END DO

      END IF

      RETURN

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C SUBROUTINE PLOADS                                                    C
C                                                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE PLOADS(NUMNP,NDOFDIM,NINCR,ID,PR,NEQ,IIN,IOUT)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DIMENSION ID(NDOFDIM,NUMNP),PR(NEQ,NINCR),RICK(NINCR)
C
      CALL CLEAR (PR,NEQ,NINCR)
C
      TH=2.00
      TP=0.25
      NINCRHP=INT(NINCR/4)
C
      READ(IIN,*) IDNOD,IDDOF
      II=ID(IDDOF,IDNOD)
C
      CALL RICKER(TH,TP,NINCRHP,RICK)
      DO JJ=1,NINCR
        PR(II,JJ)=2.0*RICK(JJ)
      END DO
C
      RETURN
C
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C SUBROUTINE IMPHIST                                                   C
C                                                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE IMPHIST(NIMP,ISTOUT,IIN)

      IMPLICIT REAL*8 (A-H,O-Z)

      DIMENSION ISTOUT(NIMP)

      DO I=1, NIMP
        READ(IIN,*) ISTOUT(I)
      END DO

      RETURN

      END

C11111111122222222223333333333444444444455555555556666666666777777777777
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE RICKER(SRick,SRickA,Nf,DF)                            C
C                                                                      C
C     Srick  :Complex valued Fourier coefficients corresponding to     C
C             the Ricker wavelet (Numerically undamped)                C
C     SrickA :Complex valued Fourier coefficients corresponding to     C
C             the Ricker wavelet (Numerically damped)                  C
C                                                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE RICKER(TH,TP,NINCRHP,RICK)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DIMENSION RICK(4*NINCRHP),TTIME(4*NINCRHP+1)
C
      NINCR=4*NINCRHP
C
      PI=4.0D00*DATAN(1.0D0)
C
      TIME=0.0
      DT=TH/NINCRHP
C
      DO I=1,NINCRHP
        A=PI*(TIME-TH)/TP
        RICK(I)=(A*A-1.0D0)*DEXP(-A**2)
        TIME=TIME+DT
      END DO
C
      DO I=1,NINCRHP
        RICK(NINCRHP+I)=RICK(NINCRHP+1-I)
      END DO
      DO I=2*NINCRHP,4*NINCRHP
        RICK(I)=0.0
      END DO
C
      RETURN
C
      END
C
C11111111122222222223333333333444444444455555555556666666666777777777777
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE RWAVLET(SRick,SRickA,Nf,DF)                           C
C                                                                      C
C     Srick  :Complex valued Fourier coefficients corresponding to     C
C             the Ricker wavelet (Numerically undamped)                C
C     SrickA :Complex valued Fourier coefficients corresponding to     C
C             the Ricker wavelet (Numerically damped)                  C
C                                                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE RWAVLET(Rick,Nf,Tmax,fc,Tini)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DIMENSION Rick(2*Nf+1)
C
      PI=4.0D00*DATAN(1.0D0)
C
      fmax=Nf/Tmax
      df=fmax/Nf
      dt=Tmax/2.0/Nf
      dtao=(Tmax*fc)/2.0/Nf
      Nmed=(Tini*fc+1)/dtao+1
      Nt=2*Nf+1
C
      DO I=1,Nt
        tao=dtao*(I-Nmed)
        Rick(I)=(2.*(PI*tao)**2-1.)*DEXP(-(PI*tao)**2)
        t=(I-1)*dt
      END DO
C
      RETURN
C
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE UPULSE(Rick,SRick,Nf,Tmax,fc,Tini)                    C
C                                                                      C
C     AWAVE():ARRAYS CONTAINING THE PULSE DEFINITION PARAMETERS        C
C                                                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE UPULSE(PP,NT,DT,AWAVE,KINC)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION RICK(NT),AWAVE(5)

      PARAMETER (HALF=0.5D0,TWO=2.0D0)

      PI=4.0D00*DATAN(1.0D0)

      Tmax=AWAVE(1)
      Tini=AWAVE(2)
      Fc=AWAVE(3)
      AMP=AWAVE(4)

      TAO=(DT*(KINC-1)-Tini)
      PP=AMP*(2.0D0*(PI*TAO*Fc)**2-1.0D0)*DEXP(-(PI*TAO*Fc)**2)

      RETURN
C
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE UPULSE(Rick,SRick,Nf,Tmax,fc,Tini)                    C
C                                                                      C
C     AWAVE():ARRAYS CONTAINING THE PULSE DEFINITION PARAMETERS        C
C                                                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE ELECHON(PP,NT,DT,AWAVE,KINC)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION RICK(NT),AWAVE(5)
C
      PARAMETER (HALF=0.5D0,TWO=2.0D0)
C
      IF (KINC<21) THEN
        PP=AWAVE(4)
      ELSE
        PP=0.0D0    
      END IF
C
      RETURN
C
      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE UPULSED(Rick,SRick,Nf,Tmax,fc,Tini)                   C
C     DISPLACEMENT FIELD DUE TO A RICKER PULSE                         C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE UPULSED(PPD,NT,DT,AWAVE,KINC)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION AWAVE(5)

      PARAMETER (HALF=0.5D0,TWO=2.0D0)

      PI=4.0D00*DATAN(1.0D0)

      Tmax=AWAVE(1)
      Tini=AWAVE(2)
      fc=AWAVE(3)
C     AMP=AWAVE(4)

      tao=PI*fc*(DT*(KINC-1)-Tini)
      PPD=(2.0D0*tao**2-1.0D0)*DEXP(-tao**2)

      RETURN

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE UPULSEV(Rick,SRick,Nf,Tmax,fc,Tini)                   C
C     VELOCITY FIELD DUE TO A RICKER PULSE                             C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE UPULSEV(PPV,NT,DT,AWAVE,KINC)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION AWAVE(5)

      PARAMETER (HALF=0.5D0,TWO=2.0D0)

      PI=4.0D00*DATAN(1.0D0)

      Tmax=AWAVE(1)
      Tini=AWAVE(2)
      fc=AWAVE(3)
C     AMP=AWAVE(4)

      tao=PI*fc*(DT*(KINC-1)-Tini)

      PPV=2.0D0*(PI*fc)**2*(DT*(KINC-1)-Tini)*(3.0D0-2.0D0*tao**2)
     1    *DEXP(-tao**2)

      RETURN

      END
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE UPULSEA(Rick,SRick,Nf,Tmax,fc,Tini)                   C
C     ACELERATION FIELD DUE TO A RICKER PULSE                          C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE UPULSEA(PPA,NT,DT,AWAVE,KINC)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION AWAVE(5)

      PARAMETER (HALF=0.5D0,TWO=2.0D0)

      PI=4.0D00*DATAN(1.0D0)

      Tmax=AWAVE(1)
      Tini=AWAVE(2)
      fc=AWAVE(3)
C     AMP=AWAVE(4)

      tao=PI*fc*(DT*(KINC-1)-Tini)

      PPA=2.0D0*(PI*fc)**2*(3.0D0-12.0D0*tao**2+4.0D0*tao**4)
     1   *DEXP(-tao**2)

      RETURN

      END
C
C      END MODULE PREPROC