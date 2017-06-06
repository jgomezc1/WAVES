C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C11111111122222222223333333333444444444455555555556666666666777777777777
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C--C O N S T I T U T I V E  M A T E R I A L S  S U B R O U T I N E S---C
C                                                                      C
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C11111111122222222223333333333444444444455555555556666666666777777777777
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C     SUBROUTINE UMAT                                                  C
C     UMAT.for   Classical Isotropic Elasticity                        C
C     UNIVERSIDAD EAFIT                                                C
C     2011                                                             C
C     CANNOT BE USED FOR PLANE STRESS                                  C
C     NTENS: LENGTH OF STRESS VECTOR                                   C
C     NDI:   NUMBER OF NORMAL STRESS COMPONENTS                        C
C                                                                      C
C     PROPS(1) - E                                                     C
C     PROPS(2) - NU                                                    C
C                                                                      C
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE UMAT(DDSDDE,NDI, PROPS,NPROPS)
C
      IMPLICIT REAL*8(A-H,O-Z)
C 
      DIMENSION DDSDDE(NDI, NDI),PROPS(NPROPS)
C
      PARAMETER (ZERO=0.D0, ONE=1.D0, TWO=2.D0, THREE=3.D0, SIX=6.0D0,
     1           ENUMAX=0.4999D0)
C
C**********************************************************************
C     I S O T R O P I C   E L A S T I C I T Y
C     P L A N E  S T R A I N  A N A L Y S I S
C**********************************************************************
C
C     Wave propagation velocities.
C
      VP=PROPS(1)
      VS=PROPS(2)
      RO=PROPS(3)
C
      VPS=VP*VP
      VSS=VS*VS
C
C     Elasticity tensor.
C

	  DDSDDE(1,1)= VPS
	  DDSDDE(1,2)= VPS-2.0D0*VSS
	  DDSDDE(2,1)= VPS-2.0D0*VSS
	  DDSDDE(2,2)= VPS
	  DDSDDE(3,3)= VSS

	  DDSDDE=DDSDDE*RO
C
      RETURN
C
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C     SUBROUTINE UMAT3D                                                C
C     UMAT.for   Classical Isotropic Elasticity                        C
C     UNIVERSIDAD EAFIT                                                C
C     2011                                                             C
C     CANNOT BE USED FOR PLANE STRESS                                  C
C     NTENS: LENGTH OF STRESS VECTOR (6 fo 3D problems)                C
C     NDI:   NUMBER OF NORMAL STRESS COMPONENTS (3 for 3D problems)    C
C                                                                      C
C     PROPS(1) - alpha                                                 C
C     PROPS(2) - beta                                                  C
C     PROPS(3) - ro                                                    C
C                                                                      C
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE UMAT3D(DDSDDE,NDI,NTENS,PROPS,NPROPS)
C
      IMPLICIT REAL*8(A-H,O-Z)
C 
      DIMENSION DDSDDE(NTENS, NTENS),PROPS(NPROPS)
C
      PARAMETER (ZERO=0.D0, ONE=1.D0, TWO=2.D0, THREE=3.D0, SIX=6.0D0,
     1           ENUMAX=0.4999D0)
C
C**********************************************************************
C     I S O T R O P I C   E L A S T I C I T Y
C     P L A N E  S T R A I N  A N A L Y S I S
C**********************************************************************
C
C     Wave propagation velocities.
C
      VP=PROPS(1)
      VS=PROPS(2)
      RO=PROPS(3)
C
      VPS=VP*VP
      VSS=VS*VS
C
C     Elasticity tensor.
C
      DO K1=1, NDI
        DO K2=1, NDI
          DDSDDE(K2, K1)=(VPS-2.0D0*VSS)*RO
        END DO
        DDSDDE(K1, K1)=VPS*RO
      END DO
      DO K1=NDI+1, NTENS
        DDSDDE(K1, K1)=VSS*RO
      END DO
C
      RETURN
C
      END
C