cc      MODULE UELUTIL
cc      CONTAINS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C11111111122222222223333333333444444444455555555556666666666777777777777
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C       --U S E R   E L E M E N T    S U B R O U T I N E S---          C
C                                                                      C
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C  UELEXP9.for                                                         C
C  9-NODED ISOPARAMETRIC ELEMENT WITH FULL GAUSS INTEGRATION           C
C  USER ELEMENT SUBROUTINE FOR EXPLICIT DYNAMIC ANALYSIS               C
C                                                                      C
C  GIVEN THE DISPLACEMENT AT TIME T AND THE DISPLACEMENT INCREMENT     C
C  IT RETURNS IN THE RHS() VECTOR THE INERTIAL FORCE CONTRIBUTION      C
C  AT TIME T+DT AND IN THE ANMASS() VECTOR THE DIAGONALIZED MASS       C
C  MATRIX                                                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE UELEXP9(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,
     1                   AWAVE,IWAVE,COORDS,MCRD,NNODE,U,DU,V,A,JELEM,
     2                   DT,KINC,NINCR)
     
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,TWO=2.D0,NGPTS=9,
     1           THREE=3.D0, NDI=3)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),U(NDOFEL),
     2          DU(NDOFEL),V(NDOFEL),A(NDOFEL),AMATRX(NDOFEL,NDOFEL),
     3          AMASS(NDOFEL,NDOFEL),CDMAT(NDOFEL,NDOFEL),UP(NDOFEL),
     4          DDSDDE(NDI,NDI),B(NDI,NDOFEL),BT(NDOFEL,NDI),
     5          FRST2(NDOFEL,NDOFEL),FRST1(NDOFEL,NDOFEL),XP(2,NGPTS),
     6          XW(NGPTS),AUX1(NDI,NDOFEL)

C     Mass density.

      RO=PROPS(3)

C     Rayleigh damping parameters.

      AALFA=PROPS(1)
      ABETA=PROPS(2)

      CALL CLEARV(RHS,NDOFEL)
      CALL CLEARV(ANMASS,NDOFEL)
      CALL CLEAR(DDSDDE,NDI,NDI)
      CALL CLEAR(B,NDI,NDOFEL)
      CALL CLEAR(BT,NDOFEL,NDI)
      CALL CLEAR(FRST2,NDOFEL,NDOFEL)
      CALL CLEAR(FRST1,NDOFEL,NDOFEL)
      CALL CLEAR(XP,2,NGPTS)
      CALL CLEARV(XW,NGPTS)
      CALL CLEAR(AUX1,NDI,NDOFEL)
      CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      CALL CLEAR(AMASS,NDOFEL,NDOFEL)
      CALL CLEAR(CDMAT,NDOFEL,NDOFEL)

C       Elasticity tensor.

	  CALL UMAT(DDSDDE,NDI,PROPS,NPROPS)

C     Generates Gauss points and weights
C     and loops around all Gauss points.

      CALL GPOINTS3X3(XP,XW)
      NGPT=9
      SMS=0.0D0
      SMT=0.0D0

      DO NN=1,NGPT

        RII=XP(1,NN)
        SII=XP(2,NN)
        ALF=XW(NN)

C       Computes B matrix.

        CALL STDM9(NNODE,NDOFEL,NDI,COORDS,MCRD,B,DDET,RII,SII,
     1             XBAR)
        SMT=SMT+RO*DDET*XBAR*ALF

C       Assembles stiffness and mass matrix.

        CALL MMULT(DDSDDE,NDI,NDI,B,NDI,NDOFEL,AUX1)
        CALL CLEAR(BT,NDOFEL,NDI)
        CALL MTRAN(B,NDI,NDOFEL,BT)
        CALL MMULT(BT,NDOFEL,NDI,AUX1,NDI,NDOFEL,FRST1)
C
        CALL AMASS9(FRST2,NDOFEL,RII,SII)

C       Considers Gauss weight and Jacobian determinant
C       into partial stiffness and mass matrix.

        CALL SMULT(FRST1,NDOFEL,NDOFEL,ALF*DDET*XBAR)
        CALL SMULT(FRST2,NDOFEL,NDOFEL,ALF*DDET*XBAR*RO)

C       Updates stiffness and mass matrix.

        CALL UPDMAT(AMATRX,NDOFEL,NDOFEL,FRST1)
        CALL UPDMAT(AMASS ,NDOFEL,NDOFEL,FRST2)

      END DO

C     Computes elastic,inertial and damping nodal forces.

      CALL EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,
     1                    AALFA,ABETA,U,V,A,DT,SMT,RHS,JELEM)

      RETURN

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C  UELEXP8.for                                                         C
C  8-NODED ISOPARAMETRIC ELEMENT WITH FULL GAUSS INTEGRATION           C
C  USER ELEMENT SUBROUTINE FOR EXPLICIT DYNAMIC ANALYSIS               C
C                                                                      C
C  GIVEN THE DISPLACEMENT AT TIME T AND THE DISPLACEMENT INCREMENT     C
C  IT RETURNS IN THE RHS() VECTOR THE INERTIAL FORCE CONTRIBUTION      C
C  AT TIME T+DT AND IN THE ANMASS() VECTOR THE DIAGONALIZED MASS       C
C  MATRIX                                                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE UELEXP8(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,
     1                   AWAVE,IWAVE,COORDS,MCRD,NNODE,U,DU,V,A,JELEM,
     2                   DT,KINC,NINCR)
     
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,TWO=2.D0,NGPTS=9,
     1           THREE=3.D0, NDI=3)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),U(NDOFEL),
     2          DU(NDOFEL),V(NDOFEL),A(NDOFEL),AMATRX(NDOFEL,NDOFEL),
     3          AMASS(NDOFEL,NDOFEL),CDMAT(NDOFEL,NDOFEL),UP(NDOFEL),
     4          DDSDDE(NDI,NDI),B(NDI,NDOFEL),BT(NDOFEL,NDI),
     5          FRST2(NDOFEL,NDOFEL),FRST1(NDOFEL,NDOFEL),XP(2,NGPTS),
     6          XW(NGPTS),AUX1(NDI,NDOFEL)

C     Mass density.

      RO=PROPS(3)

C     Rayleigh damping parameters.

      AALFA=PROPS(1)
      ABETA=PROPS(2)

      CALL CLEARV(RHS,NDOFEL)
      CALL CLEARV(ANMASS,NDOFEL)
      CALL CLEAR(DDSDDE,NDI,NDI)
      CALL CLEAR(B,NDI,NDOFEL)
      CALL CLEAR(BT,NDOFEL,NDI)
      CALL CLEAR(FRST2,NDOFEL,NDOFEL)
      CALL CLEAR(FRST1,NDOFEL,NDOFEL)
      CALL CLEAR(XP,2,NGPTS)
      CALL CLEARV(XW,NGPTS)
      CALL CLEAR(AUX1,NDI,NDOFEL)
      CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      CALL CLEAR(AMASS,NDOFEL,NDOFEL)
      CALL CLEAR(CDMAT,NDOFEL,NDOFEL)

C       Elasticity tensor.

	  CALL UMAT(DDSDDE,NDI,PROPS,NPROPS)
	  
C     Generates Gauss points and weights
C     and loops around all Gauss points.

      CALL GPOINTS3X3(XP,XW)
      NGPT=9
      SMS=0.0D0
      SMT=0.0D0

      DO NN=1,NGPT

        RII=XP(1,NN)
        SII=XP(2,NN)
        ALF=XW(NN)

C       Computes B matrix.

        CALL STDM8(NNODE,NDOFEL,NDI,COORDS,MCRD,B,DDET,RII,SII,
     1             XBAR)
        SMT=SMT+RO*DDET*XBAR*ALF

C       Assembles stiffness and mass matrix.

        CALL MMULT(DDSDDE,NDI,NDI,B,NDI,NDOFEL,AUX1)
        CALL CLEAR(BT,NDOFEL,NDI)
        CALL MTRAN(B,NDI,NDOFEL,BT)
        CALL MMULT(BT,NDOFEL,NDI,AUX1,NDI,NDOFEL,FRST1)
C
        CALL AMASS8(FRST2,NDOFEL,RII,SII)

C       Considers Gauss weight and Jacobian determinant
C       into partial stiffness and mass matrix.

        CALL SMULT(FRST1,NDOFEL,NDOFEL,ALF*DDET*XBAR)
        CALL SMULT(FRST2,NDOFEL,NDOFEL,ALF*DDET*XBAR*RO)

C       Updates stiffness and mass matrix.

        CALL UPDMAT(AMATRX,NDOFEL,NDOFEL,FRST1)
        CALL UPDMAT(AMASS ,NDOFEL,NDOFEL,FRST2)

      END DO

C     Computes elastic,inertial and damping nodal forces.

      CALL EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,
     1                    AALFA,ABETA,U,V,A,DT,SMT,RHS,JELEM)

      RETURN

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C  UELEXP6.for                                                         C
C  6-NODED ISOPARAMETRIC ELEMENT WITH FULL GAUSS INTEGRATION           C
C  USER ELEMENT SUBROUTINE FOR EXPLICIT DYNAMIC ANALYSIS               C
C                                                                      C
C  GIVEN THE DISPLACEMENT AT TIME T AND THE DISPLACEMENT INCREMENT     C
C  IT RETURNS IN THE RHS() VECTOR THE INERTIAL FORCE CONTRIBUTION      C
C  AT TIME T+DT AND IN THE ANMASS() VECTOR THE DIAGONALIZED MASS       C
C  MATRIX                                                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE UELEXP6(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,
     1                   AWAVE,IWAVE,COORDS,MCRD,NNODE,U,DU,V,A,JELEM,
     2                   DT,KINC,NINCR)
     
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,TWO=2.D0,NGPTS=7,
     1           THREE=3.D0, NDI=3)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),U(NDOFEL),
     2          DU(NDOFEL),V(NDOFEL),A(NDOFEL),AMATRX(NDOFEL,NDOFEL),
     3          AMASS(NDOFEL,NDOFEL),CDMAT(NDOFEL,NDOFEL),UP(NDOFEL),
     4          DDSDDE(NDI,NDI),B(NDI,NDOFEL),BT(NDOFEL,NDI),
     5          FRST2(NDOFEL,NDOFEL),FRST1(NDOFEL,NDOFEL),XP(2,NGPTS),
     6          XW(NGPTS),AUX1(NDI,NDOFEL)

C     Mass density.

      RO=PROPS(3)

C     Rayleigh damping parameters.

      AALFA=PROPS(1)
      ABETA=PROPS(2)

      CALL CLEARV(RHS,NDOFEL)
      CALL CLEARV(ANMASS,NDOFEL)
      CALL CLEAR(DDSDDE,NDI,NDI)
      CALL CLEAR(B,NDI,NDOFEL)
      CALL CLEAR(BT,NDOFEL,NDI)
      CALL CLEAR(FRST2,NDOFEL,NDOFEL)
      CALL CLEAR(FRST1,NDOFEL,NDOFEL)
      CALL CLEAR(XP,2,NGPTS)
      CALL CLEARV(XW,NGPTS)
      CALL CLEAR(AUX1,NDI,NDOFEL)
      CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      CALL CLEAR(AMASS,NDOFEL,NDOFEL)
      CALL CLEAR(CDMAT,NDOFEL,NDOFEL)

C       Elasticity tensor.

	  CALL UMAT(DDSDDE,NDI,PROPS,NPROPS)

C     Generates Gauss points and weights
C     and loops around all Gauss points.

      CALL GPOINTS7(XP,XW)
      NGPT=7
      SMS=0.0D0
      SMT=0.0D0

      DO NN=1,NGPT

        RII=XP(1,NN)
        SII=XP(2,NN)
        ALF=XW(NN)

C       Computes B matrix.

        CALL STDM6(NNODE,NDOFEL,NDI,COORDS,MCRD,B,DDET,RII,SII,
     1             XBAR)
        SMT=SMT+RO*DDET*XBAR*ALF

C       Assembles stiffness and mass matrix.

        CALL MMULT(DDSDDE,NDI,NDI,B,NDI,NDOFEL,AUX1)
        CALL CLEAR(BT,NDOFEL,NDI)
        CALL MTRAN(B,NDI,NDOFEL,BT)
        CALL MMULT(BT,NDOFEL,NDI,AUX1,NDI,NDOFEL,FRST1)
C
        CALL AMASS6(FRST2,NDOFEL,RII,SII)

C       Considers Gauss weight and Jacobian determinant
C       into partial stiffness and mass matrix.

        CALL SMULT(FRST1,NDOFEL,NDOFEL,ALF*DDET*XBAR)
        CALL SMULT(FRST2,NDOFEL,NDOFEL,ALF*DDET*XBAR*RO)

C       Updates stiffness and mass matrix.

        CALL UPDMAT(AMATRX,NDOFEL,NDOFEL,FRST1)
        CALL UPDMAT(AMASS ,NDOFEL,NDOFEL,FRST2)

      END DO

C     Computes elastic,inertial and damping nodal forces.

      CALL EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,
     1                    AALFA,ABETA,U,V,A,DT,SMT,RHS,JELEM)

      RETURN

      END


C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C  UELEXP4.for                                                         C
C  4-NODED ISOPARAMETRIC ELEMENT WITH FULL GAUSS INTEGRATION           C
C  USER ELEMENT SUBROUTINE FOR EXPLICIT DYNAMIC ANALYSIS               C
C                                                                      C
C  GIVEN THE DISPLACEMENT AT TIME T AND THE DISPLACEMENT INCREMENT     C
C  IT RETURNS IN THE RHS() VECTOR THE INERTIAL FORCE CONTRIBUTION      C
C  AT TIME T+DT AND IN THE ANMASS() VECTOR THE DIAGONALIZED MASS       C
C  MATRIX                                                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE UELEXP4(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,
     1                   AWAVE,IWAVE,COORDS,MCRD,NNODE,U,DU,V,A,JELEM,
     2                   DT,KINC,NINCR)
     
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,TWO=2.D0,NGPTS=4,
     1           THREE=3.D0, NDI=3)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),U(NDOFEL),
     2          DU(NDOFEL),V(NDOFEL),A(NDOFEL),AMATRX(NDOFEL,NDOFEL),
     3          AMASS(NDOFEL,NDOFEL),CDMAT(NDOFEL,NDOFEL),UP(NDOFEL),
     4          DDSDDE(NDI,NDI),B(NDI,NDOFEL),BT(NDOFEL,NDI),
     5          FRST2(NDOFEL,NDOFEL),FRST1(NDOFEL,NDOFEL),XP(2,NGPTS),
     6          XW(NGPTS),AUX1(NDI,NDOFEL)

C     Mass density.

      RO=PROPS(3)
C
C     Rayleigh damping parameters.

      AALFA=PROPS(1)
      ABETA=PROPS(2)

      CALL CLEARV(RHS,NDOFEL)
      CALL CLEARV(ANMASS,NDOFEL)
      CALL CLEAR(DDSDDE,NDI,NDI)
      CALL CLEAR(B,NDI,NDOFEL)
      CALL CLEAR(BT,NDOFEL,NDI)
      CALL CLEAR(FRST2,NDOFEL,NDOFEL)
      CALL CLEAR(FRST1,NDOFEL,NDOFEL)
      CALL CLEAR(XP,2,NGPTS)
      CALL CLEARV(XW,NGPTS)
      CALL CLEAR(AUX1,NDI,NDOFEL)
      CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      CALL CLEAR(AMASS,NDOFEL,NDOFEL)
      CALL CLEAR(CDMAT,NDOFEL,NDOFEL)

C       Elasticity tensor.

	  CALL UMAT(DDSDDE,NDI,PROPS,NPROPS)

C     Generates Gauss points and weights
C     and loops around all Gauss points.

      CALL GPOINTS2X2(XP,XW)
      NGPT=4
      SMS=0.0D0
      SMT=0.0D0

      DO NN=1,NGPT

        RII=XP(1,NN)
        SII=XP(2,NN)
        ALF=XW(NN)

C       Computes B matrix.

        CALL STDM4(NNODE,NDOFEL,NDI,COORDS,MCRD,B,DDET,RII,SII,
     1             XBAR)
        SMT=SMT+RO*DDET*XBAR*ALF

C       Assembles stiffness and mass matrix.

        CALL MMULT(DDSDDE,NDI,NDI,B,NDI,NDOFEL,AUX1)
        CALL CLEAR(BT,NDOFEL,NDI)
        CALL MTRAN(B,NDI,NDOFEL,BT)
        CALL MMULT(BT,NDOFEL,NDI,AUX1,NDI,NDOFEL,FRST1)

        CALL AMASS4(FRST2,NDOFEL,RII,SII)

C       Considers Gauss weight and Jacobian determinant
C       into partial stiffness and mass matrix.

        CALL SMULT(FRST1,NDOFEL,NDOFEL,ALF*DDET*XBAR)
        CALL SMULT(FRST2,NDOFEL,NDOFEL,ALF*DDET*XBAR*RO)

C       Updates stiffness and mass matrix.

        CALL UPDMAT(AMATRX,NDOFEL,NDOFEL,FRST1)
        CALL UPDMAT(AMASS ,NDOFEL,NDOFEL,FRST2)

      END DO

C     Computes elastic,inertial and damping nodal forces.

      CALL EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,
     1                    AALFA,ABETA,U,V,A,DT,SMT,RHS,JELEM)

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE UEL9INCOT(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,  C
C    1                   COORDS,MCRD,NNODE,U,DU,V,JELEM,DT)            C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE UEL9INCOT(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,
     1                  NIPROPS,AWAVE,IWAVE,COORDS,MCRD,NNODE,U,DU,V,A,
     2                  JELEM,DT,KINC,NINCR)

      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.D0,HALF=0.5D0,TWO=2.D0,NGPTS=9,
     1           THREE=3.D0,NDI=3,ONE=1.0D0)

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),AWAVE(5),
     1          COORDS(MCRD,NNODE),U(NDOFEL),DU(NDOFEL),V(NDOFEL),
     2          UP(NDOFEL),DDSDDE(NDI,NDI),B(NDI,NDOFEL),
     3          BT(NDOFEL,NDI),FRST2(NDOFEL,NDOFEL),A(NDOFEL),
     4          FRST1(NDOFEL,NDOFEL),XP(2,NGPTS),XW(NGPTS),
     5          AUX1(NDI,NDOFEL),R00(NDOFEL),RELA(NDOFEL),
     6          RINE(NDOFEL),RDAM(NDOFEL),AMATRX(NDOFEL,NDOFEL),
     7          AMASS(NDOFEL,NDOFEL),CDMAT(NDOFEL,NDOFEL)

      DIMENSION IPROPS(NIPROPS)

C     Mass density

      RO=PROPS(3)

C     Rayleigh damping constants.

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      IDF=IPROPS(1)

C     Clears arrays.

      CALL CLEARV(RHS,NDOFEL)
      CALL CLEARV(ANMASS,NDOFEL)
      CALL CLEAR(DDSDDE,NDI,NDI)
      CALL CLEAR(B,NDI,NDOFEL)
      CALL CLEAR(BT,NDOFEL,NDI)
      CALL CLEAR(FRST2,NDOFEL,NDOFEL)
      CALL CLEAR(FRST1,NDOFEL,NDOFEL)
      CALL CLEAR(XP,2,NGPTS)
      CALL CLEARV(XW,NGPTS)
      CALL CLEAR(AUX1,NDI,NDOFEL)
      CALL CLEARV(R00,NDOFEL)
      CALL CLEARV(RELA,NDOFEL)
      CALL CLEARV(RINE,NDOFEL)
      CALL CLEARV(RDAM,NDOFEL)
      CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      CALL CLEAR(AMASS,NDOFEL,NDOFEL)
      CALL CLEAR(CDMAT,NDOFEL,NDOFEL)

C       Elasticity tensor.

	  CALL UMAT(DDSDDE,NDI,PROPS,NPROPS)

C     Generates Gauss points and weights
C     and loops around all Gauss points.

      CALL GPOINTS3X3(XP,XW)
      NGPT=9
      SMS=0.0D0
      SMT=0.0D0

      DO NN=1,NGPT

        RII=XP(1,NN)
        SII=XP(2,NN)
        ALF=XW(NN  )

C       Computes B and mass matrix.

        CALL STDM9(NNODE,NDOFEL,NDI,COORDS,MCRD,B,DDET,RII,SII,
     1             XBAR)
        SMT=SMT+RO*DDET*XBAR*ALF

C       Assembles stiffness and mass matrix.

        CALL MMULT(DDSDDE,NDI,NDI,B,NDI,NDOFEL,AUX1)
        CALL CLEAR(BT,NDOFEL,NDI)
        CALL MTRAN(B,NDI,NDOFEL,BT)
        CALL MMULT(BT,NDOFEL,NDI,AUX1,NDI,NDOFEL,FRST1)

        CALL AMASS9(FRST2,NDOFEL,RII,SII)

C       Considers Gauss weight and Jacobian determinant
C       into partial stiffness and mass matrices.

        CALL SMULT(FRST1,NDOFEL,NDOFEL,ALF*DDET*XBAR)
        CALL SMULT(FRST2,NDOFEL,NDOFEL,ALF*DDET*XBAR*RO)

C       Updates stiffness and mass matrices.

        CALL UPDMAT(AMATRX,NDOFEL,NDOFEL,FRST1)
        CALL UPDMAT(AMASS ,NDOFEL,NDOFEL,FRST2)

      END DO

C     Computes the contribution -Kij*Uj-MII*Ai-alpha*Kij*Vj.....

      CALL EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,
     1                    AALFA,ABETA,U,V,A,DT,SMT,RHS,JELEM)

C     Computes the incoming wave field and mekes it into effective
C     forces R00(n).

      CALL INCOMINGDIS9(IDF,AMATRX,COORDS,PROPS,NPROPS,MCRD,AWAVE,
     1                 IWAVE,NNODE,NDOFEL,R00,DT,KINC,NINCR,IOUT)

C     Updates the effective loads
C     loads vector RHS=R00(n)-Kij*Uj-MII*Ai-alpha*Kij*Vj....

      CALL UPDVEC(RHS,NDOFEL,R00)

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE UEL8INCOT(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,C
C    1                   COORDS,MCRD,NNODE,U,DU,V,JELEM,DT)            C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE UEL8INCOT(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,
     1                  NIPROPS,AWAVE,IWAVE,COORDS,MCRD,NNODE,U,DU,V,A,
     2                  JELEM,DT,KINC,NINCR)

      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.D0,HALF=0.5D0,TWO=2.D0,NGPTS=9,
     1           THREE=3.D0,NDI=3,ONE=1.0D0)

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),AWAVE(5),
     1          COORDS(MCRD,NNODE),U(NDOFEL),DU(NDOFEL),V(NDOFEL),
     2          UP(NDOFEL),DDSDDE(NDI,NDI),B(NDI,NDOFEL),
     3          BT(NDOFEL,NDI),FRST2(NDOFEL,NDOFEL),A(NDOFEL),
     4          FRST1(NDOFEL,NDOFEL),XP(2,NGPTS),XW(NGPTS),
     5          AUX1(NDI,NDOFEL),R00(NDOFEL),RELA(NDOFEL),
     6          RINE(NDOFEL),RDAM(NDOFEL),AMATRX(NDOFEL,NDOFEL),
     7          AMASS(NDOFEL,NDOFEL),CDMAT(NDOFEL,NDOFEL)

      DIMENSION IPROPS(NIPROPS)

C     Mass density

      RO=PROPS(3)

C     Rayleigh damping constants.

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      IDF=IPROPS(1)

C     Clears arrays.

      CALL CLEARV(RHS,NDOFEL)
      CALL CLEARV(ANMASS,NDOFEL)
      CALL CLEAR(DDSDDE,NDI,NDI)
      CALL CLEAR(B,NDI,NDOFEL)
      CALL CLEAR(BT,NDOFEL,NDI)
      CALL CLEAR(FRST2,NDOFEL,NDOFEL)
      CALL CLEAR(FRST1,NDOFEL,NDOFEL)
      CALL CLEAR(XP,2,NGPTS)
      CALL CLEARV(XW,NGPTS)
      CALL CLEAR(AUX1,NDI,NDOFEL)
      CALL CLEARV(R00,NDOFEL)
      CALL CLEARV(RELA,NDOFEL)
      CALL CLEARV(RINE,NDOFEL)
      CALL CLEARV(RDAM,NDOFEL)
      CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      CALL CLEAR(AMASS,NDOFEL,NDOFEL)
      CALL CLEAR(CDMAT,NDOFEL,NDOFEL)

C       Elasticity tensor.

	  CALL UMAT(DDSDDE,NDI,PROPS,NPROPS)

C     Generates Gauss points and weights
C     and loops around all Gauss points.

      CALL GPOINTS3X3(XP,XW)
      NGPT=9
      SMS=0.0D0
      SMT=0.0D0

      DO NN=1,NGPT

        RII=XP(1,NN)
        SII=XP(2,NN)
        ALF=XW(NN  )

C       Computes B and mass matrix.

        CALL STDM8(NNODE,NDOFEL,NDI,COORDS,MCRD,B,DDET,RII,SII,
     1             XBAR)
        SMT=SMT+RO*DDET*XBAR*ALF

C       Assembles stiffness and mass matrix.

        CALL MMULT(DDSDDE,NDI,NDI,B,NDI,NDOFEL,AUX1)
        CALL CLEAR(BT,NDOFEL,NDI)
        CALL MTRAN(B,NDI,NDOFEL,BT)
        CALL MMULT(BT,NDOFEL,NDI,AUX1,NDI,NDOFEL,FRST1)

        CALL AMASS8(FRST2,NDOFEL,RII,SII)

C       Considers Gauss weight and Jacobian determinant
C       into partial stiffness and mass matrices.

        CALL SMULT(FRST1,NDOFEL,NDOFEL,ALF*DDET*XBAR)
        CALL SMULT(FRST2,NDOFEL,NDOFEL,ALF*DDET*XBAR*RO)

C       Updates stiffness and mass matrices.

        CALL UPDMAT(AMATRX,NDOFEL,NDOFEL,FRST1)
        CALL UPDMAT(AMASS ,NDOFEL,NDOFEL,FRST2)

      END DO

C     Computes the contribution -Kij*Uj-MII*Ai-alpha*Kij*Vj.....

      CALL EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,
     1                    AALFA,ABETA,U,V,A,DT,SMT,RHS,JELEM)

C     Computes the incoming wave field and mekes it into effective
C     forces R00(n).

      CALL INCOMINGDIS8(IDF,AMATRX,COORDS,PROPS,NPROPS,MCRD,AWAVE,
     1                 IWAVE,NNODE,NDOFEL,R00,DT,KINC,NINCR,IOUT)

C     Updates the effective loads
C     loads vector RHS=R00(n)-Kij*Uj-MII*Ai-alpha*Kij*Vj....

      CALL UPDVEC(RHS,NDOFEL,R00)

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C   SUBROUTINE UEL4INCOT(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,C
C    1                   COORDS,MCRD,NNODE,U,DU,V,JELEM,DT)            C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE UEL4INCOT(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,
     1                  NIPROPS,AWAVE,IWAVE,COORDS,MCRD,NNODE,U,DU,V,A,
     2                  JELEM,DT,KINC,NINCR)
     
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,TWO=2.D0,NGPTS=4,
     1           THREE=3.D0, NDI=3)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),AWAVE(5),
     1          COORDS(MCRD,NNODE),U(NDOFEL),DU(NDOFEL),V(NDOFEL),
     2          UP(NDOFEL),DDSDDE(NDI,NDI),B(NDI,NDOFEL),
     3          BT(NDOFEL,NDI),FRST2(NDOFEL,NDOFEL),A(NDOFEL),
     4          FRST1(NDOFEL,NDOFEL),XP(2,NGPTS),XW(NGPTS),
     5          AUX1(NDI,NDOFEL),R00(NDOFEL),RELA(NDOFEL),
     6          RINE(NDOFEL),RDAM(NDOFEL),AMATRX(NDOFEL,NDOFEL),
     7          AMASS(NDOFEL,NDOFEL),CDMAT(NDOFEL,NDOFEL)

      DIMENSION IPROPS(NIPROPS)

C     Mass density.

      RO=PROPS(3)
C
C     Rayleigh damping parameters.

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      IDF=IPROPS(1)

C     Clears arrays.

      CALL CLEARV(RHS,NDOFEL)
      CALL CLEARV(ANMASS,NDOFEL)
      CALL CLEAR(DDSDDE,NDI,NDI)
      CALL CLEAR(B,NDI,NDOFEL)
      CALL CLEAR(BT,NDOFEL,NDI)
      CALL CLEAR(FRST2,NDOFEL,NDOFEL)
      CALL CLEAR(FRST1,NDOFEL,NDOFEL)
      CALL CLEAR(XP,2,NGPTS)
      CALL CLEARV(XW,NGPTS)
      CALL CLEAR(AUX1,NDI,NDOFEL)
      CALL CLEARV(R00,NDOFEL)
      CALL CLEARV(RELA,NDOFEL)
      CALL CLEARV(RINE,NDOFEL)
      CALL CLEARV(RDAM,NDOFEL)
      CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      CALL CLEAR(AMASS,NDOFEL,NDOFEL)
      CALL CLEAR(CDMAT,NDOFEL,NDOFEL)

C       Elasticity tensor.

	  CALL UMAT(DDSDDE,NDI,PROPS,NPROPS)

C     Generates Gauss points and weights
C     and loops around all Gauss points.

      CALL GPOINTS2X2(XP,XW)
      NGPT=4
      SMS=0.0D0
      SMT=0.0D0

      DO NN=1,NGPT

        RII=XP(1,NN)
        SII=XP(2,NN)
        ALF=XW(NN)

C       Computes B matrix.

        CALL STDM4(JELEM,NNODE,NDOFEL,NDI,COORDS,MCRD,B,DDET,RII,SII,
     1             XBAR)
        SMT=SMT+RO*DDET*XBAR*ALF

C       Assembles stiffness and mass matrix.

        CALL MMULT(DDSDDE,NDI,NDI,B,NDI,NDOFEL,AUX1)
        CALL CLEAR(BT,NDOFEL,NDI)
        CALL MTRAN(B,NDI,NDOFEL,BT)
        CALL MMULT(BT,NDOFEL,NDI,AUX1,NDI,NDOFEL,FRST1)

        CALL AMASS4(FRST2,NDOFEL,RII,SII)

C       Considers Gauss weight and Jacobian determinant
C       into partial stiffness and mass matrix.

        CALL SMULT(FRST1,NDOFEL,NDOFEL,ALF*DDET*XBAR)
        CALL SMULT(FRST2,NDOFEL,NDOFEL,ALF*DDET*XBAR*RO)

C       Updates stiffness and mass matrix.

        CALL UPDMAT(AMATRX,NDOFEL,NDOFEL,FRST1)
        CALL UPDMAT(AMASS ,NDOFEL,NDOFEL,FRST2)

      END DO

C     Computes elastic,inertial and damping nodal forces.

      CALL EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,
     1                    AALFA,ABETA,U,V,A,DT,SMT,RHS,JELEM)

C     Computes the incoming wave field and mekes it into effective
C     forces R00(n).

      CALL INCOMINGDIS4(IDF,AMATRX,COORDS,PROPS,NPROPS,MCRD,AWAVE,
     1                  IWAVE,NNODE,NDOFEL,R00,DT,KINC,NINCR,IOUT)

C     Updates the effective loads
C     loads vector RHS=R00(n)-Kij*Uj-MII*Ai-alpha*Kij*Vj....

      CALL UPDVEC(RHS,NDOFEL,R00)

      RETURN

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C    SUBROUTINE UELEDASH3(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,C
C    1                    COORDS,MCRD,NNODE,U,DU,V,JELEM,DT)           C
C                                                                      C
C    ABSORBING DASHPOT ELEMENT                                         C
C                                                                      C
C                                                                      C
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE UELEDASH3(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,
     1                    NIPROPS,AWAVE,IWAVE,COORDS,MCRD,NNODE,
     2                    U,DU,V,A,JELEM,DT,KINC,NINCR)

      IMPLICIT REAL*8(A-H,O-Z)
  
      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,TWO=2.D0,THREE=3.D0)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),U(NDOFEL),
     2          DU(NDOFEL),V(NDOFEL),AMATRX(NDOFEL,NDOFEL),DDSDDE(2,2),
     3          XGP(6),WGP(6),DDSDDET(2,2), DAUX(2,2),AUX(2,6),
     4          AUXT(6,2),RL(6,6),RLL(6,2),RM(2,2),RMT(2,2),A(NDOFEL)

C     CALL CLEARV(RHS,NDOFEL)
C     CALL CLEARV(ANMASS,NDOFEL)
C     CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      AMATRX=0.0D0

C     Wave propagation velocities

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      RO=PROPS(3)

C     Constitutive tensor

      DDSDDE(1,2)=0.0D0
      DDSDDE(2,1)=0.0D0
      DDSDDE(1,1)=RO*AALFA
      DDSDDE(2,2)=RO*ABETA
      
      XI=COORDS(1,1)
      XM=COORDS(1,2)
      XJ=COORDS(1,3)
      YI=COORDS(2,1)
      YM=COORDS(2,2)
      YJ=COORDS(2,3)
      DXS=(XI-XJ)**2
      DYS=(YI-YJ)**2
      DL=DSQRT(DXS+DYS)
      DETJAC=DL/2.0

      CALL NORMVCT(XI,XJ,YI,YJ,CT,ST)
      RM(1,1)=  CT
      RM(1,2)=  ST
      RM(2,1)= -ST
      RM(2,2)=  CT
    
      CALL MTRAN(RM,2,2,RMT)
      CALL MMULT(RMT,2,2,DDSDDE,2,2,DAUX)
      CALL MMULT(DAUX,2,2,RM,2,2,DDSDDET)

      CALL GAUSS1D(XGP,WGP)

      DO N=1,6
        ETA =XGP(N)
        ALF =WGP(N)
        CALL SHAPE1D(AUXT,ETA)

        CALL MMULT(AUXT,6,2,DDSDDET,2,2,RLL)
        CALL MTRAN(AUXT,6,2,AUX)
        CALL MMULT(RLL,6,2,AUX,2,6,RL)
        AMATRX=AMATRX-RL*ALF*DETJAC
      END DO

      CALL MAVEC(AMATRX,NDOFEL,NDOFEL,V,RHS)

      RETURN

      END


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C    SUBROUTINE UELDASH3D(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPSC
C    1                    ,COORDS,MCRD,NNODE,U,DU,V,JELEM,DT)          C
C                                                                      C
C    ABSORBING 3D DASHPOT ELEMENT                                      C
C                                                                      C
C                                                                      C
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE UELDASH3D(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,
     1                    NIPROPS,AWAVE,IWAVE,COORDS,MCRD,NNODE,
     2                    U,DU,V,A,JELEM,DT,KINC,NINCR)

      IMPLICIT REAL*8(A-H,O-Z)
  
      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,TWO=2.D0,THREE=3.D0,
     1           NGPTS=4)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),U(NDOFEL),
     2          DU(NDOFEL),V(NDOFEL),AMATRX(NDOFEL,NDOFEL),DDSDDE(3,3),
     3          XGP(2,NGPTS),WGP(NGPTS),DDSDDET(3,3),DAUX(2,2),
     4          AUX(3,12),AUXT(12,3),RL(12,12),RLL(12,3),RM(3,3),
     5			RMT(3,3),A(NDOFEL),P1(3), P2(3), P3(3), P4(3),
     6			V1(3),V2(3),V3(3),V4(3),P1T(3),P2T(3),P3T(3),P4T(3),
     7			XY(2,4), P(2,4), XJ(2,2)

      AMATRX=0.0D0

C     Wave propagation velocities

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      RO=PROPS(3)

C     Constitutive tensor

      DDSDDE=0.0D0

      DDSDDE(3,3)=RO*ABETA
      DDSDDE(2,2)=RO*ABETA
	  DDSDDE(1,1)=RO*AALFA

	  CALL TRANSFORMA(COORDS, RM, XY)

      RMT=TRANSPOSE(RM)
      
      DDSDDET= MATMUL(RM, DDSDDE)
	  
	  CALL GPOINTS2X2(XGP,WGP)
c	  SI ESTA MULTIPLICACIÓN ESTÁ ANTES DE GPOINTS2X2, SE MODIFICA DDSDDE
c	  NO TIENE PORQUE PASAR ESO.
	  DDSDDE = MATMUL(DDSDDET, RMT)
	  
	  SMT= 0.0D0

      DO N=1,NGPTS

      	RII= XGP(1,N)
      	SII= XGP(2,N)
        ALF= WGP(N)      	

        CALL SFDER4(1, NDOFEL, 4, RII, SII, P)
        CALL JACOPER(4,XJ,XX,P)
	    DETJAC= (XJ(1,1)*XJ(2,2)-XJ(1,2)*XJ(2,1))
	    CALL SHAPEF4DASH(AUXT, RII, SII, KINC)

	  	RLL= MATMUL(AUXT, DDSDDE)
	  	AUX= TRANSPOSE(AUXT)
	  	RL= MATMUL(RLL, AUX)
        AMATRX=AMATRX-RL*ALF*DETJAC
      END DO
C	  APROXIMADAMENTE EN EL CICLO 80 SE DE DESBORDA EL MODELO, AL PARECER POR UN VALOR
C	  MUY ALTO DE LA VELOCIDAD.
      CALL MAVEC(AMATRX,NDOFEL,NDOFEL,V,RHS)
	  
      RETURN

      END



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C    SUBROUTINE UELEDASHINC(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,C
C    1                    COORDS,MCRD,NNODE,U,DU,V,JELEM,DT)           C
C                                                                      C
C    TRANSMITING DASHPOT ELEMENT USED TO EXCITATE THE BASE OF          C
C    A MODEL WITH A SV WAVE WITH VERTICAL INCIDENCE                    C
C                                                                      C
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE UELDASHINC(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,
     1                      NIPROPS,AWAVE,IWAVE,COORDS,MCRD,NNODE,
     2                      U,DU,V,A,JELEM,DT,KINC,NINCR)

      IMPLICIT REAL*8(A-H,O-Z)
  
      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,TWO=2.D0,THREE=3.D0)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),
     2          U(NDOFEL),DU(NDOFEL),V(NDOFEL),
     3          AMATRX(NDOFEL,NDOFEL),DDSDDE(2,2),XGP(6),VIN(6),
     4          WGP(6),AUX(2,6),AUXT(6,2),RL(6,6),RLL(6,2),A(NDOFEL)

C     CALL CLEARV(RHS,NDOFEL)
C     CALL CLEARV(ANMASS,NDOFEL)
C     CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      AMATRX=0.0D0
      VIN=0.0D0
      PPV=0.0D0

C     Wave propagation velocities

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      RO=PROPS(3)

C     Constitutive tensor

      DDSDDE(1,2)=0.0D0
      DDSDDE(2,1)=0.0D0
      DDSDDE(1,1)=RO*ABETA
      DDSDDE(2,2)=0.0D0
      
      XI=COORDS(1,1)
      XM=COORDS(1,2)
      XJ=COORDS(1,3)
      YI=COORDS(2,1)
      YM=COORDS(2,2)
      YJ=COORDS(2,3)
      DXS=(XI-XJ)**2
      DYS=(YI-YJ)**2
      DL=DSQRT(DXS+DYS)
      DETJAC=DL/2.0D0
    
      CALL GAUSS1D(XGP,WGP)

      DO N=1,6
        ETA =XGP(N)
        ALF=WGP(N)
        CALL SHAPE1D(AUXT,ETA)

        CALL MMULT(AUXT,6,2,DDSDDE,2,2,RLL)
        CALL MTRAN(AUXT,6,2,AUX)
        CALL MMULT(RLL,6,2,AUX,2,6,RL)
        AMATRX=AMATRX-RL*ALF*DETJAC
      END DO
      
      CALL UPULSEV(PPV,NINCR,DT,AWAVE,KINC)
      VIN(1)=PPV
      VIN(3)=PPV
      VIN(5)=PPV
      CALL MAVEC(AMATRX,NDOFEL,NDOFEL,VIN,RHS)

      RETURN

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C    SUBROUTINE UELEDASH2(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,NIPROPS,C
C    1                    COORDS,MCRD,NNODE,U,DU,V,JELEM,DT)           C
C                                                                      C
C    ABSORBING DASHPOT ELEMENT                                         C
C                                                                      C
C                                                                      C
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE UELEDASH2(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,
     1                    NIPROPS,AWAVE,IWAVE,COORDS,MCRD,NNODE,
     2                    U,DU,V,A,JELEM,DT,KINC,NINCR)

      IMPLICIT REAL*8(A-H,O-Z)
  
      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,TWO=2.D0,THREE=3.D0)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),U(NDOFEL),
     2          DU(NDOFEL),V(NDOFEL),AMATRX(NDOFEL,NDOFEL),DDSDDE(2,2),
     3          XGP(4),WGP(4),DDSDDET(2,2), DAUX(2,2),AUX(2,4),
     4          AUXT(4,2),RL(4,4),RLL(4,2),RM(2,2),RMT(2,2),A(NDOFEL)

C     CALL CLEARV(RHS,NDOFEL)
C     CALL CLEARV(ANMASS,NDOFEL)
C     CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      AMATRX=0.0D0

C     Wave propagation velocities

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      RO=PROPS(3)

C     Constitutive tensor

      DDSDDE(1,2)=0.0D0
      DDSDDE(2,1)=0.0D0
      DDSDDE(1,1)=RO*AALFA
      DDSDDE(2,2)=RO*ABETA
      
      XI=COORDS(1,1)
      XM=COORDS(1,2)
      XJ=COORDS(1,3)
      YI=COORDS(2,1)
      YM=COORDS(2,2)
      YJ=COORDS(2,3)
      DXS=(XI-XJ)**2
      DYS=(YI-YJ)**2
      DL=DSQRT(DXS+DYS)
      DETJAC=DL/2.0

      CALL NORMVCT(XI,XJ,YI,YJ,CT,ST)
      RM(1,1)=  CT
      RM(1,2)=  ST
      RM(2,1)= -ST
      RM(2,2)=  CT
    
      CALL MTRAN(RM,2,2,RMT)
      CALL MMULT(RMT,2,2,DDSDDE,2,2,DAUX)
      CALL MMULT(DAUX,2,2,RM,2,2,DDSDDET)

      CALL GAUSS1D(XGP,WGP)
C     CALL CLEAR(RL,4,4)
C     CALL CLEAR(RLL,4,4)

      DO N=1,4
        ETA =XGP(N)
        ALF=WGP(N)
        CALL SHAPE1D(AUXT,ETA)

        CALL MMULT(AUXT,4,2,DDSDDET,2,2,RLL)
        CALL MTRAN(AUXT,4,2,AUX)
        CALL MMULT(RLL,4,2,AUX,2,4,RL)
        AMATRX=AMATRX-RL*ALF*DETJAC
C       CALL CLEAR(RL,NDOFEL,NDOFEL)
      END DO

      CALL MAVEC(AMATRX,NDOFEL,NDOFEL,V,RHS)

      RETURN

      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C  UEL3DEXP8.for                                                       C
C  8-NODED ISOPARAMETRIC 3D ELEMENT WITH FULL GAUSS INTEGRATION        C
C  USER ELEMENT SUBROUTINE FOR EXPLICIT DYNAMIC ANALYSIS               C
C                                                                      C
C  GIVEN THE DISPLACEMENT AT TIME T AND THE DISPLACEMENT INCREMENT     C
C  IT RETURNS IN THE RHS() VECTOR THE INERTIAL FORCE CONTRIBUTION      C
C  AT TIME T+DT AND IN THE ANMASS() VECTOR THE DIAGONALIZED MASS       C
C  MATRIX                                                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE UEL3DEXP8(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,
     1                   NIPROPS,AWAVE,IWAVE,COORDS,MCRD,NNODE,U,DU,
     2                   V,A,JELEM,DT,KINC,NINCR,INOD)
     
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,NTENS=6,TWO=2.D0,
     1           NGPTS=8,THREE=3.D0, NDI=3)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),U(NDOFEL),
     2          DU(NDOFEL),V(NDOFEL),A(NDOFEL),AMATRX(NDOFEL,NDOFEL),
     3          AMASS(NDOFEL,NDOFEL),CDMAT(NDOFEL,NDOFEL),UP(NDOFEL),
     4          DDSDDE(NTENS,NTENS),B(NTENS,NDOFEL),BT(NDOFEL,NTENS),
     5          FRST2(NDOFEL,NDOFEL),FRST1(NDOFEL,NDOFEL),XP(3,NGPTS),
     6          XW(NGPTS),AUX1(NTENS,NDOFEL)

C     Mass density.
	  
      RO=PROPS(3)

C     Rayleigh damping parameters.

      AALFA=PROPS(1)
      ABETA=PROPS(2)

      CALL CLEARV(RHS,NDOFEL)
      CALL CLEARV(ANMASS,NDOFEL)
      CALL CLEAR(DDSDDE,NTENS,NTENS)
      CALL CLEAR(B,NTENS,NDOFEL)
      CALL CLEAR(BT,NDOFEL,NTENS)
      CALL CLEAR(FRST2,NDOFEL,NDOFEL)
      CALL CLEAR(FRST1,NDOFEL,NDOFEL)
      CALL CLEAR(XP,3,NGPTS)
      CALL CLEARV(XW,NGPTS)
      CALL CLEAR(AUX1,NTENS,NDOFEL)
      CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      CALL CLEAR(AMASS,NDOFEL,NDOFEL)
      CALL CLEAR(CDMAT,NDOFEL,NDOFEL)

C       Elasticity tensor.

	  CALL UMAT3D(DDSDDE,NDI,NTENS,PROPS,NPROPS)

C     Generates Gauss points and weights
C     and loops around all Gauss points.
	  
      CALL GPOINTS3D2X2(XP,XW)
      NGPT=8
      SMS=0.0D0
      SMT=0.0D0
	  
      DO NN=1,NGPT

        RII=XP(1,NN)
        SII=XP(2,NN)
        TII=XP(3,NN)
        ALF=XW(NN)

C       Computes B matrix.

        CALL STDM3D8(NNODE,NDOFEL,NTENS,COORDS,MCRD,B,DDET,RII,SII,
     1               TII,XBAR)
	  

        SMT=SMT+RO*DDET*XBAR*ALF
	  
C       Assembles stiffness and mass matrix.

        CALL MMULT(DDSDDE,NTENS,NTENS,B,NTENS,NDOFEL,AUX1)
        CALL CLEAR(BT,NDOFEL,NTENS)
        CALL MTRAN(B,NTENS,NDOFEL,BT)
        CALL MMULT(BT,NDOFEL,NTENS,AUX1,NTENS,NDOFEL,FRST1)
C
        CALL AMASS3D8(FRST2,NDOFEL,RII,SII,TII)

C       Considers Gauss weight and Jacobian determinant
C       into partial stiffness and mass matrix.

        CALL SMULT(FRST1,NDOFEL,NDOFEL,ALF*DDET*XBAR)
        CALL SMULT(FRST2,NDOFEL,NDOFEL,ALF*DDET*XBAR*RO)

C       Updates stiffness and mass matrix.

        CALL UPDMAT(AMATRX,NDOFEL,NDOFEL,FRST1)
        CALL UPDMAT(AMASS ,NDOFEL,NDOFEL,FRST2)

      END DO

      
C     Computes elastic,inertial and damping nodal forces.

      CALL EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,
     1                    AALFA,ABETA,U,V,A,DT,SMT,RHS,JELEM)

      RETURN

      END
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C  UEL3DEXP8.for                                                       C
C  8-NODED ISOPARAMETRIC 3D ELEMENT WITH FULL GAUSS INTEGRATION        C
C  USER ELEMENT SUBROUTINE FOR EXPLICIT DYNAMIC ANALYSIS               C
C                                                                      C
C  GIVEN THE DISPLACEMENT AT TIME T AND THE DISPLACEMENT INCREMENT     C
C  IT RETURNS IN THE RHS() VECTOR THE INERTIAL FORCE CONTRIBUTION      C
C  AT TIME T+DT AND IN THE ANMASS() VECTOR THE DIAGONALIZED MASS       C
C  MATRIX                                                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE UEL3DWED10(RHS,ANMASS,NDOFEL,PROPS,NPROPS,IPROPS,
     1                   NIPROPS,AWAVE,IWAVE,COORDS,MCRD,NNODE,U,DU,
     2                   V,A,JELEM,DT,KINC,NINCR,INOD)
     
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.D0,HALF=0.5D0,ONE=1.D0,NTENS=6,TWO=2.D0,
     1           NGPTS=8,THREE=3.D0, NDI=3)

C     Parameter arrays from UEL.f

      DIMENSION RHS(NDOFEL),ANMASS(NDOFEL),PROPS(NPROPS),
     1          IPROPS(NIPROPS),AWAVE(5),COORDS(MCRD,NNODE),U(NDOFEL),
     2          DU(NDOFEL),V(NDOFEL),A(NDOFEL),AMATRX(NDOFEL,NDOFEL),
     3          AMASS(NDOFEL,NDOFEL),CDMAT(NDOFEL,NDOFEL),UP(NDOFEL),
     4          DDSDDE(NTENS,NTENS),B(NTENS,NDOFEL),BT(NDOFEL,NTENS),
     5          FRST2(NDOFEL,NDOFEL),FRST1(NDOFEL,NDOFEL),XP(3,NGPTS),
     6          XW(NGPTS),AUX1(NTENS,NDOFEL)

C     Mass density.
	  
      RO=PROPS(3)

C     Rayleigh damping parameters.

      AALFA=PROPS(1)
      ABETA=PROPS(2)

      CALL CLEARV(RHS,NDOFEL)
      CALL CLEARV(ANMASS,NDOFEL)
      CALL CLEAR(DDSDDE,NTENS,NTENS)
      CALL CLEAR(B,NTENS,NDOFEL)
      CALL CLEAR(BT,NDOFEL,NTENS)
      CALL CLEAR(FRST2,NDOFEL,NDOFEL)
      CALL CLEAR(FRST1,NDOFEL,NDOFEL)
      CALL CLEAR(XP,3,NGPTS)
      CALL CLEARV(XW,NGPTS)
      CALL CLEAR(AUX1,NTENS,NDOFEL)
      CALL CLEAR(AMATRX,NDOFEL,NDOFEL)
      CALL CLEAR(AMASS,NDOFEL,NDOFEL)
      CALL CLEAR(CDMAT,NDOFEL,NDOFEL)

C       Elasticity tensor.

	  CALL UMAT3D(DDSDDE,NDI,NTENS,PROPS,NPROPS)

C     Generates Gauss points and weights
C     and loops around all Gauss points.
	  
      CALL GPOINTSW10(XP,XW)
      NGPT=8
      SMS=0.0D0
      SMT=0.0D0
	  
      DO NN=1,NGPT

        RII=XP(1,NN)
        SII=XP(2,NN)
        TII=XP(3,NN)
        ALF=XW(NN)

C       Computes B matrix.
ccccccccc ESTA ES LA RUTINA CON EL PROBLEMA.	  
C       CALL STDM3D8(JELEM,NNODE,NDOFEL,NTENS,COORDS,MCRD,B,DDET,RII,SII,
        CALL STDM3D8(NNODE,NDOFEL,NTENS,COORDS,MCRD,B,DDET,RII,SII,
     1               TII,XBAR)
	  
c	    CALL STDM9(NNODE,NDOFEL,NTENS,COORDS,MCRD,B,DDET,RII,SII,
c    1             XBAR)
          	
        SMT=SMT+RO*DDET*XBAR*ALF
	  
C       Assembles stiffness and mass matrix.

        CALL MMULT(DDSDDE,NTENS,NTENS,B,NTENS,NDOFEL,AUX1)
        CALL CLEAR(BT,NDOFEL,NTENS)
        CALL MTRAN(B,NTENS,NDOFEL,BT)
        CALL MMULT(BT,NDOFEL,NTENS,AUX1,NTENS,NDOFEL,FRST1)
C
C       CALL AMASS3D8(FRST2,NDOFEL,RII,SII)
        CALL AMASS3D8(FRST2,NDOFEL,RII,SII,TII)

C       Considers Gauss weight and Jacobian determinant
C       into partial stiffness and mass matrix.

        CALL SMULT(FRST1,NDOFEL,NDOFEL,ALF*DDET*XBAR)
        CALL SMULT(FRST2,NDOFEL,NDOFEL,ALF*DDET*XBAR*RO)

C       Updates stiffness and mass matrix.

        CALL UPDMAT(AMATRX,NDOFEL,NDOFEL,FRST1)
        CALL UPDMAT(AMASS ,NDOFEL,NDOFEL,FRST2)

      END DO

      
C     Computes elastic,inertial and damping nodal forces.

      CALL EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,
     1                    AALFA,ABETA,U,V,A,DT,SMT,RHS,JELEM)

      RETURN

      END
C
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,AALFA,ABETA,U,C
C    1                      DU,UP,DT,SMT,RHS)                          C
C                                                                      C
C   Diagonalizes the mass matrix and computes the strain and inertial  C
C   force contributions                                                C
C                                                                      C
C   AMASS(,)     :Mass matrix output in diagaonal form                 C
C   AMATRX(,)    :Stiffness matrix                                     C
C   ANMASS()     :Diagonalized mass matrix in vector form              C
C   CDMAT(,)     :Rayleigh damping matrix                              C
C   NDOFEL       :Nodal D.O.F per element                              C
C   AALFA        :Rayleigh damping parameter                           C
C   ABETA        :Rayleigh damping parameter                           C
C   U()          :Nodal displacements vector at the current increment  C
C   DU()         :Nodal incremental displacements vector               C
C   UM()         :Nodal displacements vector at the previous increment C
C   DT           :Time increment                                       C
C   SMT          :Total element mass                                   C
C   RHS()        :Right Hand Side vector                               C
C   JELEM        :Element identifier                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE EFFECTIVEFORCE(AMASS,AMATRX,ANMASS,NDOFEL,AALFA,
     1                          ABETA,U,V,A,DT,SMT,RHS,JELEM)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION AMATRX(NDOFEL,NDOFEL),AMASS(NDOFEL,NDOFEL),
     1          ANMASS(NDOFEL),U(NDOFEL),V(NDOFEL),A(NDOFEL),
     2          RSC(NDOFEL),RDC(NDOFEL),RIC(NDOFEL),RIM(NDOFEL),
     3          RSM(NDOFEL),RHS(NDOFEL)

C     Diagonalizes the mass matrix

      SMS=0.0D0
      DO I=1,NDOFEL
        SMS=SMS+AMASS(I,I)
      END DO
      DO I=1,NDOFEL
        ANMASS(I)=2.0*(AMASS(I,I)/SMS)*SMT
      END DO
C
      AMASS=0.0D0
      DO I=1,NDOFEL
        AMASS(I,I)=ANMASS(I)
      END DO

C     Forms the effective forces vector

      CALL MAVEC(AMATRX,NDOFEL,NDOFEL,U,RSC)    ! Elastic contribution
      CALL SMULTV(RSC,NDOFEL,-1.D0)
      CALL MAVEC(AMASS,NDOFEL,NDOFEL,A, RIC)    ! Inertial contribution
      CALL SMULTV(RIC,NDOFEL,-1.D0)

cc      CALL MAVEC(AMATRX,NDOFEL,NDOFEL,V,RDC)  ! Damping contribution
cc      CALL SMULTV(RDC,NDOFEL,-ABETA)
cc      CALL MAVEC(AMASS,NDOFEL,NDOFEL,V, RIM)
cc      CALL SMULTV(RIM,NDOFEL,-AALFA)

C     Puts everything into the RHS vector.

      CALL UPDVEC(RHS,NDOFEL,RSC)
      CALL UPDVEC(RHS,NDOFEL,RIC)
cc      CALL UPDVEC(RHS,NDOFEL,RDC)
cc      CALL UPDVEC(RHS,NDOFEL,RIM)

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE LINJAC(IDF,COORDS,MCRD,NNODE,IELCON,II,IM,IJ,DETJAC,    C
C    1                CT,ST)                                           C
C                                                                      C
C   Using the local connectivity array IELCON for an 8-noded 2D        C
C   element and the face ID computes the Jacobian for a 3-noded        C
C   1D-element and the outward normal vector.                          C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE LINJAC(IDF,COORDS,MCRD,NNODE,IELCON,II,IM,IJ,DETJAC,
     1                  XM,ZM,CT,ST)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION COORDS(MCRD,NNODE),IELCON(4,3)

      CALL LOCIEL(IELCON)

      II=IELCON(IDF,1)
      IM=IELCON(IDF,3)
      IJ=IELCON(IDF,2)

      XI=COORDS(1,II)
      ZI=COORDS(2,II)
      XM=COORDS(1,IM)
      ZM=COORDS(2,IM)
      XJ=COORDS(1,IJ)
      ZJ=COORDS(2,IJ)
      DXS=(XI-XJ)**2
      DZS=(ZI-ZJ)**2
      DL=DSQRT(DXS+DZS)
      DETJAC=DL/2.0

      CALL NORMVCTINT(XI,XJ,ZI,ZJ,CT,ST)

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE ISOCART8(RR,COORDS,NDOFDIM,XX)                          C
C                                                                      C
C   RECOVERS THE PHYSICAL CARTESIAN COORDINATES OF A POINT             C
C   THAT BELONGS TO AN 8-NODED ELEMENT GIVEN ITS NATURAL COORDINATES   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE ISOCART8(RR,COORDS,MCRD,NNODES,XX)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION RR(2),XX(2),COORDS(MCRD,NNODES),SN(8)

      R=RR(1)
      S=RR(2)
      CALL SHAPEF8(SN,R,S)

      XXX=0.0D0
      ZZZ=0.0D0
      DO JJ=1,8
        XXX=XXX+SN(JJ)*COORDS(1,JJ)
        ZZZ=ZZZ+SN(JJ)*COORDS(2,JJ)
      END DO

      XX(1)=XXX
      XX(2)=ZZZ

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE LOCASSEM(RHS,FF,II,IM,IJ)                               C
C                                                                      C
C   Assembles the surface force vector FF() into the elemental force   C
C   vector RHS() according to II,IM,IJ.                                C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE LOCASSEM(RHS,FF,II,IM,IJ)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION RHS(16),FF(6)

      RHS(2*II-1)=FF(1)
      RHS(2*II  )=FF(2)
      RHS(2*IM-1)=FF(5)
      RHS(2*IM  )=FF(6)
      RHS(2*IJ-1)=FF(3)
      RHS(2*IJ  )=FF(4)

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE RSETUP(IDF,RR)                                          C
C                                                                      C
C   According to IDF sets up the natural coordinates variation.        C
C                                                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE RSETUP(IDF,RR,IIF)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION RR(2)

      SELECT CASE (IDF)
        CASE (1)
        RR(2)=-1.0
        IIF=1

        CASE (2)
        RR(1)= 1.0D0
        IIF=2

        CASE (3)
        RR(2)= 1.0D0
        IIF=1

        CASE (4)
        RR(1)=-1.0D0
        IIF=2

      END SELECT

      RETURN

      END


































