
C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE INCOMINGDIS9(IDF,AMATRX,AMASS,CDMAT,COORDS,PROPS,NPROPS,C
C    1                  MCRD,AWAVE,IWAVE,NNODE,NDOFEL,R00,RELA,RINE,   C
C    2                  RDAM,DT,KINC)                                  C
C                                                                      C
C   Computes effective forces consistent with the incoming wave field  C
C   at time t.                                                         C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012
C
      SUBROUTINE INCOMINGDIS9(IDF,AMATRX,COORDS,PROPS,NPROPS,MCRD,AWAVE,
     1                       IWAVE,NNODE,NDOFEL,R00,DT,KINC,NINCR,IOUT)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      PARAMETER(ONE=1.0D0,ONENEG=-1.0D0)
C
      DIMENSION AMATRX(NDOFEL,NDOFEL),COORDS(MCRD,NNODE),PROPS(NPROPS),
     1          R00(NDOFEL),U0(NDOFEL),AWAVE(5),
     2          IELCON(4,3),U00(NDOFEL),RP(NDOFEL) !,LB(6),LEE(6),LE(12)

      INTEGER, ALLOCATABLE, DIMENSION(:):: LB, LEE, LE      
C
      CALL CLEARV(U0 ,NDOFEL)
      CALL CLEARV(U00,NDOFEL)

C     Evaluate incoming displacements at time t.

      CALL DISPINCT9(IDF,U0,COORDS,NNODE,PROPS,NPROPS,MCRD,
     1              AWAVE,IWAVE,DT,KINC,IOUT)

C     Convert displacements and velocities into
C     effective forces at time t.

      IF (IDF==1 .OR. IDF==2 .OR. IDF==3 .OR. IDF==4) THEN
        
        ALLOCATE(LB(6), LEE(6), LE(12))
          
        DO I=1,NDOFEL
            AMATRX(I,I)=0.0D0
            U00(I)=U0(I)
        END DO

        CALL LOCIEL(IELCON)

        IF (IDF.EQ.1) THEN
            LEE(1)=3
            LEE(2)=4
            LEE(3)=6
            LEE(4)=7
            LEE(5)=8
            LEE(6)=9
        END IF

        IF (IDF.EQ.2) THEN
            LEE(1)=1
            LEE(2)=4
            LEE(3)=5
            LEE(4)=7
            LEE(5)=8
            LEE(6)=9
        END IF

        IF (IDF.EQ.3) THEN
            LEE(1)=1
            LEE(2)=2
            LEE(3)=5
            LEE(4)=6
            LEE(5)=8
            LEE(6)=9
        END IF

        IF (IDF.EQ.4) THEN
            LEE(1)=2
            LEE(2)=3
            LEE(3)=5
            LEE(4)=6
            LEE(5)=7
            LEE(6)=9
        END IF

C     Creates list of exterior DOFs

        DO I=1,6
            J=LEE(I)
            LE(2*I-1)=2*J-1
            LE(2*I)=2*J
        END DO

C     Creates list of boundary DOFs

        DO I=1,3
            J=IELCON(IDF,I)
            LB(2*I-1)=2*J-1
            LB(2*I)=2*J
        END DO

C     Identifies DOF in LB.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,6,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
                U00(J)=0.0D0
            END IF
        END DO

        CALL CLEARV(R00,NDOFEL)
        CALL CLEARV(RP,NDOFEL)
        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,6,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
                R00(J)=-RP(J)
            END IF
        END DO

        CALL CLEARV(U00,NDOFEL)

        DO I=1,NDOFEL
            U00(I)=U0(I)
        END DO
      
        CALL CLEARV(RP,NDOFEL)

C     Identifies DOF in LE.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,12,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
                U00(J)=0.0D0
            END IF
        END DO

        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,12,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
                R00(J)=RP(J)
            END IF
        END DO
      
      ELSE

        ALLOCATE(LB(2), LEE(8), LE(16))
          
        DO I=1,NDOFEL
            AMATRX(I,I)=0.0D0
            U00(I)=U0(I)
        END DO

        IF (IDF.EQ.5) THEN
            LEE(1)=2
            LEE(2)=3
            LEE(3)=4
            LEE(4)=5
            LEE(5)=6
            LEE(6)=7
            LEE(7)=8
            LEE(8)=9
             
C     Creates list of boundary DOFs
             
            LB(1)=1
            LB(2)=2
             
        END IF

        IF (IDF.EQ.6) THEN
            LEE(1)=1
            LEE(2)=3
            LEE(3)=4
            LEE(4)=5
            LEE(5)=6
            LEE(6)=7
            LEE(7)=8
            LEE(8)=9

C     Creates list of boundary DOFs
             
            LB(1)=3
            LB(2)=4
             
        END IF

        IF (IDF.EQ.7) THEN
            LEE(1)=1
            LEE(2)=2
            LEE(3)=4
            LEE(4)=5
            LEE(5)=6
            LEE(6)=7
            LEE(7)=8
            LEE(8)=9

C     Creates list of boundary DOFs
             
            LB(1)=5
            LB(2)=6

        END IF

        IF (IDF.EQ.8) THEN
            LEE(1)=1
            LEE(2)=2
            LEE(3)=3
            LEE(4)=5
            LEE(5)=6
            LEE(6)=7
            LEE(7)=8
            LEE(8)=9

C     Creates list of boundary DOFs
             
            LB(1)=7
            LB(2)=8

        END IF

C     Creates list of exterior DOFs

        DO I=1,8
            J=LEE(I)
            LE(2*I-1)=2*J-1
            LE(2*I)=2*J
        END DO

C     Identifies DOF in LB.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,2,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               U00(J)=0.0D0
            END IF
        END DO

        CALL CLEARV(R00,NDOFEL)
        CALL CLEARV(RP,NDOFEL)
        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,2,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=-RP(J)
            END IF
        END DO

        CALL CLEARV(U00,NDOFEL)

        DO I=1,NDOFEL
            U00(I)=U0(I)
        END DO
      
        CALL CLEARV(RP,NDOFEL)

C     Identifies DOF in LE.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,16,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               U00(J)=0.0D0
            END IF
        END DO

        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,16,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=RP(J)
            END IF
        END DO
          
      END IF

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE INCOMINGDIS8(IDF,AMATRX,AMASS,CDMAT,COORDS,PROPS,NPROPS, C
C    1                  MCRD,AWAVE,IWAVE,NNODE,NDOFEL,R00,RELA,RINE,   C
C    2                  RDAM,DT,KINC)                                  C
C                                                                      C
C   Computes effective forces consistent with the incoming wave field  C
C   at time t.                                                         C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE INCOMINGDIS8(IDF,AMATRX,COORDS,PROPS,NPROPS,MCRD,AWAVE,
     1                       IWAVE,NNODE,NDOFEL,R00,DT,KINC,NINCR,IOUT)

      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER(ONE=1.0D0,ONENEG=-1.0D0)

      DIMENSION AMATRX(NDOFEL,NDOFEL),COORDS(MCRD,NNODE),PROPS(NPROPS),
     1          R00(NDOFEL),U0(NDOFEL),AWAVE(5),
     2          IELCON(4,3),U00(NDOFEL),RP(NDOFEL)! ,LB(6),LEE(5),LE(10)
     
      INTEGER, ALLOCATABLE, DIMENSION(:):: LB, LEE, LE
      

      CALL CLEARV(U0 ,NDOFEL)
      CALL CLEARV(U00,NDOFEL)

C     Evaluate incoming displacements at time t.

      CALL DISPINCT8(IDF,U0,COORDS,NNODE,PROPS,NPROPS,MCRD,
     1              AWAVE,IWAVE,DT,KINC,IOUT)

C     Convert displacements and velocities into
C     effective forces at time t.

      IF (IDF==1 .OR. IDF==2 .OR. IDF==3 .OR. IDF==4) THEN
        
        ALLOCATE(LB(6), LEE(5), LE(10))
          
        DO I=1,NDOFEL
            AMATRX(I,I)=0.0D0
            U00(I)=U0(I)
        END DO

        CALL LOCIEL(IELCON)

        IF (IDF.EQ.1) THEN
            LEE(1)=3
            LEE(2)=4
            LEE(3)=6
            LEE(4)=7
            LEE(5)=8
        END IF

        IF (IDF.EQ.2) THEN
            LEE(1)=1
            LEE(2)=4
            LEE(3)=5
            LEE(4)=7
            LEE(5)=8
        END IF

        IF (IDF.EQ.3) THEN
            LEE(1)=1
            LEE(2)=2
            LEE(3)=5
            LEE(4)=6
            LEE(5)=8
        END IF

        IF (IDF.EQ.4) THEN
            LEE(1)=2
            LEE(2)=3
            LEE(3)=5
            LEE(4)=6
            LEE(5)=7
        END IF

C     Creates list of exterior DOFs

        DO I=1,5
            J=LEE(I)
            LE(2*I-1)=2*J-1
            LE(2*I)=2*J
        END DO

C     Creates list of boundary DOFs

        DO I=1,3
            J=IELCON(IDF,I)
            LB(2*I-1)=2*J-1
            LB(2*I)=2*J
        END DO

C     Identifies DOF in LB.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,6,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
                U00(J)=0.0D0
            END IF
        END DO

        CALL CLEARV(R00,NDOFEL)
        CALL CLEARV(RP,NDOFEL)
        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,6,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=-RP(J)
            END IF
        END DO

        CALL CLEARV(U00,NDOFEL)

        DO I=1,NDOFEL
            U00(I)=U0(I)
        END DO
      
        CALL CLEARV(RP,NDOFEL)

C     Identifies DOF in LE.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,10,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               U00(J)=0.0D0
            END IF
        END DO

        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,10,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=RP(J)
            END IF
        END DO
      
      ELSE

        ALLOCATE(LB(2), LEE(7), LE(14))
          
        DO I=1,NDOFEL
            AMATRX(I,I)=0.0D0
            U00(I)=U0(I)
        END DO

        IF (IDF.EQ.5) THEN
            LEE(1)=2
            LEE(2)=3
            LEE(3)=4
            LEE(4)=5
            LEE(5)=6
            LEE(6)=7
            LEE(7)=8
             
C     Creates list of boundary DOFs
             
            LB(1)=1
            LB(2)=2
             
        END IF

        IF (IDF.EQ.6) THEN
            LEE(1)=1
            LEE(2)=3
            LEE(3)=4
            LEE(4)=5
            LEE(5)=6
            LEE(6)=7
            LEE(7)=8

C     Creates list of boundary DOFs
             
            LB(1)=3
            LB(2)=4
             
        END IF

        IF (IDF.EQ.7) THEN
            LEE(1)=1
            LEE(2)=2
            LEE(3)=4
            LEE(4)=5
            LEE(5)=6
            LEE(6)=7
            LEE(7)=8

C     Creates list of boundary DOFs
             
            LB(1)=5
            LB(2)=6

        END IF

        IF (IDF.EQ.8) THEN
            LEE(1)=1
            LEE(2)=2
            LEE(3)=3
            LEE(4)=5
            LEE(5)=6
            LEE(6)=7
            LEE(7)=8

C     Creates list of boundary DOFs
             
            LB(1)=7
            LB(2)=8

        END IF

C     Creates list of exterior DOFs

        DO I=1,7
            J=LEE(I)
            LE(2*I-1)=2*J-1
            LE(2*I)=2*J
        END DO

C     Identifies DOF in LB.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,2,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               U00(J)=0.0D0
            END IF
        END DO

        CALL CLEARV(R00,NDOFEL)
        CALL CLEARV(RP,NDOFEL)
        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,2,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=-RP(J)
            END IF
        END DO

        CALL CLEARV(U00,NDOFEL)

        DO I=1,NDOFEL
            U00(I)=U0(I)
        END DO
      
        CALL CLEARV(RP,NDOFEL)

C     Identifies DOF in LE.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,14,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               U00(J)=0.0D0
            END IF
        END DO

        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,14,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=RP(J)
            END IF
        END DO
          
      END IF

      RETURN
C
      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE INCOMINGDIS4(IDF,AMATRX,AMASS,CDMAT,COORDS,PROPS,NPROPS,C
C    1                  MCRD,AWAVE,IWAVE,NNODE,NDOFEL,R00,RELA,RINE,   C
C    2                  RDAM,DT,KINC)                                  C
C                                                                      C
C   Computes effective forces consistent with the incoming wave field  C
C   at time t.                                                         C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE INCOMINGDIS4(IDF,AMATRX,COORDS,PROPS,NPROPS,MCRD,AWAVE,
     1                       IWAVE,NNODE,NDOFEL,R00,DT,KINC,NINCR,IOUT)

      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER(ONE=1.0D0,ONENEG=-1.0D0)

      DIMENSION AMATRX(NDOFEL,NDOFEL),COORDS(MCRD,NNODE),PROPS(NPROPS),
     1          R00(NDOFEL),U0(NDOFEL),AWAVE(5),
     2          IELCON(4,2),U00(NDOFEL),RP(NDOFEL)
     
      INTEGER, ALLOCATABLE, DIMENSION(:):: LB, LEE, LE
      

      CALL CLEARV(U0 ,NDOFEL)
      CALL CLEARV(U00,NDOFEL)

C     Evaluate incoming displacements at time t.

      CALL DISPINCT4(IDF,U0,COORDS,NNODE,PROPS,NPROPS,MCRD,
     1              AWAVE,IWAVE,DT,KINC,IOUT)

C     Convert displacements and velocities into
C     effective forces at time t.

      IF (IDF==1 .OR. IDF==2 .OR. IDF==3 .OR. IDF==4) THEN
        
        ALLOCATE(LB(4), LEE(2), LE(4))
          
        DO I=1,NDOFEL
            AMATRX(I,I)=0.0D0
            U00(I)=U0(I)
        END DO

        CALL LOCIEL2(IELCON)

        IF (IDF.EQ.1) THEN
            LEE(1)=3
            LEE(2)=4
        END IF

        IF (IDF.EQ.2) THEN
            LEE(1)=1
            LEE(2)=4
        END IF

        IF (IDF.EQ.3) THEN
            LEE(1)=1
            LEE(2)=2
        END IF

        IF (IDF.EQ.4) THEN
            LEE(1)=2
            LEE(2)=3
        END IF

C     Creates list of exterior DOFs

        DO I=1,2
            J=LEE(I)
            LE(2*I-1)=2*J-1
            LE(2*I)=2*J
        END DO

C     Creates list of boundary DOFs

        DO I=1,2
            J=IELCON(IDF,I)
            LB(2*I-1)=2*J-1
            LB(2*I)=2*J
        END DO

C     Identifies DOF in LB.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,4,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
                U00(J)=0.0D0
            END IF
        END DO

        CALL CLEARV(R00,NDOFEL)
        CALL CLEARV(RP,NDOFEL)
        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,4,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=-RP(J)
            END IF
        END DO

        CALL CLEARV(U00,NDOFEL)

        DO I=1,NDOFEL
            U00(I)=U0(I)
        END DO
      
        CALL CLEARV(RP,NDOFEL)

C     Identifies DOF in LE.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,4,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               U00(J)=0.0D0
            END IF
        END DO

        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,4,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=RP(J)
            END IF
        END DO
      
      ELSE

        ALLOCATE(LB(2), LEE(3), LE(6))
          
        DO I=1,NDOFEL
            AMATRX(I,I)=0.0D0
            U00(I)=U0(I)
        END DO

        IF (IDF.EQ.5) THEN
            LEE(1)=2
            LEE(2)=3
            LEE(3)=4
             
C     Creates list of boundary DOFs
             
            LB(1)=1
            LB(2)=2
             
        END IF

        IF (IDF.EQ.6) THEN
            LEE(1)=1
            LEE(2)=3
            LEE(3)=4

C     Creates list of boundary DOFs
             
            LB(1)=3
            LB(2)=4
             
        END IF

        IF (IDF.EQ.7) THEN
            LEE(1)=1
            LEE(2)=2
            LEE(3)=4

C     Creates list of boundary DOFs
             
            LB(1)=5
            LB(2)=6

        END IF

        IF (IDF.EQ.8) THEN
            LEE(1)=1
            LEE(2)=2
            LEE(3)=3

C     Creates list of boundary DOFs
             
            LB(1)=7
            LB(2)=8

        END IF

C     Creates list of exterior DOFs

        DO I=1,3
            J=LEE(I)
            LE(2*I-1)=2*J-1
            LE(2*I)=2*J
        END DO

C     Identifies DOF in LB.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,2,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               U00(J)=0.0D0
            END IF
        END DO

        CALL CLEARV(R00,NDOFEL)
        CALL CLEARV(RP,NDOFEL)
        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LB,2,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=-RP(J)
            END IF
        END DO

        CALL CLEARV(U00,NDOFEL)

        DO I=1,NDOFEL
            U00(I)=U0(I)
        END DO
      
        CALL CLEARV(RP,NDOFEL)

C     Identifies DOF in LE.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,6,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               U00(J)=0.0D0
            END IF
        END DO

        RP=MATMUL(AMATRX, U00)

C     Assembles effective forces.

        DO J=1,NDOFEL
            IFLAG=0
            CALL BELONGS(LE,6,J,IFLAG)
            IF (IFLAG.EQ.1) THEN
               R00(J)=RP(J)
            END IF
        END DO
          
      END IF

      RETURN
C
      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE DISPINCT9(IDF,U0,V0,A0,COORDS,NNODE,PROPS,NPROPS,MCRD,   C
C                      AWAVE,IWAVE,IELCON,DT,KNINC)                    C
C                                                                      C
C   Evaluates and assembles the displacement field.                    C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE DISPINCT9(IDF,U0,COORDS,NNODE,PROPS,NPROPS,MCRD,
     1                    AWAVE,IWAVE,DT,KINC,IOUT)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION U0(18), AWAVE(5),COORDS(MCRD,NNODE),PROPS(NPROPS)

      PARAMETER (ZERO=0.0D0,ONE=1.0D0,HALF=0.5D0,TWO=2.0D0,FOUR=4.0D0)

C     Incident wave properties.

      FC=AWAVE(3)
      TMAX=AWAVE(1)
      TINI=AWAVE(2)
      AMPR=AWAVE(4)
      PHI=AWAVE(5)

C     Elastic properties

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      RO=PROPS(3)

C     Compute incoming dispalcements for all the nodes in
C     an element.
      
      DO II=1,NNODE
        X1=COORDS(1,II)
        X3=COORDS(2,II)

        IF(IWAVE.EQ.1) THEN

          US1=0.0D0
          US2=0.0D0

          CALL PWDISPT(FC,TINI,AMPR,PHI,AALFA,ABETA,X1,X3,
     1                  KINC,DT,US1,US2)

          U0(2*II-1)=US1
          U0(2*II  )=US2

        ELSE

          US1=0.0D0
          US2=0.0D0

          CALL SWDISPT(FC,TINI,AMPR,PHI,AALFA,ABETA,X1,X3,
     1                  KINC,DT,US1,US2)

          U0(2*II-1)=US1
          U0(2*II  )=US2
          
        END IF

      END DO

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE DISPINCT8(IDF,U0,V0,A0,COORDS,NNODE,PROPS,NPROPS,MCRD,   C
C                      AWAVE,IWAVE,IELCON,DT,KNINC)                    C
C                                                                      C
C   Evaluates and assembles the displacement field.                    C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE DISPINCT8(IDF,U0,COORDS,NNODE,PROPS,NPROPS,MCRD,
     1                    AWAVE,IWAVE,DT,KINC,IOUT)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION U0(16), AWAVE(5),COORDS(MCRD,NNODE),PROPS(NPROPS)

      PARAMETER (ZERO=0.0D0,ONE=1.0D0,HALF=0.5D0,TWO=2.0D0,FOUR=4.0D0)

C     Incident wave properties.

      FC=AWAVE(3)
      TMAX=AWAVE(1)
      TINI=AWAVE(2)
      AMPR=AWAVE(4)
      PHI=AWAVE(5)

C     Elastic properties

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      RO=PROPS(3)

C     Compute incoming dispalcements for all the nodes in
C     an element.
      
      DO II=1,NNODE
        X1=COORDS(1,II)
        X3=COORDS(2,II)

        IF(IWAVE.EQ.1) THEN

          US1=0.0D0
          US2=0.0D0

          CALL PWDISPT(FC,TINI,AMPR,PHI,AALFA,ABETA,X1,X3,
     1                  KINC,DT,US1,US2)

          U0(2*II-1)=US1
          U0(2*II  )=US2

        ELSE

          US1=0.0D0
          US2=0.0D0

          CALL SWDISPT(FC,TINI,AMPR,PHI,AALFA,ABETA,X1,X3,
     1                  KINC,DT,US1,US2)

          U0(2*II-1)=US1
          U0(2*II  )=US2
          
        END IF

      END DO

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE DISPINCT4(IDF,U0,V0,A0,COORDS,NNODE,PROPS,NPROPS,MCRD,  C
C                      AWAVE,IWAVE,IELCON,DT,KNINC)                    C
C                                                                      C
C   Evaluates and assembles the displacement field.                    C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE DISPINCT4(IDF,U0,COORDS,NNODE,PROPS,NPROPS,MCRD,
     1                    AWAVE,IWAVE,DT,KINC,IOUT)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION U0(8), AWAVE(5),COORDS(MCRD,NNODE),PROPS(NPROPS)

      PARAMETER (ZERO=0.0D0,ONE=1.0D0,HALF=0.5D0,TWO=2.0D0,FOUR=4.0D0)

C     Incident wave properties.

      FC=AWAVE(3)
      TMAX=AWAVE(1)
      TINI=AWAVE(2)
      AMPR=AWAVE(4)
      PHI=AWAVE(5)

C     Elastic properties

      AALFA=PROPS(1)
      ABETA=PROPS(2)
      RO=PROPS(3)

C     Compute incoming dispalcements for all the nodes in
C     an element.
      
      DO II=1,NNODE
        X1=COORDS(1,II)
        X3=COORDS(2,II)

        IF(IWAVE.EQ.1) THEN

          US1=0.0D0
          US2=0.0D0

          CALL PWDISPT(FC,TINI,AMPR,PHI,AALFA,ABETA,X1,X3,
     1                  KINC,DT,US1,US2)

          U0(2*II-1)=US1
          U0(2*II  )=US2

        ELSE

          US1=0.0D0
          US2=0.0D0

          CALL SWDISPT(FC,TINI,AMPR,PHI,AALFA,ABETA,X1,X3,
     1                  KINC,DT,US1,US2)

          U0(2*II-1)=US1
          U0(2*II  )=US2
          
        END IF

      END DO

      RETURN

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE PWDISPT(FC,TMAX,TINI,AMP,PHI,AALFA,ABETA,RO,X1,X3,US1,C
C    1                   US2,VS1,VS2,AS1,AS2,NF)                       C
C                                                                      C
C     Computes the displacements vector for a P-Wave incident at a     C
C     point X1,X3.                                                     C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE PWDISPT(FC,TINI,AMP,PHI,AALFA,ABETA,X1,X3,
     1                   KINC,DT,US1,US2)

      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.0D0,ONE=1.0D0,HALF=0.5D0,TWO=2.0D0,FOUR=4.0D0)

      PI=4.0D0*DATAN(ONE)

      PHIR=PHI*PI/180.0D0
      PL=DSIN(PHIR)/AALFA
      ZHIR=DASIN(ABETA*PL)

      SNX=DSIN(PHIR)/AALFA*X1
      CNZ=DCOS(PHIR)/AALFA*X3
      ENX=DSIN(ZHIR)/ABETA*X1
      ENZ=DCOS(ZHIR)/ABETA*X3

      F1=ONE/ABETA**2-TWO*PL**2
      F1S=F1**2
      F2=FOUR*PL**2*(DCOS(PHIR)/AALFA)*(DCOS(ZHIR)/ABETA)

C     Compute reflection coefficients (PP,PS) and related constants.

      PP=(F2-F1S)/(F2+F1S)
      PS=(FOUR*AALFA/ABETA*PL*(DCOS(PHIR)/AALFA)*F1)/(F1S+F2)

C     Incident P-wave field.

      tao=(SNX-CNZ)-(DT*(KINC-1)-TINI)        
      upft=(2.0D0*(PI*tao*FC)**2-1.0D0)*DEXP(-(PI*tao*FC)**2)

      UPPI= AMP*DSIN(PHIR)*upft
      WPPI=-AMP*DCOS(PHIR)*upft

C     Reflected P-wave field.

      tao=(SNX+CNZ)-(DT*(KINC-1)-TINI)
      upft=(2.0D0*(PI*tao*FC)**2-1.0D0)*DEXP(-(PI*tao*FC)**2)

      UPPR=AMP*DSIN(PHIR)*PP*upft
      WPPR=AMP*DCOS(PHIR)*PP*upft

C     Reflected S-wave field.

      tao=(ENX+ENZ)-(DT*(KINC-1)-TINI)
      upft=(2.0D0*(PI*tao*FC)**2-1.0D0)*DEXP(-(PI*tao*FC)**2)

      UPSR= AMP*DCOS(ZHIR)*PS*upft
      WPSR=-AMP*DSIN(ZHIR)*PS*upft

C     Total wave field.

      US1=UPPI+UPPR+UPSR
      US2=WPPI+WPPR+WPSR

      RETURN

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE SWDISPT(FC,TMAX,TINI,AMP,ZHI,AALFA,ABETA,RO,X1,X3,US1,C
C    1                   US2,VS1,VS2,AS1,AS2,NF)                       C
C                                                                      C
C     Computes the displacements vector for a P-Wave incident at a     C
C     point X1,X3.                                                     C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SWDISPT(FC,TINI,AMP,ZHI,AALFA,ABETA,X1,X3,
     1                   KINC,DT,US1,US2)

      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (ZERO=0.0D0,ONE=1.0D0,HALF=0.5D0,TWO=2.0D0,FOUR=4.0D0)

      PI=4.0D0*DATAN(ONE)

      ZHIR=ZHI*PI/180.D0
      PL=DSIN(ZHIR)/ABETA
      PHIR=DASIN(AALFA*PL)

      SNX=DSIN(PHIR)/AALFA*X1
      CNZ=DCOS(PHIR)/AALFA*X3
      ENX=DSIN(ZHIR)/ABETA*X1
      ENZ=DCOS(ZHIR)/ABETA*X3

      F1=ONE/ABETA**2-TWO*PL**2
      F1S=F1**2
      F2=FOUR*PL**2*(DCOS(PHIR)/AALFA)*(DCOS(ZHIR)/ABETA)

C     Compute reflection coefficients PP,PS and related constants.

      SS=(F1S-F2)/(F2+F1S)
      SP=(FOUR*ABETA/AALFA*PL*(DCOS(ZHIR)/ABETA)*F1)/(F1S+F2)

C     Incident SV-wave field.

      tao=(ENX-ENZ)-(DT*(KINC-1)-TINI)
      usft=(2.0D0*(PI*tao*FC)**2-1.0D0)*DEXP(-(PI*tao*FC)**2)
      USVI= AMP*DCOS(ZHIR)*usft
      WSVI= AMP*DSIN(ZHIR)*usft

C     Reflected P-wave field.

      tao=(SNX+CNZ)-(DT*(KINC-1)-TINI)
      usft=(2.0D0*(PI*tao*FC)**2-1.D0)*DEXP(-(PI*tao*FC)**2)
      USPR= AMP*DSIN(PHIR)*SP*usft
      WSPR= AMP*DCOS(PHIR)*SP*usft
      
C     Reflected SV-wave field.

      tao=(ENX+ENZ)-(DT*(KINC-1)-TINI)
      usft=(2.0D0*(PI*tao*FC)**2-1.0D0)*DEXP(-(PI*tao*FC)**2)
      USVR= AMP*DCOS(ZHIR)*SS*usft
      WSVR=-AMP*DSIN(ZHIR)*SS*usft

C     Total wave field.
        
      US1=USVI+USPR+USVR
      US2=WSVI+WSPR+WSVR

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE LOCIEL(IELCON)                                          C
C                                                                      C
C   Creates the connectivity array for an 8-noded 2D element which is  C
C   considered like an assembly of 4 3-noded 1D elements               C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE LOCIEL(IELCON)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION IELCON(4,3)

      IELCON(1,1)=1
      IELCON(1,2)=2
      IELCON(1,3)=5

      IELCON(2,1)=2
      IELCON(2,2)=3
      IELCON(2,3)=6

      IELCON(3,1)=3
      IELCON(3,2)=4
      IELCON(3,3)=7

      IELCON(4,1)=4
      IELCON(4,2)=1
      IELCON(4,3)=8

      RETURN

      END

C23456789012345678901234567890123456789012345678901234567890123456789012
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C   SUBROUTINE LOCIEL2(IELCON)                                         C
C                                                                      C
C   Creates the connectivity array for an 8-noded 2D element which is  C
C   considered like an assembly of 4 3-noded 1D elements               C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C23456789012345678901234567890123456789012345678901234567890123456789012

      SUBROUTINE LOCIEL2(IELCON)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION IELCON(4,2)

      IELCON(1,1)=1
      IELCON(1,2)=2

      IELCON(2,1)=2
      IELCON(2,2)=3

      IELCON(3,1)=3
      IELCON(3,2)=4

      IELCON(4,1)=4
      IELCON(4,2)=1

      RETURN

      END
































