C
C
C program:
C GRPY
C
C
C author:
C Pawel Jan Zuk
C 
C
C Copyright (C) 2017
C Pawel Jan Zuk
C 
C
C This program calculates hydrodynamic properties of rigid bead models of macromolecules
C
C 
C This library is free software; you can redistribute it and/or
C modify it under the terms of the GNU General Public License version 3
C
C

*****************************************

      MODULE SIZE
      INTEGER NN
      END MODULE SIZE

*****************************************

      MODULE DIAMETERS
      REAL*8,  ALLOCATABLE :: DIAM_SPH(:)
      END MODULE DIAMETERS

*****************************************

      MODULE TENSORS  
      REAL*8 :: U(3,3),EPS(3,3,3),Y2(5,3,3),T4I(3,3,3,3)
      END MODULE TENSORS

******************************************

      MODULE THREADS
      INTEGER NCORES
      END MODULE THREADS

*****************************************


      PROGRAM MAIN
      USE SIZE
      USE DIAMETERS
      IMPLICIT NONE
      REAL*8 FDD,MDD,BR,UNITS
      REAL*8 AR(11,11),RC(3),RR(3),V,ARR(3,3),DRR(3),ATR(6,6),DTR(6,6)
      REAL*8 RG2,SI(3),DELTA,TAU(8),DTAU,TK,ETA,MW,MWI,VBAR,RHO,TOTMASS
      REAL*8 D12(3),DD12,VO,R1,R2,RCH(3),ATRCH(6,6)
      REAL*8, ALLOCATABLE :: CONF(:,:),A(:,:),RADII(:)
      CHARACTER COORDFILENAME*120,SETTINGSFILE*70,COORDFILE*150,FLAG*4
      CHARACTER PARTNAME*30,OUTPUTFILENAME*120,OUTPUTFILE*150,UTEMP*150
      CHARACTER INPUTTYPE*10,STATUSWORD*29
      INTEGER NARG,I,J,SEED,UN,STAT,USINPUT
      REAL*8 EIV(3,3),EIVINV(3,3)
      REAL, PARAMETER :: PI = 3.1415925359

200   FORMAT(' ',A80)
201   FORMAT(' ',A50,A20)
202   FORMAT(' ',A70)

300   FORMAT(' ',A32)

      CALL INIT_THREADS

      CALL INIT_U
      CALL INIT_EPS
      CALL INIT_Y2
      CALL INIT_T4I

      NARG=IARGC()
      
      IF (NARG.EQ.2) THEN

       CALL getarg(1,FLAG)
       CALL getarg(2,SETTINGSFILE)

       IF (TRIM(FLAG).NE.'-d') THEN
        IF (TRIM(FLAG).NE.'-u') THEN
         IF (TRIM(FLAG).NE.'-e') THEN
          CALL WRONGINPUT()
          STOP
         ENDIF
        ENDIF
       ENDIF


       IF (TRIM(FLAG).EQ.'-d') THEN

        INPUTTYPE="hydro++"
        
        OPEN(32,FILE=SETTINGSFILE)
        DO
         READ(32,'(A)') PARTNAME
         IF (PARTNAME(1:1).EQ.'*') THEN
          STOP
         ENDIF
         READ(32,*) OUTPUTFILENAME
         READ(32,*) COORDFILENAME
         J=0
         DO I=1,LEN(SETTINGSFILE)
          IF (SETTINGSFILE(I:I).EQ.'/') THEN
           J=I
          ENDIF
         ENDDO
         READ(32,*) 
         READ(32,*) TK
         READ(32,*) ETA
         READ(32,*) MW
         READ(32,*) VBAR
         READ(32,*) RHO
         READ(32,*) 
         READ(32,*) 
         READ(32,*) 
         READ(32,*) 
         READ(32,*) 
         READ(32,*) 

         TK=TK + 273.15
 
         WRITE(*,*) 'particle name: ',TRIM(PARTNAME)
         WRITE(*,*) 'output file name: ',TRIM(OUTPUTFILENAME)
     *   // '-GRPY.dat'

         IF (J.GT.0) THEN
          COORDFILE = SETTINGSFILE(1:J) // TRIM(COORDFILENAME)
          OUTPUTFILE = SETTINGSFILE(1:J) // TRIM(OUTPUTFILENAME) 
     *                   // '-GRPY.dat'
         ELSE
          COORDFILE = TRIM(COORDFILENAME)
          OUTPUTFILE = TRIM(OUTPUTFILENAME) // '-GRPY.dat'
         ENDIF

         OPEN(31,FILE=TRIM(COORDFILE))
         READ(31,*) UNITS
         READ(31,*) NN
 
         IF( .NOT.ALLOCATED( CONF ) ) THEN
          ALLOCATE( CONF(3,NN),A(11*NN,11*NN),RADII(NN) )
         ENDIF
 
         IF( .NOT.ALLOCATED( DIAM_SPH ) ) THEN
          ALLOCATE( DIAM_SPH(NN) )
         ENDIF
  
         V=0.D0
         DO I=1,NN
          READ(31,*) CONF(1,I),CONF(2,I),CONF(3,I),RADII(I)
          DIAM_SPH(I) = 2.D0*RADII(I)
         ENDDO

         CLOSE(31)

         CALL CALCRG2(CONF,RADII,RC,RG2)

         RR = 0.D0

         CALL HYDRO(A,AR,CONF,RR)
         CALL INTRINSIC_HIGH(AR,MDD,FDD)
         CALL BROWN_EW(AR,BR)

         ATR = AR(1:6,1:6)
         CALL MATREV(ATR,6,'M')
         CALL CALCRCH(ATR,RCH)
         RCH = RR + RCH

         CALL CALCATRCH(ATRCH,AR,EIV,DRR,RCH)

         CALL CALCTAU(DTAU,TAU,DRR)

         CALL WRITEFILE(OUTPUTFILE,ATR,RR,ATRCH,RCH,UNITS,ETA,
     *             TK,VBAR,RHO,RG2,MDD,BR,DTAU,TAU,MW,EIV,INPUTTYPE)
         DEALLOCATE(CONF,A,RADII,DIAM_SPH)

         WRITE(*,*) 'completed'
        ENDDO
       
       ELSE IF (TRIM(FLAG).EQ.'-u') THEN

        INPUTTYPE='us-somo'

        OPEN(32,FILE=SETTINGSFILE)

        READ(32,*) NN,VBAR
 
        ALLOCATE( CONF(3,NN),A(11*NN,11*NN),RADII(NN) )

        IF( .NOT.ALLOCATED( DIAM_SPH ) ) THEN
         ALLOCATE( DIAM_SPH(NN))
        ENDIF
  
        V=0.D0
        MW=0.D0
        DO I=1,NN
         READ(32,*) CONF(1,I),CONF(2,I),CONF(3,I),RADII(I),MWI
         MW=MW+MWI
         DIAM_SPH(I) = 2.D0*RADII(I)
        ENDDO

        READ(32,*)
        READ(32,*)
        READ(32,*)
        READ(32,*)
        READ(32,*)
        READ(32,*)
        READ(32,*)
        READ(32,*)
        READ(32,'(A)') UTEMP
        READ(UTEMP(SCAN(TRIM(UTEMP),":")+2:LEN(TRIM(UTEMP))),
     *       *,IOSTAT=STAT)  UN
       
        UNITS = 10.D0**(2 - UN)
        TK = 20
        ETA = 0.01D0
        RHO = 1.D0
        TK = TK + 273.15

        CLOSE(32)

        CALL CALCRG2(CONF,RADII,RC,RG2)

        RR = 0.D0

        CALL HYDRO(A,AR,CONF,RR)
        CALL INTRINSIC_HIGH(AR,MDD,FDD)
        CALL BROWN_EW(AR,BR)

        ATR = AR(1:6,1:6)
        CALL MATREV(ATR,6,'M')
        CALL CALCRCH(ATR,RCH)
        RCH = RR + RCH

        CALL CALCATRCH(ATRCH,AR,EIV,DRR,RCH)
        
        CALL CALCTAU(DTAU,TAU,DRR)

        CALL WRITESTDOUT(ATR,RR,ATRCH,RCH,UNITS,ETA,TK,VBAR,RHO,
     *                      RG2,MDD,BR,DTAU,TAU,MW,EIV,INPUTTYPE)

        DEALLOCATE(CONF,A,RADII,DIAM_SPH)

       ELSE IF (TRIM(FLAG).EQ.'-e') THEN

        WRITE(UNIT=STATUSWORD,FMT='(A29)') 'READING DATA'
        CALL PROGRESSSTATUS(0,STATUSWORD)

        INPUTTYPE='GRPY'
 
        CALL getarg(2,SETTINGSFILE)      
 
        OPEN(32,FILE=SETTINGSFILE)
 
        READ(32,'(A)') PARTNAME
        READ(32,*) TK
        READ(32,*) ETA
        READ(32,*) MW
        READ(32,*) VBAR
        READ(32,*) RHO
        READ(32,*) UNITS
        READ(32,*) NN
 
        TK = TK + 273.15

        ALLOCATE( CONF(3,NN),A(11*NN,11*NN),RADII(NN) )
 
        IF( .NOT.ALLOCATED( DIAM_SPH ) ) THEN
         ALLOCATE( DIAM_SPH(NN))
        ENDIF

        V=0.D0
        DO I=1,NN
         READ(32,*) CONF(1,I),CONF(2,I),CONF(3,I),RADII(I)
         DIAM_SPH(I) = 2.D0*RADII(I)
        ENDDO
 
        CLOSE(32)

        WRITE(UNIT=STATUSWORD,FMT='(A29)') 'CALCULATING RC AND RG'
        CALL PROGRESSSTATUS(1,STATUSWORD)

        CALL CALCRG2(CONF,RADII,RC,RG2)
 
        RR = 0.D0
       
        CALL HYDROPROGRESS(A,AR,CONF,RR)

        WRITE(UNIT=STATUSWORD,FMT='(A29)') 'CALCULATING PROPERTIES'
        CALL PROGRESSSTATUS(98,STATUSWORD)

        CALL INTRINSIC_HIGH(AR,MDD,FDD)
        CALL BROWN_EW(AR,BR)
 
        ATR = AR(1:6,1:6)
        CALL MATREV(ATR,6,'M')
        CALL CALCRCH(ATR,RCH)
        RCH = RR + RCH
 
        CALL CALCATRCH(ATRCH,AR,EIV,DRR,RCH)
 
        CALL CALCTAU(DTAU,TAU,DRR)

        WRITE(UNIT=STATUSWORD,FMT='(A29)') 'COMPLETE'
        CALL PROGRESSSTATUS(100,STATUSWORD)
        WRITE(*,FMT='(A,A40,$)') CHAR(13),''

        CALL WRITESTDOUT(ATR,RR,ATRCH,RCH,UNITS,ETA,TK,VBAR,RHO,
     *                     RG2,MDD,BR,DTAU,TAU,MW,EIV,INPUTTYPE)
 
        DEALLOCATE(CONF,A,RADII,DIAM_SPH)

       ENDIF

      ELSEIF (NARG.EQ.1) THEN

       INPUTTYPE='GRPY'

       CALL getarg(1,SETTINGSFILE)      

       OPEN(32,FILE=SETTINGSFILE)

       READ(32,'(A)') PARTNAME
       READ(32,*) TK
       READ(32,*) ETA
       READ(32,*) MW
       READ(32,*) VBAR
       READ(32,*) RHO
       READ(32,*) UNITS
       READ(32,*) NN

       TK = TK + 273.15
 
       ALLOCATE( CONF(3,NN),A(11*NN,11*NN),RADII(NN) )

       IF( .NOT.ALLOCATED( DIAM_SPH ) ) THEN
        ALLOCATE( DIAM_SPH(NN))
       ENDIF
  
       V=0.D0
       DO I=1,NN
        READ(32,*) CONF(1,I),CONF(2,I),CONF(3,I),RADII(I)
        DIAM_SPH(I) = 2.D0*RADII(I)
       ENDDO

       CLOSE(32)

       CALL CALCRG2(CONF,RADII,RC,RG2)

       RR = 0.D0

       CALL HYDRO(A,AR,CONF,RR)
       CALL INTRINSIC_HIGH(AR,MDD,FDD)
       CALL BROWN_EW(AR,BR)

       ATR = AR(1:6,1:6)
       CALL MATREV(ATR,6,'M')
       CALL CALCRCH(ATR,RCH)
       RCH = RR + RCH

       CALL CALCATRCH(ATRCH,AR,EIV,DRR,RCH)
       
       CALL CALCTAU(DTAU,TAU,DRR)

       CALL WRITESTDOUT(ATR,RR,ATRCH,RCH,UNITS,ETA,TK,VBAR,RHO,
     *                     RG2,MDD,BR,DTAU,TAU,MW,EIV,INPUTTYPE)

       DEALLOCATE(CONF,A,RADII,DIAM_SPH)

      ELSE 

       CALL WRONGINPUT()
       STOP

      ENDIF

      STOP
      END


C***********************************************************
C***********************************************************
C***********************************************************


      SUBROUTINE WRONGINPUT()

      WRITE(*,*) 
      WRITE(*,*) 'ERROR: wrong input specified'     
      WRITE(*,*) 'please execute GRPY program ' //
     *           'in the following way:' 
      WRITE(*,*)
      WRITE(*,*) '1) ./GRPY.exe <input file>    ' //
     *          '  --  input in the GRPY format'     
      WRITE(*,*) 
      WRITE(*,*) '2) ./GRPY.exe -e <input file> ' //
     *          '  --  input in the GRPY format' //
     *          ' to additionally display program progress information' 
      WRITE(*,*) 
      WRITE(*,*) '3) ./GRPY.exe -d <input file> ' //
     *          '  --  input in the hydro++10 format'     
      WRITE(*,*) 
      WRITE(*,*) '4) ./GRPY.exe -u <input file> ' //
     *          '  --  input in the us-somo .bead_model format'     

      RETURN
      END
 
C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALCRG2(CONF,RADII,RC,RG2)
      USE SIZE
      IMPLICIT NONE
      REAL*8 RC(3),RG2,TOTMASS,SI(3),CONF(3,NN),RADII(NN)
      INTEGER I

      RC=0.D0
      TOTMASS=0.D0
      DO I=1,NN
       RC=RC+CONF(:,I)*RADII(I)**3.D0
       TOTMASS=TOTMASS+RADII(I)**3.D0
      ENDDO
      RC=RC/TOTMASS

      RG2=0.D0
      DO I=1,NN
       SI = CONF(:,I) - RC
       RG2 = RG2 + ( 3.D0/5.D0*(RADII(I)**2) 
     *        + SUM(SI*SI) ) * RADII(I)**3.D0 / TOTMASS
      ENDDO

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************


      SUBROUTINE CALCRCH(ATR,RCH)
      IMPLICIT NONE

      REAL*8 ATR(6,6),MRCH(3,3),RCH(3)
      INTEGER PIVOT(3),RCHOK

      MRCH=0.D0

      MRCH(1,1)=ATR(4,6)
      MRCH(1,2)=ATR(5,6)
      MRCH(1,3)=-ATR(4,4)-ATR(5,5)
      MRCH(2,1)=-ATR(4,5)
      MRCH(2,2)=ATR(4,4)+ATR(6,6)
      MRCH(2,3)=-ATR(6,5)
      MRCH(3,1)=-ATR(5,5)-ATR(6,6)
      MRCH(3,2)=ATR(5,4)
      MRCH(3,3)=ATR(6,4)

      RCH(1)=ATR(5,1)-ATR(4,2)
      RCH(2)=ATR(6,1)-ATR(4,3)
      RCH(3)=ATR(6,2)-ATR(5,3)

      CALL DGESV(3, 1, MRCH, 3, PIVOT, RCH, 3, RCHOK)

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALCATRCH(ATRCH,AR,EIV,DRR,RCH)
      IMPLICIT NONE

      REAL*8 EIV(3,3),EIVINV(3,3),ARR(3,3),ATRCH(6,6),DRR(3)
      REAL*8 AR(11,11),RCH(3),EIJK(3,3,3),TMP(6,6,2)
      INTEGER I,J,K,L,N,M

      EIJK=0.D0
      EIJK(1,2,3)= 1.D0 
      EIJK(1,3,2)=-1.D0
      EIJK(2,3,1)= 1.D0
      EIJK(2,1,3)=-1.D0
      EIJK(3,1,2)= 1.D0
      EIJK(3,2,1)=-1.D0

      ATRCH = AR(1:6,1:6)
      CALL MATREV(ATRCH,6,'M')


      ATRCH=ATRCH
      TMP=0.D0
      TMP(:,:,1)=ATRCH
      DO L=1,3
       DO I=1,3 
        DO J=1,3
         DO K=1,3
          DO M=1,3
           DO N=1,3
          TMP(L,I,2) = TMP(L,I,2) 
     *               + RCH(K)*EIJK(L,K,M)
     *               * TMP(3+M,3+N,1)
     *               * RCH(J)*EIJK(I,N,J)
           ENDDO
          ENDDO
          TMP(3+I,L,2) = TMP(3+I,L,2) 
     *          + RCH(J)*TMP(3+K,L,1)*EIJK(I,J,K)
          TMP(L,3+I,2) = TMP(L,3+I,2) 
     *                 + TMP(L,3+J,1)*RCH(K)*EIJK(I,J,K)
          TMP(3+L,3+I,2) = TMP(3+L,3+I,2) 
     *                   + TMP(3+L,3+J,1)*RCH(K)*EIJK(I,J,K)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      ATRCH(1:3,1:3) = ATRCH(1:3,1:3) - TMP(1:3,1:3,2)
     *          - TMP(4:6,1:3,2) + TMP(1:3,4:6,2)

      ATRCH(4:6,1:3) = ATRCH(4:6,1:3) + TMP(4:6,4:6,2)
      ATRCH(1:3,4:6) = TRANSPOSE(ATRCH(4:6,1:3))

      ARR = ATRCH(4:6,4:6)
      EIV = ARR
      DRR = 0.D0
      CALL EIGEN3X3(DRR,EIV,EIVINV)
      
      ATRCH(4:6,4:6) = MATMUL(EIVINV,MATMUL(ARR,EIV))
      ARR = ATRCH(1:3,1:3)
      ATRCH(1:3,1:3) = MATMUL(EIVINV,MATMUL(ARR,EIV))
      ARR = ATRCH(1:3,4:6)
      ATRCH(1:3,4:6) = MATMUL(EIVINV,MATMUL(ARR,EIV))
      ARR = ATRCH(4:6,1:3)
      ATRCH(4:6,1:3) = MATMUL(EIVINV,MATMUL(ARR,EIV))


      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************


      SUBROUTINE CALCTAU(DTAU,TAU,DRR)
      IMPLICIT NONE

      REAL*8 DRR(3),DTAU,TAU(8),DELTA

      DTAU = (SUM(DRR))/3.D0
      DELTA = (DRR(1)**2 + DRR(2)**2 + DRR(3)**2
     *        -DRR(1)*DRR(2) - DRR(1)*DRR(3) - DRR(2)*DRR(3))
      CALL NUMIFZERO(DELTA)
      DELTA = SQRT(DELTA) 
      TAU(1) = 1.D0/(6.D0*DTAU + 2.D0*DELTA)
      TAU(2) = 1.D0/(6.D0*DTAU - 2.D0*DELTA)
      TAU(3) = 1.D0/3.D0/(DTAU + DRR(1))
      TAU(4) = 1.D0/3.D0/(DTAU + DRR(2))
      TAU(5) = 1.D0/3.D0/(DTAU + DRR(3))
      TAU(6) = 1.D0/(3.D0*DTAU - DRR(1))
      TAU(7) = 1.D0/(3.D0*DTAU - DRR(2))
      TAU(8) = 1.D0/(3.D0*DTAU - DRR(3))

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE WRITEFILE(OUTPUTFILE,ATR,RR,ATRCH,RCH,UNITS,ETA,
     *           TK,VBAR,RHO,RG2,MDD,BR,DTAU,TAU,MW,EIV,INPUTTYPE)
      REAL*8 MDD,BR,UNITS,DTAU,RR(3),RCH(3),EIV(3,3)
      REAL*8 ATR(6,6),DTR(6,6),TAU(8),ATRCH(6,6)
      REAL*8 RG2,SI(3),DELTA,TK,ETA,MW,VBAR,RHO
      CHARACTER OUTPUTFILE*150,INPUTTYPE*10
      REAL, PARAMETER :: PI = 3.1415925359
      REAL, PARAMETER :: KB = 1.38064852E-16
      REAL, PARAMETER :: NA = 6.022140858E+23

100   FORMAT(' ',A70,ES11.3,A11)
101   FORMAT(' ',ES11.3,ES11.3,ES11.3,A3,ES11.3,ES11.3,ES11.3)
105   FORMAT(' ',ES11.3,ES11.3,ES11.3)
106   FORMAT(' ',A11,A11,A11)
102   FORMAT(' ',A40,A15)
103   FORMAT(' ',A60)
107   FORMAT(' ',A11,ES11.3,ES11.3,ES11.3)

200   FORMAT(' ',A80)
201   FORMAT(' ',A50,A20)
202   FORMAT(' ',A70)

      OPEN(41,FILE=OUTPUTFILE)

      WRITE(41,*)
      WRITE(41,202) 'GRPY program'
      WRITE(41,*)
      WRITE(41,200) 'Hydrodynamic properties of the macromolecule'
      WRITE(41,200) 'based on the Generalized ' //
     *              'Rotne-Prager-Yamakawa method'
      WRITE(41,*)

      WRITE(41,201) 'from the:', TRIM(INPUTTYPE)//' input file'
      WRITE(41,*) 
      WRITE(41,100) 'Rotational diffusion coefficient:',
     *          DTAU
     *          *(KB*TK)/((UNITS**3)*PI*ETA),
     *          '[s^-1]' // '    '
      WRITE(41,100) 'Rotational relaxation times associated with'
      WRITE(41,100) 'the Brownian relaxation of a vector:'
      WRITE(41,100) '-> Relaxation time (1):',
     *          TAU(6)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(41,100) '-> Relaxation time (2):',
     *          TAU(7)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(41,100) '-> Relaxation time (3):',
     *          TAU(8)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(41,100) 'Rotational relaxation times associated with'
      WRITE(41,100) 'the Brownian relaxation of'
     *              // ' a traceless symmetric tensor:'
      WRITE(41,100) '-> Relaxation time (1):',
     *          TAU(1)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(41,100) '-> Relaxation time (2):',
     *          TAU(2)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(41,100) '-> Relaxation time (3):',
     *          TAU(3)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(41,100) '-> Relaxation time (4):',
     *          TAU(4)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(41,100) '-> Relaxation time (5):',
     *          TAU(5)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(41,100) 'Harmonic mean (correlation) time:',
     *          5.D0/(1.D0/TAU(1) + 1.D0/TAU(2) + 1.D0/TAU(3)
     *           + 1.D0/TAU(4) + 1.D0/TAU(5))
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
 
      WRITE(41,100) 'Sedimentation coefficient ' //
     *           '(Mw Dlt (1. - (vbar*rho))/(nA kB T)):',
     *          MW*(1.D0 - (VBAR*RHO))*1.E+13/NA
     *        *((ATRCH(1,1)+ATRCH(2,2)+ATRCH(3,3))/3.D0)/(UNITS*PI*ETA),
     *          '[Svedberg]'

      WRITE(41,*)

      WRITE(41,100) 'High frequency intrinsic viscosity eta oo:',
     *          MDD
     *          * PI*NA/MW*(UNITS**3),
     *          '[cm^3/g]' // '  '
      WRITE(41,100) 'Zero frequency intrinsic viscosity eta 0:',
     *          (MDD + BR)
     *          * PI*NA/MW*(UNITS**3),
     *          '[cm^3/g]' // '  '

      WRITE(41,*)
      WRITE(41,102) ' calculated using the origin',''
      WRITE(41,102) ' of the coordinate system:',''
      WRITE(41,105) 0.D0,0.D0,0.D0
      WRITE(41,102) ' as the reference point',''
      WRITE(41,102) ' in the standard reference frame:',''
      WRITE(41,107) ' e1:',1.D0,0.D0,0.D0
      WRITE(41,107) ' e2:',0.D0,1.D0,0.D0
      WRITE(41,107) ' e3:',0.D0,0.D0,1.D0

      WRITE(41,*)

      WRITE(41,100) 'Translational diffusion coefficient:',
     *          ((ATR(1,1)+ATR(2,2)+ATR(3,3))/3.D0)
     *          * KB*TK/(UNITS*PI*ETA),
     *          '[cm^2/s]' // '  '
      WRITE(41,*)
      WRITE(41,102) ' 6x6 diffusion matrix: ','Dtt  Dtr'
      WRITE(41,*) 
      WRITE(41,102) '','Drt  Drr'
      WRITE(41,*)
      DTR(1:3,1:3) = ATR(1:3,1:3) * KB*TK/(UNITS*PI*ETA)
      DTR(4:6,4:6) = ATR(4:6,4:6) * KB*TK/((UNITS**3)*PI*ETA)
      DTR(1:3,4:6) = ATR(1:3,4:6) * KB*TK/((UNITS**2)*PI*ETA)
      DTR(4:6,1:3) = ATR(4:6,1:3) * KB*TK/((UNITS**2)*PI*ETA)

      WRITE(41,101) DTR(1,1),DTR(1,2),DTR(1,3),
     *             " ",DTR(1,4),DTR(1,5),DTR(1,6)
      WRITE(41,101) DTR(2,1),DTR(2,2),DTR(2,3),
     *             " ",DTR(2,4),DTR(2,5),DTR(2,6)
      WRITE(41,101) DTR(3,1),DTR(3,2),DTR(3,3),
     *             " ",DTR(3,4),DTR(3,5),DTR(3,6)
      WRITE(41,*)
      WRITE(41,101) DTR(4,1),DTR(4,2),DTR(4,3),
     *             " ",DTR(4,4),DTR(4,5),DTR(4,6)
      WRITE(41,101) DTR(5,1),DTR(5,2),DTR(5,3),
     *             " ",DTR(5,4),DTR(5,5),DTR(5,6)
      WRITE(41,101) DTR(6,1),DTR(6,2),DTR(6,3),
     *             " ",DTR(6,4),DTR(6,5),DTR(6,6)
     
      WRITE(41,*)

      WRITE(41,102) 'calculated using the mobility center:',''
      RCH=RCH*UNITS
      WRITE(41,106) 'x [cm]','y [cm]','z [cm]'
      WRITE(41,105) RCH(1),RCH(2),RCH(3)
      WRITE(41,102) ' as the reference point',''
      WRITE(41,102) ' in the reference frame:',''
      WRITE(41,107) ' e1:',EIV(:,1)
      WRITE(41,107) ' e2:',EIV(:,2)
      WRITE(41,107) ' e3:',EIV(:,3)

      WRITE(41,*)

      WRITE(41,100) 'Translational diffusion coefficient:',
     *          ((ATRCH(1,1)+ATRCH(2,2)+ATRCH(3,3))/3.D0)
     *          * KB*TK/(UNITS*PI*ETA),
     *          '[cm^2/s]' // '  '

      WRITE(41,*)
      WRITE(41,102) ' 6x6 diffusion matrix: ','Dtt  Dtr'
      WRITE(41,*) 
      WRITE(41,102) '','Drt  Drr'
      WRITE(41,*)

      DTR(1:3,1:3) = ATRCH(1:3,1:3) * KB*TK/(UNITS*PI*ETA)
      DTR(4:6,4:6) = ATRCH(4:6,4:6) * KB*TK/((UNITS**3)*PI*ETA)
      DTR(1:3,4:6) = ATRCH(1:3,4:6) * KB*TK/((UNITS**2)*PI*ETA)
      DTR(4:6,1:3) = ATRCH(4:6,1:3) * KB*TK/((UNITS**2)*PI*ETA)

      WRITE(41,101) DTR(1,1),DTR(1,2),DTR(1,3),
     *             " ",DTR(1,4),DTR(1,5),DTR(1,6)
      WRITE(41,101) DTR(2,1),DTR(2,2),DTR(2,3),
     *             " ",DTR(2,4),DTR(2,5),DTR(2,6)
      WRITE(41,101) DTR(3,1),DTR(3,2),DTR(3,3),
     *             " ",DTR(3,4),DTR(3,5),DTR(3,6)
      WRITE(41,*)
      WRITE(41,101) DTR(4,1),DTR(4,2),DTR(4,3),
     *             " ",DTR(4,4),DTR(4,5),DTR(4,6)
      WRITE(41,101) DTR(5,1),DTR(5,2),DTR(5,3),
     *             " ",DTR(5,4),DTR(5,5),DTR(5,6)
      WRITE(41,101) DTR(6,1),DTR(6,2),DTR(6,3),
     *             " ",DTR(6,4),DTR(6,5),DTR(6,6)

      WRITE(41,*)

      WRITE(41,100) 'Radius of the sphere with equal:'
      WRITE(41,*)
      WRITE(41,100) 'Translational diffusion coefficient:',
     *          UNITS/((ATRCH(1,1)+ATRCH(2,2)+ATRCH(3,3))/3.D0)/6.D0,
     *          '[cm]' // '      '
       WRITE(41,100) 'Rotational diffusion coefficient:',
     *          UNITS/((8.D0 * DTAU)**(1.D0/3.D0)),
     *          '[cm]' // '      '
      WRITE(41,100) 'Rotational relaxation times associated with'
      WRITE(41,100) 'the Brownian relaxation of a vector:'
       WRITE(41,100) '-> Relaxation time (1):',
     *          UNITS*((6.D0*TAU(6)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(41,100) '-> Relaxation time (2):',
     *          UNITS*((6.D0*TAU(7)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(41,100) '-> Relaxation time (3):',
     *          UNITS*((6.D0*TAU(8)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
      WRITE(41,100) 'Rotational relaxation times associated with'
      WRITE(41,100) 'the Brownian relaxation of'
     *              // ' a traceless symmetric tensor:'
       WRITE(41,100) '-> Relaxation time (1):',
     *          UNITS*((6.D0*TAU(1)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(41,100) '-> Relaxation time (2):',
     *          UNITS*((6.D0*TAU(2)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(41,100) '-> Relaxation time (3):',
     *          UNITS*((6.D0*TAU(3)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(41,100) '-> Relaxation time (4):',
     *          UNITS*((6.D0*TAU(4)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(41,100) '-> Relaxation time (5):',
     *          UNITS*((6.D0*TAU(5)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(41,100) 'Mean relaxation time:',
     *          UNITS*((6.D0*
     *          5.D0/(1.D0/TAU(1) + 1.D0/TAU(2) + 1.D0/TAU(3)
     *           + 1.D0/TAU(4) + 1.D0/TAU(5))
     *          /(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(41,100) 'High frequency intrinsic viscosity eta oo:',
     *          (3.D0/10.D0 * MDD )**(1.D0/3.D0)*UNITS,
     *          '[cm]' // '      '
       WRITE(41,100) 'Zero frequency intrinsic viscosity eta 0:',
     *          (3.D0/10.D0 * (MDD+BR) )**(1.D0/3.D0)*UNITS,
     *          '[cm]' // '      '


      CLOSE(41)

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE WRITESTDOUT(ATR,RR,ATRCH,RCH,UNITS,ETA,TK,VBAR,RHO,
     *                     RG2,MDD,BR,DTAU,TAU,MW,EIV,INPUTTYPE)
      REAL*8 MDD,BR,UNITS,DTAU,RCH(3),RR(3),EIV(3,3)
      REAL*8 ATR(6,6),ATRCH(6,6),DTR(6,6),TAU(8)
      REAL*8 RG2,SI(3),DELTA,TK,ETA,MW,VBAR,RHO
      CHARACTER INPUTTYPE*10
      REAL, PARAMETER :: PI = 3.1415925359
      REAL, PARAMETER :: KB = 1.38064852E-16
      REAL, PARAMETER :: NA = 6.022140858E+23

100   FORMAT(' ',A70,ES11.3,A11)
101   FORMAT(' ',ES11.3,ES11.3,ES11.3,A3,ES11.3,ES11.3,ES11.3)
105   FORMAT(' ',ES11.3,ES11.3,ES11.3)
106   FORMAT(' ',A11,A11,A11)
102   FORMAT(' ',A40,A15)
103   FORMAT(' ',A60)
107   FORMAT(' ',A11,ES11.3,ES11.3,ES11.3)

200   FORMAT(' ',A80)
201   FORMAT(' ',A50,A20)
202   FORMAT(' ',A70)

      WRITE(*,*)
      WRITE(*,202) 'GRPY program'
      WRITE(*,*)
      WRITE(*,200) 'Hydrodynamic properties of the macromolecule'
      WRITE(*,200) 'based on the Generalized ' //
     *              'Rotne-Prager-Yamakawa method'
      WRITE(*,*)

      WRITE(*,201) 'from the:', TRIM(INPUTTYPE)//' input file'
      WRITE(*,*) 
      WRITE(*,100) 'Rotational diffusion coefficient:',
     *          DTAU
     *          *(KB*TK)/((UNITS**3)*PI*ETA),
     *          '[s^-1]' // '    '
      WRITE(*,100) 'Rotational relaxation times associated with'
      WRITE(*,100) 'the Brownian relaxation of a vector:'
      WRITE(*,100) '-> Relaxation time (1):',
     *          TAU(6)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(*,100) '-> Relaxation time (2):',
     *          TAU(7)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(*,100) '-> Relaxation time (3):',
     *          TAU(8)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(*,100) 'Rotational relaxation times associated with'
      WRITE(*,100) 'the Brownian relaxation of'
     *              // ' a traceless symmetric tensor:'
      WRITE(*,100) '-> Relaxation time (1):',
     *          TAU(1)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(*,100) '-> Relaxation time (2):',
     *          TAU(2)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(*,100) '-> Relaxation time (3):',
     *          TAU(3)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(*,100) '-> Relaxation time (4):',
     *          TAU(4)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(*,100) '-> Relaxation time (5):',
     *          TAU(5)
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
      WRITE(*,100) 'Harmonic mean (correlation) time:',
     *          5.D0/(1.D0/TAU(1) + 1.D0/TAU(2) + 1.D0/TAU(3)
     *           + 1.D0/TAU(4) + 1.D0/TAU(5))
     *          * ((UNITS**3)*PI*ETA)/(KB*TK),
     *          '[s]' // '       '
 
      WRITE(*,100) 'Sedimentation coefficient ' //
     *           '(Mw Dlt (1. - (vbar*rho))/(nA kB T)):',
     *          MW*(1.D0 - (VBAR*RHO))*1.E+13/NA
     *        *((ATRCH(1,1)+ATRCH(2,2)+ATRCH(3,3))/3.D0)/(UNITS*PI*ETA),
     *          '[Svedberg]'

      WRITE(*,*)

      WRITE(*,100) 'High frequency intrinsic viscosity eta oo:',
     *          MDD
     *          * PI*NA/MW*(UNITS**3),
     *          '[cm^3/g]' // '  '
      WRITE(*,100) 'Zero frequency intrinsic viscosity eta 0:',
     *          (MDD + BR)
     *          * PI*NA/MW*(UNITS**3),
     *          '[cm^3/g]' // '  '

      WRITE(*,*)
      WRITE(*,102) ' calculated using the origin',''
      WRITE(*,102) ' of the coordinate system:',''
      WRITE(*,105) 0.D0,0.D0,0.D0
      WRITE(*,102) ' as the reference point',''
      WRITE(*,102) ' in the standard reference frame:',''
      WRITE(*,107) ' e1:',1.D0,0.D0,0.D0
      WRITE(*,107) ' e2:',0.D0,1.D0,0.D0
      WRITE(*,107) ' e3:',0.D0,0.D0,1.D0

      WRITE(*,*)

      WRITE(*,100) 'Translational diffusion coefficient:',
     *          ((ATR(1,1)+ATR(2,2)+ATR(3,3))/3.D0)
     *          * KB*TK/(UNITS*PI*ETA),
     *          '[cm^2/s]' // '  '
      WRITE(*,*)
      WRITE(*,102) ' 6x6 diffusion matrix: ','Dtt  Dtr'
      WRITE(*,*) 
      WRITE(*,102) '','Drt  Drr'
      WRITE(*,*)
      DTR(1:3,1:3) = ATR(1:3,1:3) * KB*TK/(UNITS*PI*ETA)
      DTR(4:6,4:6) = ATR(4:6,4:6) * KB*TK/((UNITS**3)*PI*ETA)
      DTR(1:3,4:6) = ATR(1:3,4:6) * KB*TK/((UNITS**2)*PI*ETA)
      DTR(4:6,1:3) = ATR(4:6,1:3) * KB*TK/((UNITS**2)*PI*ETA)

      WRITE(*,101) DTR(1,1),DTR(1,2),DTR(1,3),
     *             " ",DTR(1,4),DTR(1,5),DTR(1,6)
      WRITE(*,101) DTR(2,1),DTR(2,2),DTR(2,3),
     *             " ",DTR(2,4),DTR(2,5),DTR(2,6)
      WRITE(*,101) DTR(3,1),DTR(3,2),DTR(3,3),
     *             " ",DTR(3,4),DTR(3,5),DTR(3,6)
      WRITE(*,*)
      WRITE(*,101) DTR(4,1),DTR(4,2),DTR(4,3),
     *             " ",DTR(4,4),DTR(4,5),DTR(4,6)
      WRITE(*,101) DTR(5,1),DTR(5,2),DTR(5,3),
     *             " ",DTR(5,4),DTR(5,5),DTR(5,6)
      WRITE(*,101) DTR(6,1),DTR(6,2),DTR(6,3),
     *             " ",DTR(6,4),DTR(6,5),DTR(6,6)
     
      WRITE(*,*)

      WRITE(*,102) 'calculated using the mobility center:',''
      RCH=RCH*UNITS
      WRITE(*,106) 'x [cm]','y [cm]','z [cm]'
      WRITE(*,105) RCH(1),RCH(2),RCH(3)
      WRITE(*,102) ' as the reference point',''
      WRITE(*,102) ' in the reference frame:',''
      WRITE(*,107) ' e1:',EIV(:,1)
      WRITE(*,107) ' e2:',EIV(:,2)
      WRITE(*,107) ' e3:',EIV(:,3)

      WRITE(*,*)

      WRITE(*,100) 'Translational diffusion coefficient:',
     *          ((ATRCH(1,1)+ATRCH(2,2)+ATRCH(3,3))/3.D0)
     *          * KB*TK/(UNITS*PI*ETA),
     *          '[cm^2/s]' // '  '

      WRITE(*,*)
      WRITE(*,102) ' 6x6 diffusion matrix: ','Dtt  Dtr'
      WRITE(*,*) 
      WRITE(*,102) '','Drt  Drr'
      WRITE(*,*)

      DTR(1:3,1:3) = ATRCH(1:3,1:3) * KB*TK/(UNITS*PI*ETA)
      DTR(4:6,4:6) = ATRCH(4:6,4:6) * KB*TK/((UNITS**3)*PI*ETA)
      DTR(1:3,4:6) = ATRCH(1:3,4:6) * KB*TK/((UNITS**2)*PI*ETA)
      DTR(4:6,1:3) = ATRCH(4:6,1:3) * KB*TK/((UNITS**2)*PI*ETA)

      WRITE(*,101) DTR(1,1),DTR(1,2),DTR(1,3),
     *             " ",DTR(1,4),DTR(1,5),DTR(1,6)
      WRITE(*,101) DTR(2,1),DTR(2,2),DTR(2,3),
     *             " ",DTR(2,4),DTR(2,5),DTR(2,6)
      WRITE(*,101) DTR(3,1),DTR(3,2),DTR(3,3),
     *             " ",DTR(3,4),DTR(3,5),DTR(3,6)
      WRITE(*,*)
      WRITE(*,101) DTR(4,1),DTR(4,2),DTR(4,3),
     *             " ",DTR(4,4),DTR(4,5),DTR(4,6)
      WRITE(*,101) DTR(5,1),DTR(5,2),DTR(5,3),
     *             " ",DTR(5,4),DTR(5,5),DTR(5,6)
      WRITE(*,101) DTR(6,1),DTR(6,2),DTR(6,3),
     *             " ",DTR(6,4),DTR(6,5),DTR(6,6)

      WRITE(*,*)

      WRITE(*,100) 'Radius of the sphere with equal:'
      WRITE(*,*)
      WRITE(*,100) 'Translational diffusion coefficient:',
     *          UNITS/((ATRCH(1,1)+ATRCH(2,2)+ATRCH(3,3))/3.D0)/6.D0,
     *          '[cm]' // '      '
       WRITE(*,100) 'Rotational diffusion coefficient:',
     *          UNITS/((8.D0 * DTAU)**(1.D0/3.D0)),
     *          '[cm]' // '      '
      WRITE(*,100) 'Rotational relaxation times associated with'
      WRITE(*,100) 'the Brownian relaxation of a vector:'
       WRITE(*,100) '-> Relaxation time (1):',
     *          UNITS*((6.D0*TAU(6)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(*,100) '-> Relaxation time (2):',
     *          UNITS*((6.D0*TAU(7)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(*,100) '-> Relaxation time (3):',
     *          UNITS*((6.D0*TAU(8)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
      WRITE(*,100) 'Rotational relaxation times associated with'
      WRITE(*,100) 'the Brownian relaxation of'
     *              // ' a traceless symmetric tensor:'
       WRITE(*,100) '-> Relaxation time (1):',
     *          UNITS*((6.D0*TAU(1)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(*,100) '-> Relaxation time (2):',
     *          UNITS*((6.D0*TAU(2)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(*,100) '-> Relaxation time (3):',
     *          UNITS*((6.D0*TAU(3)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(*,100) '-> Relaxation time (4):',
     *          UNITS*((6.D0*TAU(4)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(*,100) '-> Relaxation time (5):',
     *          UNITS*((6.D0*TAU(5)/(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(*,100) 'Mean relaxation time:',
     *          UNITS*((6.D0*
     *          5.D0/(1.D0/TAU(1) + 1.D0/TAU(2) + 1.D0/TAU(3)
     *           + 1.D0/TAU(4) + 1.D0/TAU(5))
     *          /(8.D0))**(1.D0/3.D0)),
     *          '[cm]' // '      '
       WRITE(*,100) 'High frequency intrinsic viscosity eta oo:',
     *          (3.D0/10.D0 * MDD )**(1.D0/3.D0)*UNITS,
     *          '[cm]' // '      '
       WRITE(*,100) 'Zero frequency intrinsic viscosity eta 0:',
     *          (3.D0/10.D0 * (MDD+BR) )**(1.D0/3.D0)*UNITS,
     *          '[cm]' // '      '

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE EIGEN3X3(DRR,EIV,EIVINV)
      REAL*8 EIV(3,3),EIVINV(3,3),IPIV(3),DRR(3),DET
      REAL*8 MATA(3,3),VECR(3),WORK(500)
      INTEGER J,K,L,ILIST(3),LWORK,INFO

      LWORK=500

      MATA=EIV

      CALL DSYEV('V','U',3,MATA,3,VECR,WORK,LWORK,INFO)

      DRR=VECR
      EIV=MATA
      EIVINV=EIV

      CALL MAT_INV3(EIVINV,EIV,DET)

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE INTRINSIC_HIGH(AR,MDD,FDD)
      REAL*8 AR(11,11),MDD,FDD
      REAL*8 ATT(6,6),ATD(6,5),ADD(5,5)
      INTEGER I

      ATT=AR(1:6,1:6)
      ATD=AR(1:6,7:11)
      ADD=AR(7:11,7:11)

      FDD=0.D0
      DO I=1,5
      FDD=FDD+ADD(I,I)
      ENDDO
      FDD=FDD/10

      CALL MATREV(ATT,6,'M')

      ADD=ADD-MATMUL(TRANSPOSE(ATD),MATMUL(ATT,ATD))

      MDD=0.D0
      DO I=1,5
      MDD=MDD+ADD(I,I)
      ENDDO
      MDD=MDD/10

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE BROWN_EW(AR,BR)
      IMPLICIT NONE
      REAL*8 AR(11,11),BR
      REAL*8 MRD(3,3,3),MRR(3,3),TRMRR
      REAL*8 ATT(6,6),ATD(6,5),MRDS(3,-2:2)
      REAL*8 H(3,3),HS(-2:2),EDE(3,3,3,3),LRR(3,3,3,3),LRRS(-2:2,-2:2)
      REAL*8 EPS(3,3,3),U(3,3)
      SAVE EPS,U      
      REAL*8 Y2m(-2:2,3,3)
      SAVE Y2m
      LOGICAL :: INIT=.TRUE.
      SAVE INIT
      INTEGER M1,M2,I,J,K,L,P,Q

      IF(INIT) THEN
      INIT=.FALSE.
      Y2m=0.D0

      Y2m(-2, 1,2)= 1.D0/SQRT(2.D0)
      Y2m(-2, 2,1)= Y2m(-2, 1,2)
             
      Y2m(-1, 2,3)=-1.D0/SQRT(2.D0)
      Y2m(-1, 3,2)= Y2m(-1, 2,3)
             
      Y2m( 0, 1,1)=-1.D0/SQRT(6.D0)
      Y2m( 0, 2,2)= Y2m( 0, 1,1)
      Y2m( 0, 3,3)= SQRT(2.D0/3.D0)
             
      Y2m( 1, 1,3)=-1.D0/SQRT(2.D0)
      Y2m( 1, 3,1)= Y2m( 1, 1,3)
             
      Y2m( 2, 1,1)= 1.D0/SQRT(2.D0)
      Y2m( 2, 2,2)=-Y2m( 2, 1,1)
      
      EPS=0.D0
      EPS(1,2,3)= 1.D0
      EPS(2,3,1)= 1.D0
      EPS(3,1,2)= 1.D0      
      EPS(3,2,1)=-1.D0
      EPS(2,1,3)=-1.D0
      EPS(1,3,2)=-1.D0
      
      U=0
      U(1,1)=1.D0
      U(2,2)=1.D0
      U(3,3)=1.D0
            
      ENDIF
      
      ATT=AR(1:6,1:6)
      ATD=AR(1:6,7:11)

      CALL MATREV(ATT,6,'M')

      MRR=ATT(4:6,4:6)
      TRMRR=MRR(1,1)+MRR(2,2)+MRR(3,3)

      MRDS=MATMUL(ATT(4:6,:),ATD)
      
      MRD=0.D0
      DO M1=1,3
      DO M2=-2,2
      MRD(M1,:,:)=MRD(M1,:,:)+MRDS(M1,M2)*Y2m(M2,:,:)
      ENDDO
      ENDDO
      
      H=0.D0
      DO I=1,3
      DO J=1,3
      DO K=1,3
      DO L=1,3
      H(I,J)=H(I,J)+EPS(I,K,L)*MRD(K,L,J)+EPS(J,K,L)*MRD(K,L,I)
      ENDDO
      ENDDO
      ENDDO
      ENDDO

      HS=0.D0      
      DO M1=-2,2
      DO I=1,3
      DO J=1,3
      HS(M1)=HS(M1)+Y2m(M1,I,J)*H(I,J)
      ENDDO
      ENDDO
      ENDDO      
      
      EDE=0.D0
      DO I=1,3
      DO J=1,3
      DO K=1,3
      DO L=1,3
      DO P=1,3
      DO Q=1,3      
      EDE(I,J,K,L)=EDE(I,J,K,L)+EPS(I,K,P)*EPS(J,L,Q)*MRR(P,Q)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ENDDO

      LRR=0      
      DO I=1,3
      DO J=1,3
      DO K=1,3
      DO L=1,3
      LRR(I,J,K,L)=MRR(I,K)*U(J,L)+U(I,K)*MRR(J,L)+2*EDE(I,J,K,L)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      
      DO M1=-2,2
      DO M2=-2,2
      IF(M1==M2) THEN
       LRRS(M1,M2)=2.D0*TRMRR
      ELSE
       LRRS(M1,M2)=0.D0
      ENDIF      
       DO I=1,3
       DO J=1,3
       DO K=1,3
       DO L=1,3
       LRRS(M1,M2)=LRRS(M1,M2)-Y2m(M1,I,J)*LRR(I,J,K,L)*Y2m(M2,K,L)
       ENDDO
       ENDDO
       ENDDO
       ENDDO
      ENDDO
      ENDDO
            
      CALL MATREV(LRRS,5,'M')
      
      BR=SUM(HS*MATMUL(LRRS,HS))/10.D0
      
      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************


      SUBROUTINE HYDRO(AQQ,AR,CONF,RC)
      USE SIZE       ! NN
      USE TENSORS
      USE DIAMETERS
      IMPLICIT NONE
      REAL*8 APP(6*NN,6*NN),APQ(6*NN,5*NN),AQQ(5*NN,5*NN)
      REAL*8 T(11*NN,11),FM(11*NN,11*NN)
      REAL*8 CONF(3,NN),RADII(NN)
      REAL*8 AR(11,11),RC(3)
      INTEGER I,J

      RADII=0.5*DIAM_SPH

      CALL HYDRO_RP(APP,APQ,AQQ,CONF,RADII)

      CALL INVFRI_TO_FRI(APP,APQ,AQQ,NN)

      FM(1:6*NN,1:6*NN)=APP
      FM(1:6*NN,6*NN+1:11*NN)=APQ
      FM(6*NN+1:11*NN,1:6*NN)=TRANSPOSE(APQ)
      FM(6*NN+1:11*NN,6*NN+1:11*NN)=AQQ

      CALL T_RIGID_11(CONF,RC,T,NN)

      AR=MATMUL(TRANSPOSE(T),MATMUL(FM,T))

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE HYDRO_RP(APP,APQ,AQQ,CONF,RADII)
      USE SIZE
      IMPLICIT NONE
      REAL*8 APP(6*NN,6*NN),APQ(6*NN,5*NN),AQQ(5*NN,5*NN)
      REAL*8 CONF(3,NN),RADII(NN)

      CALL ROTNE_PRAGER_YAMAKAWA(APP,APQ,AQQ,CONF,RADII,NN)

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE ROTNE_PRAGER_YAMAKAWA(APP,APQ,AQQ,CONF,RADII,NN)
      IMPLICIT NONE
      INTEGER NN
      REAL*8 APP(6*NN,6*NN),APQ(6*NN,5*NN),AQQ(5*NN,5*NN)
      REAL*8 CONF(3,NN),RADII(NN)
      REAL*8 A1(3,3),B1(5,5),C1(3,5),R(3),DIST
      REAL*8 aaI,aaJ            ! radius
      INTEGER I,J
      LOGICAL :: INIT=.TRUE.
      SAVE INIT
      
      IF(INIT) THEN
       INIT=.FALSE.
       CALL INIT_U
       CALL INIT_EPS
       CALL INIT_Y2
      ENDIF

      APP=0.D0
      APQ=0.D0
      AQQ=0.D0

C PARTICLES LOOP ********************************************

      DO I=1,NN
        aaI=RADII(I)
       DO J=1,3
        APP(6*(I-1)+J,6*(I-1)+J) = 1.D0/(6.D0*aaI)
        APP(6*(I-1)+3+J,6*(I-1)+3+J) = 1.D0/(8.D0*aaI**3)
       ENDDO
       DO J=1,5
        AQQ(5*(I-1)+J,5*(I-1)+J) = 3.D0/(20.D0*aaI**3)
       ENDDO
      ENDDO

      DO I=1,NN-1
      DO J=I+1,NN       
       
       R=CONF(1:3,I)-CONF(1:3,J)
       DIST = SQRT(SUM(R**2))       

       aaI=RADII(I)
       aaJ=RADII(J)
       
       IF(DIST>aaI+aaJ) THEN
      
       CALL ROTNE_PRAGER_TT_IJ(A1,R,aaI,aaJ)
       APP(6*(I-1)+1:6*(I-1)+3,6*(J-1)+1:6*(J-1)+3)=A1
       APP(6*(J-1)+1:6*(J-1)+3,6*(I-1)+1:6*(I-1)+3)=A1
      
       CALL ROTNE_PRAGER_RR_IJ(A1,R)
       APP(6*(I-1)+4:6*(I-1)+6,6*(J-1)+4:6*(J-1)+6)=A1
       APP(6*(J-1)+4:6*(J-1)+6,6*(I-1)+4:6*(I-1)+6)=A1
      
       CALL ROTNE_PRAGER_RT_IJ(A1,R)
       APP(6*(I-1)+4:6*(I-1)+6,6*(J-1)+1:6*(J-1)+3)=A1  ! RT IJ
       APP(6*(J-1)+4:6*(J-1)+6,6*(I-1)+1:6*(I-1)+3)=-A1 ! RT JI
       APP(6*(I-1)+1:6*(I-1)+3,6*(J-1)+4:6*(J-1)+6)=A1  ! TR IJ
       APP(6*(J-1)+1:6*(J-1)+3,6*(I-1)+4:6*(I-1)+6)=-A1 ! TR JI
       
       CALL ROTNE_PRAGER_TD_IJ_Y2(C1,R,aaI,aaJ)
       APQ(6*(I-1)+1:6*(I-1)+3,5*(J-1)+1:5*(J-1)+5)=-C1
       CALL ROTNE_PRAGER_TD_IJ_Y2(C1,-R,aaJ,aaI)
       APQ(6*(J-1)+1:6*(J-1)+3,5*(I-1)+1:5*(I-1)+5)=-C1
       
       CALL ROTNE_PRAGER_RD_IJ_Y2(C1,R,aaI,aaJ)
       APQ(6*(I-1)+4:6*(I-1)+6,5*(J-1)+1:5*(J-1)+5)=-C1
       CALL ROTNE_PRAGER_RD_IJ_Y2(C1,-R,aaJ,aaI)
       APQ(6*(J-1)+4:6*(J-1)+6,5*(I-1)+1:5*(I-1)+5)=-C1

       CALL ROTNE_PRAGER_DD_IJ_Y2(B1,R,aaI,aaJ)
       AQQ(5*(I-1)+1:5*(I-1)+5,5*(J-1)+1:5*(J-1)+5)=B1
       AQQ(5*(J-1)+1:5*(J-1)+5,5*(I-1)+1:5*(I-1)+5)=B1

C***************************************************************    
       ELSE
              
       CALL YAMAKAWA_TT_IJ(A1,R,aaI,aaJ)
       APP(6*(I-1)+1:6*(I-1)+3,6*(J-1)+1:6*(J-1)+3)=A1
       APP(6*(J-1)+1:6*(J-1)+3,6*(I-1)+1:6*(I-1)+3)=A1
       
       CALL YAMAKAWA_RR_IJ(A1,R,aaI,aaJ)
       APP(6*(I-1)+4:6*(I-1)+6,6*(J-1)+4:6*(J-1)+6)=A1
       APP(6*(J-1)+4:6*(J-1)+6,6*(I-1)+4:6*(I-1)+6)=A1
       
       CALL YAMAKAWA_RT_IJ(A1,R,aaI,aaJ)
       APP(6*(I-1)+4:6*(I-1)+6,6*(J-1)+1:6*(J-1)+3)=A1  ! RT IJ
       APP(6*(J-1)+1:6*(J-1)+3,6*(I-1)+4:6*(I-1)+6)=-A1 ! TR JI

       CALL YAMAKAWA_RT_IJ(A1,-R,aaJ,aaI)
       APP(6*(J-1)+4:6*(J-1)+6,6*(I-1)+1:6*(I-1)+3)=A1  ! RT JI
       APP(6*(I-1)+1:6*(I-1)+3,6*(J-1)+4:6*(J-1)+6)=-A1 ! TR IJ

       CALL YAMAKAWA_TD_IJ_Y2(C1,R,aaI,aaJ)
       APQ(6*(I-1)+1:6*(I-1)+3,5*(J-1)+1:5*(J-1)+5)=-C1
       CALL YAMAKAWA_TD_IJ_Y2(C1,-R,aaJ,aaI)
       APQ(6*(J-1)+1:6*(J-1)+3,5*(I-1)+1:5*(I-1)+5)=-C1
       
       CALL YAMAKAWA_RD_IJ_Y2(C1,R,aaI,aaJ)
       APQ(6*(I-1)+4:6*(I-1)+6,5*(J-1)+1:5*(J-1)+5)=-C1
       CALL YAMAKAWA_RD_IJ_Y2(C1,-R,aaJ,aaI)
       APQ(6*(J-1)+4:6*(J-1)+6,5*(I-1)+1:5*(I-1)+5)=-C1

       CALL YAMAKAWA_DD_IJ_Y2(B1,R,aaI,aaJ)
       AQQ(5*(I-1)+1:5*(I-1)+5,5*(J-1)+1:5*(J-1)+5)=B1
       AQQ(5*(J-1)+1:5*(J-1)+5,5*(I-1)+1:5*(I-1)+5)=B1
       
       ENDIF
       
      ENDDO
      ENDDO
      
      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE MAT_INV3(LI,LR,DET_LR)
      IMPLICIT NONE
      REAL*8 LI(3,3),LR(3,3),DET_LR
      
      det_LR=LR(1,1)*LR(2,2)*LR(3,3)+LR(1,2)*LR(2,3)*LR(3,1)+
     *        LR(1,3)*LR(2,1)*LR(3,2)-LR(1,3)*LR(2,2)*LR(3,1)-      
     *        LR(2,3)*LR(3,2)*LR(1,1)-LR(3,3)*LR(1,2)*LR(2,1)

      LI(1,1)=(1.D0/det_LR)*(LR(2,2)*LR(3,3)-LR(2,3)*LR(3,2))
      LI(1,2)=(1.D0/det_LR)*(LR(1,3)*LR(3,2)-LR(1,2)*LR(3,3))
      LI(1,3)=(1.D0/det_LR)*(LR(1,2)*LR(2,3)-LR(1,3)*LR(2,2))

      LI(2,1)=(1.D0/det_LR)*(LR(2,3)*LR(3,1)-LR(2,1)*LR(3,3))
      LI(2,2)=(1.D0/det_LR)*(LR(1,1)*LR(3,3)-LR(1,3)*LR(3,1))
      LI(2,3)=(1.D0/det_LR)*(LR(1,3)*LR(2,1)-LR(1,1)*LR(2,3))

      LI(3,1)=(1.D0/det_LR)*(LR(2,1)*LR(3,2)-LR(2,2)*LR(3,1))
      LI(3,2)=(1.D0/det_LR)*(LR(1,2)*LR(3,1)-LR(1,1)*LR(3,2))
      LI(3,3)=(1.D0/det_LR)*(LR(1,1)*LR(2,2)-LR(1,2)*LR(2,1))
      
      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

C NOT NORMALIZED
      SUBROUTINE ROTNE_PRAGER_TT_IJ(A1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 A1(3,3),R(3)
      REAL*8 DIST,DIST3,RW(3),RR(3,3)
      REAL*8 aaI,aaJ           ! radius
      REAL*8 P18,P23a2
      PARAMETER(P18=1.D0/8.D0)
      P23a2=(aaI**2 + aaJ**2)/3.D0

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST
      DIST3=DIST**3

      CALL CALC_RR(RR,RW)

      A1=P18*( (1.D0/DIST+P23a2/DIST3)*U + (1/DIST-3.D0*P23a2/DIST3)*RR)

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

C NOT NORMALIZED
      SUBROUTINE YAMAKAWA_TT_IJ(A1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 A1(3,3),R(3)
      REAL*8 DIST,DIST2,DIST3,RW(3),RR(3,3)
      REAL*8 aaI,aaJ            ! radius
      REAL*8 M0TT          ! tt single sphere mobility 
      PARAMETER(M0TT=1.D0/6.D0)
      REAL*8 PRE,PU,PRR,aaIaaJ

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST

      CALL CALC_RR(RR,RW)

      IF (aaI.LE.aaJ) THEN
       IF (DIST.LE.(aaJ-aaI)) THEN
        A1=M0TT/aaJ*U
       ELSE
        DIST2=DIST**2
        DIST3=DIST**3
        aaIaaJ=aaI-aaJ
        PRE=1.D0/(6*32*aaI*aaJ)
        PU=16*(aaI+aaJ)-(aaIaaJ**2+3*DIST2)**2/DIST3
        PRR=3*(aaIaaJ**2 - DIST2)**2/DIST3
        A1=PRE*(PU*U+PRR*RR)
       ENDIF
      ELSE
       IF (DIST.LE.(aaI-aaJ)) THEN
        A1=M0TT/aaI*U
       ELSE
        DIST2=DIST**2
        DIST3=DIST**3
        aaIaaJ=aaI-aaJ
        PRE=1.D0/(6*32*aaI*aaJ)
        PU=16*(aaI+aaJ)-(aaIaaJ**2+3*DIST2)**2/DIST3
        PRR=3*(aaIaaJ**2 - DIST2)**2/DIST3
        A1=PRE*(PU*U+PRR*RR)
       ENDIF       
      ENDIF

      RETURN
      END
      
C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE ROTNE_PRAGER_RR_IJ(A1,R)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 A1(3,3),R(3)
      REAL*8 DIST,DIST3,RW(3),RR(3,3)
      REAL*8 P16
      PARAMETER(P16=1.D0/16.D0)

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST
      DIST3=DIST**3

      CALL CALC_RR(RR,RW)

      A1=-P16*(U/DIST3-3.D0*RR/DIST3)

      RETURN
      END
      
C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE YAMAKAWA_RR_IJ(A1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 A1(3,3),R(3)
      REAL*8 DIST,DIST2,DIST3,RW(3),RR(3,3)
      REAL*8 aaI,aaJ
      REAL*8 M0RR
      PARAMETER(M0RR=1.D0/8.D0)
      REAL*8 PRE,PU,PRR,aaIaaJ,aaI2,aaJ2,aaI3,aaJ3

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST

      CALL CALC_RR(RR,RW)

      IF (aaI.LE.aaJ) THEN
       IF (DIST.LE.(aaJ-aaI)) THEN
        A1=M0RR/aaJ**3*U
       ELSE
        DIST2=DIST**2
        DIST3=DIST**3
        aaIaaJ=aaI-aaJ
        aaI2=aaI**2
        aaJ2=aaJ**2
        aaI3=aaI**3
        aaJ3=aaJ**3
        PRE=1.D0/(8*64*aaI3*aaJ3)

        PU=  5*DIST3 - 27*DIST*(aaI2+aaJ2) 
     *     + 32*(aaI3+aaJ3)
     *     - 9*(aaI2-aaJ2)**2/DIST 
     *     - aaIaaJ**4*(aaI2 + 4*aaI*aaJ + aaJ2)/DIST3

        PRR=3*(aaIaaJ**2 - DIST2)**2
     *    *((aaI2 + 4*aaI*aaJ + aaJ2)-DIST2)/DIST3

        A1=PRE*(PU*U+PRR*RR)
       ENDIF
      ELSE
       IF (DIST.LE.(aaI-aaJ)) THEN
        A1=M0RR/aaI**3*U
       ELSE
        DIST2=DIST**2
        DIST3=DIST**3
        aaIaaJ=aaI-aaJ
        aaI2=aaI**2
        aaJ2=aaJ**2
*-----------------------------------------------------------
        aaI3=aaI**3
        aaJ3=aaJ**3
*-----------------------------------------------------------
        PRE=1.D0/(8*64*(aaI**3)*(aaJ**3))

        PU=  5*DIST3 - 27*DIST*(aaI2+aaJ2) 
     *     + 32*(aaI3+aaJ3)
     *     - 9*(aaI2-aaJ2)**2/DIST 
     *     - aaIaaJ**4*(aaI2 + 4*aaI*aaJ + aaJ2)/DIST3

        PRR=3*(aaIaaJ**2 - DIST2)**2
     *    *((aaI2 + 4*aaI*aaJ + aaJ2)-DIST2)/DIST3

        A1=PRE*(PU*U+PRR*RR)
       ENDIF       
      ENDIF

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE ROTNE_PRAGER_RT_IJ(A1,R)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 A1(3,3),R(3)
      REAL*8 DIST,DIST2,RW(3),EPSR(3,3)
      REAL*8 P18
      PARAMETER(P18=1.D0/8.D0)
      
      DIST=SQRT( SUM(R**2) )
      RW=R/DIST
      DIST2=DIST**2
      
      CALL CALC_EPSR(EPSR,RW)

      A1=P18*EPSR/DIST2

      RETURN
      END
      
C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE YAMAKAWA_RT_IJ(A1,R,aaI,aaJ)
      IMPLICIT NONE
      REAL*8 A1(3,3),R(3)
      REAL*8 DIST,RW(3),EPSR(3,3)
      REAL*8 aaI,aaJ            ! radius
      REAL*8 PRE,PEPSR

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST

      CALL CALC_EPSR(EPSR,RW)

      IF (aaI.LE.aaJ) THEN
       IF (DIST.LE.(aaJ-aaI)) THEN
        A1=0.D0
       ELSE
        PRE=1.D0/(16*8*aaI**3*aaJ)
        PEPSR=((aaI-aaJ)+DIST)**2
     *   *(aaJ**2+2*aaJ*(aaI+DIST)-3*(aaI-DIST)**2)
     *   /DIST**2
        A1=PRE*PEPSR*EPSR
       ENDIF
      ELSE
       IF (DIST.LE.(aaI-aaJ)) THEN
        A1=DIST/(8*(aaI**3))*EPSR
       ELSE
        PRE=1.D0/(16*8*aaI**3*aaJ)
        PEPSR=((aaI-aaJ)+DIST)**2
     *   *(aaJ**2+2*aaJ*(aaI+DIST)-3*(aaI-DIST)**2)
     *   /DIST**2
        A1=PRE*PEPSR*EPSR
       ENDIF       
      ENDIF
      
      RETURN
      END
      
C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE ROTNE_PRAGER_DD_IJ(B1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 B1(3,3,3,3),t4D1(3,3,3,3),t4D2(3,3,3,3),t4D0(3,3,3,3)
      REAL*8 DIST,DIST2,DIST5,RW(3),RR(3,3),R(3)
      REAL*8 aaI,aaJ,aaI2,aaJ2

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST
      DIST5=DIST**5
      DIST2=DIST**2
      aaI2=aaI**2
      aaJ2=aaJ**2

      CALL CALC_RR(RR,RW)

      CALL CALC_t4D0(t4D0,RR)
      CALL CALC_t4D1(t4D1,RR)
      CALL CALC_t4D2(t4D2,RR)
      t4D2 = t4D2 - t4D1

      B1=0.D0

      B1=B1 +3.D0
     *      *(6.D0*(aaI2+aaJ2)-5.D0*DIST2)
     *      /(20.D0*DIST5)
     *      *t4D0

      B1=B1 -3.D0
     *      *(8.D0*(aaI2+aaJ2)-5.D0*DIST2)
     *      /(40.D0*DIST5)
     *      *t4D1

      B1=B1 +3.D0
     *      *(aaI2+aaJ2)
     *      /(20.D0*DIST5)
     *      *t4D2

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE ROTNE_PRAGER_DD_IJ_Y2(B1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 B1_CART(3,3,3,3),B1(5,5),R(3),a
      REAL*8 aaI,aaJ 
      INTEGER I,J

      CALL ROTNE_PRAGER_DD_IJ(B1_CART,R,aaI,aaJ)

      DO I=1,5
       DO J=1,5
        CALL mulT2aT4T2b(Y2(I,1:3,1:3),B1_CART,Y2(J,1:3,1:3),a) 
        B1(I,J)=a
       ENDDO
      ENDDO

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE YAMAKAWA_DD_IJ(B1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 B1(3,3,3,3),t4D1(3,3,3,3),t4D2(3,3,3,3),t4D0(3,3,3,3)
      REAL*8 DIST,DIST3,DIST5,RW(3),RR(3,3),R(3)
      REAL*8 aaI,aaJ,aaI2,aaJ2,aaI3,aaJ3

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST
      DIST5=DIST**5
      DIST3=DIST**3
      aaI2=aaI**2
      aaJ2=aaJ**2
      aaI3=aaI**3
      aaJ3=aaJ**3

      CALL CALC_RR(RR,RW)

      CALL CALC_t4D0(t4D0,RR)
      CALL CALC_t4D1(t4D1,RR)
      CALL CALC_t4D2(t4D2,RR)
      t4D2 = t4D2 - t4D1

      B1=0.D0

      IF (aaI.LE.aaJ) THEN
       IF (DIST.LE.(aaJ-aaI)) THEN
        B1= 3.D0/(aaJ**3*20.D0)*(t4D0+t4D1+t4D2)
       ELSE

      B1=B1+3.D0/(1280.D0*aaI3*aaJ3)
     *     *(
     *     +3.D0*(aaI-aaJ)**6*(aaI2+6.D0*aaI*aaJ+aaJ2)/DIST5
     *     -10.D0*(aaI-aaJ)**4*(aaI2+4.D0*aaI*aaJ+aaJ2)/DIST3
     *     +32.D0*(aaI3+aaJ3)
     *     -30.D0*(aaI2+aaJ2)*DIST
     *     +5.D0*DIST3
     *      )*t4D0

      B1=B1+3.D0/(1280.D0*aaI3*aaJ3)
     *     *(
     *     -2.D0*(aaI-aaJ)**6*(aaI2+6.D0*aaI*aaJ+aaJ2)/DIST5
     *     +5.D0*(aaI-aaJ)**4*(aaI2+4.D0*aaI*aaJ+aaJ2)/DIST3
     *     -15.D0*(aaI2-aaJ2)**2/DIST
     *     +32.D0*(aaI3+aaJ3)
     *     -25.D0*(aaI2+aaJ2)*DIST
     *     +5.D0*DIST3
     *     )*t4D1

      B1=B1+3.D0/(2560.D0*aaI3*aaJ3)
     *     *(
     *     +(aaI-aaJ)**6*(aaI2+6.D0*aaI*aaJ+aaJ2)/DIST5
     *     -30.D0*(aaI2-aaJ2)**2/DIST
     *     +64.D0*(aaI3+aaJ3)
     *     -40.D0*(aaI2+aaJ2)*DIST
     *     +5.D0*DIST3
     *     )*t4D2

       ENDIF
      ELSE
       IF (DIST.LE.(aaI-aaJ)) THEN
        B1= 3.D0/(aaI**3*20.D0)*(t4D0+t4D1+t4D2)
       ELSE

      B1=B1+3.D0/(1280.D0*aaI3*aaJ3)
     *     *(
     *     +3.D0*(aaI-aaJ)**6*(aaI2+6.D0*aaI*aaJ+aaJ2)/DIST5
     *     -10.D0*(aaI-aaJ)**4*(aaI2+4.D0*aaI*aaJ+aaJ2)/DIST3
     *     +32.D0*(aaI3+aaJ3)
     *     -30.D0*(aaI2+aaJ2)*DIST
     *     +5.D0*DIST3
     *      )*t4D0

      B1=B1+3.D0/(1280.D0*aaI3*aaJ3)
     *     *(
     *     -2.D0*(aaI-aaJ)**6*(aaI2+6.D0*aaI*aaJ+aaJ2)/DIST5
     *     +5.D0*(aaI-aaJ)**4*(aaI2+4.D0*aaI*aaJ+aaJ2)/DIST3
     *     -15.D0*(aaI2-aaJ2)**2/DIST
     *     +32.D0*(aaI3+aaJ3)
     *     -25.D0*(aaI2+aaJ2)*DIST
     *     +5.D0*DIST3
     *     )*t4D1

      B1=B1+3.D0/(2560.D0*aaI3*aaJ3)
     *     *(
     *     +(aaI-aaJ)**6*(aaI2+6.D0*aaI*aaJ+aaJ2)/DIST5
     *     -30.D0*(aaI2-aaJ2)**2/DIST
     *     +64.D0*(aaI3+aaJ3)
     *     -40.D0*(aaI2+aaJ2)*DIST
     *     +5.D0*DIST3
     *     )*t4D2

       ENDIF       
      ENDIF

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE YAMAKAWA_DD_IJ_Y2(B1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 B1_CART(3,3,3,3),B1(5,5),R(3),a
      REAL*8 aaI,aaJ
      INTEGER I,J

      CALL YAMAKAWA_DD_IJ(B1_CART,R,aaI,aaJ)

      DO I=1,5
       DO J=1,5
        CALL mulT2aT4T2b(Y2(I,1:3,1:3),B1_CART,Y2(J,1:3,1:3),a) 
        B1(I,J)=a
       ENDDO
      ENDDO

      RETURN
      END



C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE ROTNE_PRAGER_TD_IJ(C1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 C1(3,3,3),t3UR(3,3,3),t3RRR(3,3,3)
      REAL*8 DIST,DIST4,RW(3),R(3)
      REAL*8 aaI,aaJ,aaI2,aaJ2

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST
      DIST4=DIST**4
      aaI2=aaI**2
      aaJ2=aaJ**2

      CALL CALC_UR(t3UR,RW)
      CALL CALC_RRR(t3RRR,RW)

      C1=0.D0

      C1= ( - 2.D0*(5.D0*aaI2*aaJ2+3.D0*aaJ2**2)/(5.D0*DIST4)*t3UR
     *      + aaJ2*(5.D0*aaI2+3.D0*aaJ2-3.D0*DIST**2)/DIST4*t3RRR
     *    )/(8.D0*aaJ2)

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE ROTNE_PRAGER_TD_IJ_Y2(C1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 C1_CART(3,3,3),C1(3,5),R(3),V(3)
      REAL*8 aaI,aaJ
      INTEGER I

      CALL ROTNE_PRAGER_TD_IJ(C1_CART,R,aaI,aaJ)

      DO I=1,5
       CALL mulT3T2(C1_CART,Y2(I,1:3,1:3),V)
       C1(1:3,I)=V
      ENDDO

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE YAMAKAWA_TD_IJ(C1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 C1(3,3,3),t3UR(3,3,3),t3RRR(3,3,3)
      REAL*8 DIST,DIST2,DIST4,RW(3),R(3),PRE,PUR,PRRR
      REAL*8 aaI,aaJ

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST
      DIST2=DIST**2
      DIST4=DIST**4

      CALL CALC_UR(t3UR,RW)
      CALL CALC_RRR(t3RRR,RW)

      C1=0.D0

      IF (aaI.LE.aaJ) THEN
       IF (DIST.LE.(aaJ-aaI)) THEN
        C1=-3.D0*DIST/(20.D0*aaJ**3)*t3UR
       ELSE

        PRE = 1.D0/(8.D0*aaI*aaJ**3)

        PUR = (10.D0*DIST2 - 24.D0*aaI*DIST
     *       - 15.D0*(aaJ**2-aaI**2) 
     *       + (aaJ-aaI)**5*(aaI+5.D0*aaJ)/DIST4 )/40.D0

        PRRR = ((aaI-aaJ)**2-DIST2)**2
     *        *((aaI-aaJ)*(aaI+5*aaJ)-DIST2)
     *        /(16.D0*DIST4)

        C1 = PRE*(PUR*t3UR+PRRR*t3RRR)

       ENDIF
      ELSE
       IF (DIST.LE.(aaI-aaJ)) THEN
        C1=0.D0
       ELSE

        PRE = 1.D0/(8.D0*aaI*aaJ**3)

        PUR = (10.D0*DIST2 - 24.D0*aaI*DIST
     *       - 15.D0*(aaJ**2-aaI**2) 
     *       + (aaJ-aaI)**5*(aaI+5.D0*aaJ)/DIST4 )/40.D0

        PRRR = ((aaI-aaJ)**2-DIST2)**2
     *        *((aaI-aaJ)*(aaI+5*aaJ)-DIST2)
     *        /(16.D0*DIST4)

        C1 = PRE*(PUR*t3UR+PRRR*t3RRR)

       ENDIF       
      ENDIF

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE YAMAKAWA_TD_IJ_Y2(C1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 C1_CART(3,3,3),C1(3,5),R(3),V(3)
      REAL*8 aaI,aaJ
      INTEGER I

      CALL YAMAKAWA_TD_IJ(C1_CART,R,aaI,aaJ)

      DO I=1,5
       CALL mulT3T2(C1_CART,Y2(I,1:3,1:3),V)
       C1(1:3,I)=V
      ENDDO

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE ROTNE_PRAGER_RD_IJ(C1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 C1(3,3,3),t3EPSRR(3,3,3)
      REAL*8 DIST,RW(3),R(3)
      REAL*8 aaI,aaJ

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST

      CALL CALC_EPSRR(t3EPSRR,RW)

      C1=0.D0

      C1= - 3.D0/(8.D0*DIST**3)*t3EPSRR

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE ROTNE_PRAGER_RD_IJ_Y2(C1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 C1_CART(3,3,3),C1(3,5),R(3),V(3)
      REAL*8 aaI,aaJ
      INTEGER I

      CALL ROTNE_PRAGER_RD_IJ(C1_CART,R,aaI,aaJ)

      DO I=1,5
       CALL mulT3T2(C1_CART,Y2(I,1:3,1:3),V)
       C1(1:3,I)=V
      ENDDO

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE YAMAKAWA_RD_IJ(C1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 C1(3,3,3),t3EPSRR(3,3,3)
      REAL*8 DIST,DIST2,RW(3),R(3)
      REAL*8 aaI,aaJ

      DIST=SQRT( SUM(R**2) )
      RW=R/DIST
      DIST2=DIST**2

      CALL CALC_EPSRR(t3EPSRR,RW)

      C1=0.D0

      IF (aaI.LE.aaJ) THEN
       IF (DIST.LE.(aaJ-aaI)) THEN
        C1=0.D0
       ELSE

        C1 = -15.D0/(1280.D0*(aaI**3)*(aaJ**3)*DIST**3)
     *       *((aaI-aaJ)**2 - DIST2)**2
     *       *((aaI**2 + 4*aaI*aaJ + aaJ**2)-DIST2)
     *       *t3EPSRR

       ENDIF
      ELSE
       IF (DIST.LE.(aaI-aaJ)) THEN
        C1=0.D0
       ELSE

        C1 = -15.D0/(1280.D0*(aaI**3)*(aaJ**3)*DIST**3)
     *       *((aaI-aaJ)**2 - DIST2)**2
     *       *((aaI**2 + 4*aaI*aaJ + aaJ**2)-DIST2)
     *       *t3EPSRR

       ENDIF       
      ENDIF

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE YAMAKAWA_RD_IJ_Y2(C1,R,aaI,aaJ)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 C1_CART(3,3,3),C1(3,5),R(3),V(3)
      REAL*8 aaI,aaJ
      INTEGER I

      CALL YAMAKAWA_RD_IJ(C1_CART,R,aaI,aaJ)

      DO I=1,5
       CALL mulT3T2(C1_CART,Y2(I,1:3,1:3),V)
       C1(1:3,I)=V
      ENDDO

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************


      SUBROUTINE MOB_TO_FRI(APP,APQ,AQQ,NN)
      IMPLICIT NONE
      INTEGER NN
      REAL*8 APP(6*NN,6*NN),APQ(6*NN,5*NN),AQQ(5*NN,5*NN)
      REAL*8 AQP(5*NN,6*NN)

      CALL MATREV(APP,6*NN,'M')

      AQP = TRANSPOSE(APQ)
      APQ = MATMUL(APP,APQ)
      AQQ = AQQ + MATMUL(AQP,APQ)

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************


      SUBROUTINE FRI_TO_MOB(APP,APQ,AQQ,NN)
      IMPLICIT NONE
      INTEGER NN
      REAL*8 APP(6*NN,6*NN),APQ(6*NN,5*NN),AQQ(5*NN,5*NN)
      REAL*8 AQP(5*NN,6*NN)

      CALL MATREV(APP,6*NN,'M')
      AQP = TRANSPOSE(APQ)
      APQ = MATMUL(APP,APQ)
      AQQ = AQQ - MATMUL(AQP,APQ)

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************


      SUBROUTINE INVFRI_TO_FRI(APP,APQ,AQQ,NN)
      IMPLICIT NONE
      INTEGER NN
      REAL*8 APP(6*NN,6*NN),APQ(6*NN,5*NN),AQQ(5*NN,5*NN)
      REAL*8 AQP(5*NN,6*NN),AP(11*NN,11*NN)

      AP(11*NN,11*NN) = 0.D0

      AP(1:6*NN,1:6*NN) = APP
      AP(1:6*NN,6*NN+1:11*NN) = APQ 
      AP(6*NN+1:11*NN,1:6*NN) = TRANSPOSE(APQ)
      AP(6*NN+1:11*NN,6*NN+1:11*NN) = AQQ 

      CALL MATREV(AP,11*NN,'M')

      APP = AP(1:6*NN,1:6*NN)
      APQ = AP(1:6*NN,6*NN+1:11*NN)
      AQQ = AP(6*NN+1:11*NN,6*NN+1:11*NN)

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE INIT_U()
      USE TENSORS
      IMPLICIT NONE

      U=0.D0

      U(1,1)=1.D0
      U(2,2)=1.D0
      U(3,3)=1.D0

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE INIT_EPS()
      USE TENSORS
      IMPLICIT NONE

      EPS=0.D0

      EPS(1,2,3) = 1.D0
      EPS(1,3,2) = -1.D0
      EPS(2,1,3) = -1.D0
      EPS(2,3,1) = 1.D0
      EPS(3,1,2) = 1.D0
      EPS(3,2,1) = -1.D0

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE INIT_Y2()
      USE TENSORS
      IMPLICIT NONE

      Y2=0.D0

      Y2(1,1,2)=1.D0/SQRT(2.D0)
      Y2(1,2,1)=1.D0/SQRT(2.D0)

      Y2(2,2,3)=-1.D0/SQRT(2.D0)
      Y2(2,3,2)=-1.D0/SQRT(2.D0)

      Y2(3,1,1)=-1.D0/SQRT(6.D0)
      Y2(3,2,2)=-1.D0/SQRT(6.D0)
      Y2(3,3,3)=SQRT(2.D0/3.D0)

      Y2(4,1,3)=-1.D0/SQRT(2.D0)
      Y2(4,3,1)=-1.D0/SQRT(2.D0)

      Y2(5,1,1)=1.D0/SQRT(2.D0)
      Y2(5,2,2)=-1.D0/SQRT(2.D0)

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE INIT_T4I()
      USE TENSORS
      IMPLICIT NONE

      T4I=0.D0

      T4I(1,1,1,1) =  2.D0/3.D0
      T4I(1,1,2,2) = -1.D0/3.D0
      T4I(1,1,3,3) = -1.D0/3.D0

      T4I(1,2,1,2) =  1.D0/2.D0
      T4I(1,2,2,1) =  1.D0/2.D0

      T4I(1,3,1,3) =  1.D0/2.D0
      T4I(1,3,3,1) =  1.D0/2.D0

      T4I(2,1,1,2) =  1.D0/2.D0
      T4I(2,1,2,1) =  1.D0/2.D0

      T4I(2,2,1,1) = -1.D0/3.D0
      T4I(2,2,2,2) =  2.D0/3.D0
      T4I(2,2,3,3) = -1.D0/3.D0

      T4I(2,3,2,3) =  1.D0/2.D0
      T4I(2,3,3,2) =  1.D0/2.D0

      T4I(3,1,1,3) =  1.D0/2.D0
      T4I(3,1,3,1) =  1.D0/2.D0

      T4I(3,2,2,3) =  1.D0/2.D0
      T4I(3,2,3,2) =  1.D0/2.D0

      T4I(3,3,1,1) = -1.D0/3.D0
      T4I(3,3,2,2) = -1.D0/3.D0
      T4I(3,3,3,3) =  2.D0/3.D0

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALC_RR(RR,RW)
      IMPLICIT NONE
      REAL*8 RR(3,3),RW(3)
      INTEGER I,J

      DO I=1,3
       DO J=1,3
        RR(I,J)=RW(I)*RW(J)
       ENDDO
      ENDDO

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALC_EPSR(EPSR,RW)
      IMPLICIT NONE
      REAL*8 EPSR(3,3),RW(3)
      INTEGER I,J

      EPSR=0.D0

      EPSR(1,2)= RW(3)
      EPSR(2,3)= RW(1)
      EPSR(3,1)= RW(2)
      
      EPSR(2,1)=-RW(3)
      EPSR(3,2)=-RW(1)
      EPSR(1,3)=-RW(2)

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALC_UR(t3UR,RW)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 t3UR(3,3,3),RW(3)
      INTEGER I

      t3UR=0.D0

      DO I=1,3
       t3UR(I,I,1:3)= RW
      ENDDO

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALC_RRR(t3RRR,RW)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 t3RRR(3,3,3),RW(3)
      INTEGER I,J,K

      t3RRR=0.D0

      DO I=1,3
       DO J=1,3
        DO K=1,3
         t3RRR(I,J,K)= RW(I)*RW(J)*RW(K)
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALC_EPSRR(t3EPSRR,RW)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 t3EPSRR(3,3,3),EPSR(3,3),RW(3)
      INTEGER I,J,K

      t3EPSRR=0.D0

      CALL CALC_EPSR(EPSR,RW)

      DO I=1,3
       DO J=1,3
        DO K=1,3
         t3EPSRR(I,J,K) = EPSR(I,J)*RW(K)
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END




C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALC_t4D0(t4D0,RR)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 t4D0(3,3,3,3),RR(3,3),TMP(3,3)
      INTEGER I,J

      TMP = RR-U/3.D0

      CALL t2at2b_t4(t4D0,TMP,TMP)
      t4D0 = 3.D0/2.D0 * t4D0

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALC_t4D1(t4D1,RR)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 t4D1(3,3,3,3),RR(3,3),TMP(3,3,3,3)
      INTEGER I,J

      t4D1 = 0.D0

C     DELTAjlRiRk
      CALL t2at2b_t4(TMP,U,RR)
      CALL t4trans(TMP,1,4)
      t4D1=t4D1+TMP
C     DELTAikRjRl
      CALL t2at2b_t4(TMP,U,RR)
      CALL t4trans(TMP,2,3)
      t4D1=t4D1+TMP
C     DELTAjkRiRl
      CALL t2at2b_t4(TMP,U,RR)
      CALL t4trans(TMP,1,3)
      t4D1=t4D1+TMP
C     DELTAilRjRk
      CALL t2at2b_t4(TMP,U,RR)
      CALL t4trans(TMP,2,4)
      t4D1=t4D1+TMP
C     RiRjRkRl
      CALL t2at2b_t4(TMP,RR,RR)
      t4D1=t4D1 -4.D0*TMP

      t4D1 = 0.5D0*t4D1

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALC_t4D2(t4D2,RR)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 t4D2(3,3,3,3),RR(3,3),TMP(3,3,3,3)
      INTEGER I,J

      t4D2=0.D0

C     DELTAikDELTAjl
      CALL t2at2b_t4(TMP,U,U)
      CALL t4trans(TMP,2,3)
      t4D2=t4D2+TMP
C     DELTAjkDELTAil
      CALL t2at2b_t4(TMP,U,U)
      CALL t4trans(TMP,1,3)
      t4D2=t4D2+TMP
C     DELTAijDELTAkl
      CALL t2at2b_t4(TMP,U,U)
      t4D2=t4D2-TMP
C     DELTAklRiRj
      CALL t2at2b_t4(TMP,RR,U)
      t4D2=t4D2+TMP
C     DELTAijRkRl
      CALL t2at2b_t4(TMP,U,RR)
      t4D2=t4D2+TMP
C     RiRjRkRl
      CALL t2at2b_t4(TMP,RR,RR)
      t4D2=t4D2-3.D0*TMP

      t4D2 = 0.5D0 * t4D2

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE t2at2b_t4(t4,t2a,t2b)
      IMPLICIT NONE
      REAL*8 t4(3,3,3,3),t2a(3,3),t2b(3,3)
      INTEGER I,J,K,L

      t4=0.D0

      DO I=1,3
       DO J=1,3
        DO K=1,3
         DO L=1,3
          t4(I,J,K,L)=t2a(I,J)*t2b(K,L)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE t4trans(t4,i1,i2)
      IMPLICIT NONE
      REAL*8 t4(3,3,3,3),t4old(3,3,3,3)
      INTEGER I,J,K,L,i1,i2

      t4old=t4 
     
      IF ((i1.EQ.1).AND.(i2.EQ.2)) THEN
       DO I=1,3
        DO J=1,3
         DO K=1,3
          DO L=1,3
           t4(I,J,K,L)=t4old(J,I,K,L)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE IF ((i1.EQ.1).AND.(i2.EQ.3)) THEN
       DO I=1,3
        DO J=1,3
         DO K=1,3
          DO L=1,3
           t4(I,J,K,L)=t4old(K,J,I,L)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE IF ((i1.EQ.1).AND.(i2.EQ.4)) THEN
       DO I=1,3
        DO J=1,3
         DO K=1,3
          DO L=1,3
           t4(I,J,K,L)=t4old(L,J,K,I)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE IF ((i1.EQ.2).AND.(i2.EQ.3)) THEN
       DO I=1,3
        DO J=1,3
         DO K=1,3
          DO L=1,3
           t4(I,J,K,L)=t4old(I,K,J,L)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE IF ((i1.EQ.2).AND.(i2.EQ.4)) THEN
       DO I=1,3
        DO J=1,3
         DO K=1,3
          DO L=1,3
           t4(I,J,K,L)=t4old(I,L,K,J)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE IF ((i1.EQ.3).AND.(i2.EQ.4)) THEN
       DO I=1,3
        DO J=1,3
         DO K=1,3
          DO L=1,3
           t4(I,J,K,L)=t4old(I,J,L,K)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
 
      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE mulT2aT4T2b(t2a,t4,t2b,a)
      IMPLICIT NONE
      REAL*8 t4(3,3,3,3),t2a(3,3),t2b(3,3),a
      INTEGER I,J,K,L

      a=0.D0

      DO I=1,3
       DO J=1,3
        DO K=1,3
         DO L=1,3
          a = a + t4(I,J,K,L)*t2a(I,J)*t2b(K,L)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE mulT3T2(t3,t2,v)
      IMPLICIT NONE
      REAL*8 t3(3,3,3),t2(3,3),v(3)
      INTEGER I,J

      v=0.D0

      DO I=1,3
       DO J=1,3
          v = v + t3(1:3,I,J)*t2(I,J)
       ENDDO
      ENDDO

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE CALC_t4I13I24(t4II)
      USE TENSORS
      IMPLICIT NONE
      REAL*8 t4II(3,3,3,3)

      t4II=0.D0
      t4II(1,1,1,1)=1.D0
      t4II(1,2,1,2)=0.5D0
      t4II(1,2,2,1)=0.5D0
      t4II(1,3,1,3)=0.5D0
      t4II(1,3,3,1)=0.5D0
      t4II(2,1,1,2)=0.5D0
      t4II(2,1,2,1)=0.5D0
      t4II(2,2,2,2)=1.D0
      t4II(2,3,2,3)=0.5D0
      t4II(2,3,3,2)=0.5D0
      t4II(3,1,1,3)=0.5D0
      t4II(3,1,3,1)=0.5D0
      t4II(3,2,2,3)=0.5D0
      t4II(3,2,3,2)=0.5D0
      t4II(3,3,3,3)=1.D0

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE APQtoAQP(TPQ,TQP)
      USE SIZE
      IMPLICIT NONE
      REAL*8 TPQ(6*NN,5*NN),TQP(5*NN,6*NN)
      INTEGER I,J

      TQP = 0.D0

      DO I=1,6*NN
       DO J=1,5*NN
        TQP(J,I) = -TPQ(I,J)
       ENDDO
      ENDDO

      RETURN
      END
   

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE T_RIGID_11(CONF,RC,T,NN)
      IMPLICIT NONE
      INTEGER NN
      REAL*8 CONF(3,NN),RC(3),T(11*NN,11)
      REAL*8 CNF(3)
      INTEGER I,HA,HS,HS1,B,M
      REAL*8 Y2m(3,3,-2:2)
      SAVE Y2m
      LOGICAL :: INIT=.TRUE.
      SAVE INIT

      HA(I,B,M) = 6*(I-1) + 3*B + M
      HS(I,M)   = 6*NN + 5*(I-1) + 3   + M
      HS1(M)    = 6              + 3   + M
      
      IF(INIT) THEN
      INIT=.FALSE.
      Y2m=0.D0

      Y2m(1,2,-2)= 1.D0/SQRT(2.D0)
      Y2m(2,1,-2)= Y2m(1,2,-2)

      Y2m(2,3,-1)=-1.D0/SQRT(2.D0)
      Y2m(3,2,-1)= Y2m(2,3,-1)

      Y2m(1,1, 0)=-1.D0/SQRT(6.D0)
      Y2m(2,2, 0)= Y2m(1,1, 0)
      Y2m(3,3, 0)= SQRT(2.D0/3.D0)

      Y2m(1,3, 1)=-1.D0/SQRT(2.D0)
      Y2m(3,1, 1)= Y2m(1,3, 1)

      Y2m(1,1, 2)= 1.D0/SQRT(2.D0)
      Y2m(2,2, 2)=-Y2m(1,1, 2)
      ENDIF

      T=0.D0

      DO I=1,NN

       CNF=CONF(:,I)-RC

       DO M=1,3
        T(HA(I,0,M),HA(1,0,M))=1.D0
        T(HA(I,1,M),HA(1,1,M))=1.D0
       ENDDO

       DO M=-2,2
        T(HS(I,M),HS1(M))=1.D0
       ENDDO

       T(HA(I,0,1),HA(1,1,2))= CNF(3)
       T(HA(I,0,2),HA(1,1,1))=-CNF(3)

       T(HA(I,0,2),HA(1,1,3))= CNF(1)
       T(HA(I,0,3),HA(1,1,2))=-CNF(1)

       T(HA(I,0,3),HA(1,1,1))= CNF(2)
       T(HA(I,0,1),HA(1,1,3))=-CNF(2)

       DO M=-2,2
        T(HA(I,0,1):HA(I,0,3),HS1(M))=MATMUL(Y2m(:,:,M),CNF)
       ENDDO

      ENDDO

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE INIT_THREADS()
      USE THREADS
      IMPLICIT NONE
      
      CHARACTER(LEN=20)::CHARCORES
      CALL GETENV("CORE_NUM", CHARCORES)
      IF (CHARCORES.EQ."") THEN
        NCORES=1
      ELSE
        READ (CHARCORES,*) NCORES
      ENDIF

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE MATREV(A,NN,FLAG)
      
      USE THREADS
      IMPLICIT NONE
      INCLUDE "plasmaf.h"
*
*     .. Parameters ..
      INTEGER          CORES, NN,I,J
      CHARACTER*1      FLAG
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION  A( NN, NN ),A2(NN,NN)
      INTEGER           INFO
      DOUBLE PRECISION  XNORM, ANORM, BNORM, RNORM, RESULT, EPS
      DOUBLE PRECISION  DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL          DLARNV, DLAMCH, DLANGE
      EXTERNAL          PLASMA_INIT
      EXTERNAL          PLASMA_DPOTRI, PLASMA_FINALIZE
      EXTERNAL          PLASMA_DEALLOC_HANDLE
      EXTERNAL          DGEMM
*     ..
*     .. Executable Statements ..
*
*     Initialize Plasma
*
      IF(FLAG.EQ.'F'.OR.FLAG.EQ.'f') RETURN

      CORES = NCORES

      A2 = A

      CALL PLASMA_INIT( CORES, INFO )

      CALL PLASMA_DPOTRF(PLASMAUPPER, NN, A, NN, INFO )
      CALL PLASMA_DPOTRI(PLASMAUPPER, NN, A, NN, INFO )

      IF (( INFO .ne. 0 ) .OR. (RESULT > 60.0)) THEN
          WRITE(*,*) "-- Error in MATREV example !"
      ENDIF
*
*     Finalize Plasma
*
      CALL PLASMA_FINALIZE( INFO )

      DO I=2,NN
       DO J=1,I-1
        A(I,J)=A(J,I)
       ENDDO
      ENDDO

      RETURN
      END

C***********************************************************
C***********************************************************
C***********************************************************

      SUBROUTINE HYDROPROGRESS(AQQ,AR,CONF,RC)
      USE SIZE       ! NN
      USE TENSORS
      USE DIAMETERS
      USE THREADS
      IMPLICIT NONE
      REAL*8 APP(6*NN,6*NN),APQ(6*NN,5*NN),AQQ(5*NN,5*NN)
      REAL*8 T(11*NN,11),FM(11*NN,11*NN)
      REAL*8 CONF(3,NN),RADII(NN)
      REAL*8 AR(11,11),RC(3)
      CHARACTER STATUSWORD*29
      INTEGER K
      
      RADII=0.5*DIAM_SPH

      WRITE(UNIT=STATUSWORD,FMT='(A29)') 'CONSTRUCTING MATRICES'
      CALL PROGRESSSTATUS(3,STATUSWORD)

      CALL HYDRO_RP(APP,APQ,AQQ,CONF,RADII)

      WRITE(UNIT=STATUSWORD,FMT='(A29)') 'INVERTING MATRICES - PLASMA'
      K = 5 + MIN(45,100*(100*NN*NN)/(100*NN*NN + NN*NN*NN/NCORES))
      CALL PROGRESSSTATUS(K,STATUSWORD)

      CALL INVFRI_TO_FRI(APP,APQ,AQQ,NN)

      FM(1:6*NN,1:6*NN)=APP
      FM(1:6*NN,6*NN+1:11*NN)=APQ
      FM(6*NN+1:11*NN,1:6*NN)=TRANSPOSE(APQ)
      FM(6*NN+1:11*NN,6*NN+1:11*NN)=AQQ

      CALL T_RIGID_11(CONF,RC,T,NN)

      AR=MATMUL(TRANSPOSE(T),MATMUL(FM,T))

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************


      SUBROUTINE PROGRESSSTATUS(PROC,STATE)
      IMPLICIT NONE
      INTEGER PROC
      CHARACTER*29 :: STATE
      CHARACTER*40 :: BAR = '  0% TASK:                    '

      WRITE(UNIT=BAR(1:3),FMT='(i3)') PROC
      WRITE(UNIT=BAR(12:40),FMT='(a29)') TRIM(STATE)

      WRITE(*,FMT='(A,A40,$)') CHAR(13),BAR

      RETURN
      END


C***********************************************************
C***********************************************************
C***********************************************************


      SUBROUTINE NUMIFZERO(NUMTOTEST)
      IMPLICIT NONE
      REAL*8 NUMTOTEST

      IF (ABS(NUMTOTEST).LE.1.D-12) THEN
       NUMTOTEST = 0.D0
      ENDIF

      RETURN
      END
     
