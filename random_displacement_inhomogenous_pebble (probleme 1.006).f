        PROGRAM RANDOM_DISPLACEMENT
c
        IMPLICIT NONE
C
C       *** DECLARATION DES VARIABLES ****
C
        INTEGER              :: SITE, NMOVEMENTS, I, RANK
	INTEGER, DIMENSION(9,4) :: SPACE
	REAL, DIMENSION (9)             :: STATVECTOR  
	REAL                 :: RAND1, RAND2,A,CHK1,CHK2,CHK3
C
C       *** INITIALISATION VARIABLE ***
C
	NMOVEMENTS = 1000000
	SITE = 9
	CHK1 = 0.0	
	CHK2 = 0.0
	CHK3 = 0.0
C
	STATVECTOR=(/1.0,2.0,1.0,2.0,4.0,2.0,1.0,2.0,1.0/)
C
	SPACE(1,:)=(/1,1,2,4/)
	SPACE(2,:)=(/1,2,3,5/)
	SPACE(3,:)=(/2,3,6,3/)
	SPACE(4,:)=(/4,1,7,5/)
	SPACE(5,:)=(/2,4,6,8/)
	SPACE(6,:)=(/3,5,6,9/)
	SPACE(7,:)=(/7,7,8,4/)
	SPACE(8,:)=(/7,9,8,5/)
	SPACE(9,:)=(/9,9,8,6/)
C
	CALL RANDOM_SEED
C
C       *** DEPLACEMENT DANS LA GRILLE ***
C
	DO I=1,NMOVEMENTS
	  CALL RANDOM_NUMBER(RAND1)
	  RAND1=(RAND1*4)+1
	  RANK=RAND1
	IF (STATVECTOR(SPACE(SITE,RANK)) > STATVECTOR(SITE)) THEN
	SITE = SPACE(SITE,RANK)
	ELSE IF (STATVECTOR(SPACE(SITE,RANK)) < STATVECTOR(SITE)) THEN
          CALL RANDOM_NUMBER(RAND2)
	   IF (RAND2<(STATVECTOR(SPACE(SITE,RANK))/STATVECTOR(SITE))) THEN
	     SITE = SPACE(SITE,RANK)
	   END IF
	END IF
C
C
	IF (SITE==1) THEN
	CHK1 = CHK1+1	
	ENDIF
	IF (SITE==2) THEN
	CHK2 = CHK2+1	
	ENDIF
	IF (SITE==5) THEN
	CHK3 = CHK3+1	
	ENDIF
C
	END DO
C
	CHK1 = CHK1/NMOVEMENTS
	CHK2 = CHK2/NMOVEMENTS
	CHK3 = CHK3/NMOVEMENTS
C
C	*** AFFICHAGE RESULTAT ***
C
	PRINT*, 4*CHK1+4*CHK2+CHK3
C
	END PROGRAM
