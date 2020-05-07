        PROGRAM HARD_DISK_DIRECT_SAMPLING
        
        IMPLICIT NONE
C
C       *** DECLARATION DES VARIABLES ****
C 
        INTEGER :: NTRIALS, I, J, K, TEST
	REAL :: SQUAREDIM, RADIUS, RAND, DISTSQ
	REAL, DIMENSION(4,2) :: COORD
C
C       *** INITIALISATION VARIABLE ***
C
	CALL RANDOM_SEED
	NTRIALS = 1000
	I = 0
	J = 0
	K = 0
	TEST = 1
	SQUAREDIM = 10
	RADIUS = 1
	RAND = 0
	DISTSQ = 0
	COORD(:,:) = 0
C
C       *** GENERER COORDONNEES DES 4 DISQUES***
C
	DO I=1,NTRIALS
	TEST=1
	  DO J=1,4	
	    IF (TEST==0) THEN
	      EXIT
            END IF
	    DO K=1,2
	      CALL RANDOM_NUMBER (RAND)
	      RAND=RAND*(SQUAREDIM-2*RADIUS)+RADIUS
	      COORD (J,K)=RAND
	    END DO
	    IF (J>1) THEN
	      DO K=1,J-1
	        DISTSQ= (COORD(K,1)-COORD(J,1))**2
     $                 +(COORD(K,2)-COORD(J,2))**2  
	PRINT*, I,J,K,'La distance entre', J, 'et', K, 'est de ', DISTSQ
	      IF (DISTSQ<(4*RADIUS**2))THEN  
	PRINT*, 'STOP La distance entre', K, 'et', J-1, 'est de ', DISTSQ
	        TEST = 0
	        EXIT
	      END IF    
	      END DO
	    END IF
	  END DO
	END DO
C
C
C
C
C	*** AFFICHAGE RESULTAT ***
C
C
	END PROGRAM
