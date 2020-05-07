        PROGRAM DIRECT_SAMPLING
        
        IMPLICIT NONE
C
C       *** DECLARATION DES VARIABLES ****
C
        INTEGER :: NTRIALS, NRUNS, NHITS, I, J
	REAL :: X,Y,PI
C
C       *** INITIALISATION VARIABLE ***
C
	NRUNS   = 1000
	NTRIALS = 100000
 	NHITS   = 0
	I       = 0
	J       = 0
	X       = 0.0
	Y       = 0.0
	PI      = 0.0
	CALL RANDOM_SEED
C
C       *** GENERER COORDONNEES D UN POINT NTRIAL FOIS***
C
	DO I=1,NRUNS
	DO J=1,NTRIALS
	  CALL RANDOM_NUMBER (X)
	  X=(X-0.5)*2
	  CALL RANDOM_NUMBER (Y)
	  Y=(Y-0.5)*2
	  IF ((X*X+Y*Y)<1) THEN
	    NHITS=NHITS+1
	  END IF 	  
	END DO
	END DO
C
C	*** ESTIMATION DE PI ***
C
	PI = 4.0*NHITS/NTRIALS/NRUNS
C
C	*** AFFICHAGE RESULTAT ***
C
	        PRINT*, 'l estimation de pi est de', PI
C
	END PROGRAM
