        PROGRAM MARKOV_CHAIN
c
        IMPLICIT NONE
C
C       *** DECLARATION DES VARIABLES ****
C
        INTEGER :: NTRIALS, NHITS, I
	REAL :: X,Y,PI,DELTA,DELTAX,DELTAY
C
C       *** INITIALISATION VARIABLE ***
C
	NTRIALS = 2000
 	NHITS   = 0
	DELTA   = 0.01
	DELTAX  = 0
	DELTAY  = 0
	X       = 1.0
	Y       = 1.0
	PI      = 0.0
	CALL RANDOM_SEED
C
C       *** GENERER COORDONNEES D UN POINT NTRIAL FOIS***
C
	DO I=1,NTRIALS
	  CALL RANDOM_NUMBER (DELTAX)
	  DELTAX = (DELTAX-0.5)*2
	  CALL RANDOM_NUMBER (DELTAY)
	  DELTAY = (DELTAY-0.5)*2
	  IF (((X+DELTAX)<1.0).AND.((Y+DELTAY)<1.0)
     $         .AND.((X+DELTAX)>-1.0)
     $         .AND.((Y+DELTAY)>-1.0))THEN
	    X=X+DELTAX
	    Y=Y+DELTAY
	  END IF 	
	  IF ((X*X+Y*Y)<1) THEN
	    NHITS=NHITS+1
	  END IF
	  PRINT* ,'(',X-DELTAX,',',Y-DELTAY,')',
     $             '(',DELTAX,',',DELTAY,')',
     $             '(',X,',',Y,')', NHITS
	END DO
C
C	*** ESTIMATION DE PI ***
C
	PI = 4.0*NHITS/NTRIALS
C
C	*** AFFICHAGE RESULTAT ***
C
	        PRINT*, 'l estimation de pi est de', PI
C
	END PROGRAM
