OUTPUT TO d:\tmp\pers-ubigeos.txt.
FOR EACH pl-pers NO-LOCK WHERE codcia = 001:
    PUT UNFORMATTED 
        codper '|'
        patper '|'
        matper '|'
        nomper '|'
        distri '|'
        provi '|'
        ubigeo '|'.
    FIND TabDepto WHERE TabDepto.CodDepto = SUBSTRING(ubigeo,1,2) NO-LOCK NO-ERROR.
    IF AVAILABLE TabDepto THEN PUT UNFORMATTED TabDepto.NomDepto '|'.
    ELSE '|'.
    FIND TabProvi WHERE TabProvi.CodDepto = SUBSTRING(ubigeo,1,2) AND
        TabProvi.CodProvi = SUBSTRING(ubigeo,3,2) 
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabProvi THEN PUT UNFORMATTED TabProvi.NomProvi '|'.
    ELSE PUT '|'.
    FIND TabDistr WHERE TabDistr.CodDepto = SUBSTRING(ubigeo,1,2) AND
        TabDistr.CodProvi = SUBSTRING(ubigeo,3,2) AND
        TabDistr.CodDistr = SUBSTRING(ubigeo,5,2) 
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabDistr THEN PUT UNFORMATTED TabDistr.NomDistr.
    ELSE PUT '|'.
    PUT SKIP.

END.
