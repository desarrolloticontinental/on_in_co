DEF BUFFER b-fac FOR ccbcdocu.
OUTPUT TO d:\tmp\INFO.txt.
FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
    AND ccbcdocu.coddoc = 'N/C'
    AND ccbcdocu.fchdoc >= DATE(01,01,2018)
    AND ccbcdocu.fchdoc <= DATE(12,31,2018)
    AND ccbcdocu.flgest <> 'A',
    FIRST b-fac NO-LOCK WHERE b-fac.codcia = 001
    AND b-fac.coddoc = ccbcdocu.codref 
    AND b-fac.nrodoc = ccbcdocu.nroref:               
    PUT UNFORMATTED
        ccbcdocu.coddiv '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.glosa  '|'
        ccbcdocu.codmon '|'
        ccbcdocu.imptot '|'
        ccbcdocu.codref '|'
        ccbcdocu.nroref '|'
        b-fac.fchdoc '|'
        ccbcdocu.usuario
        SKIP.
END.
OUTPUT CLOSE.

