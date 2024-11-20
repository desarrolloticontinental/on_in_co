FOR EACH almdmov NO-LOCK WHERE codcia = 1
    AND fchdoc >= 01/01/10
    AND tipmov = 'i'
    AND (codmov = 02 OR codmov = 06),
    FIRST almcmov OF almdmov NO-LOCK:
    
    FIND FIRST lg-docmp WHERE lg-docmp.codcia = 1
        AND lg-docmp.nrodoc = integer(almcmov.nrorf1)
        AND lg-docmp.codmat = almdmov.codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE lg-docmp 
    THEN DISPLAY
        almcmov.fchdoc
        almcmov.tipmov
        almcmov.codmov
        almcmov.nrodoc
        almcmov.nrorf1
        almcmov.codpro
        almdmov.codmat.
END.
