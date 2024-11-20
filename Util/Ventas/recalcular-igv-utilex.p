DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.

FOR EACH ccbcdocu WHERE codcia = 1
    AND coddiv = '00023'
    AND fchdoc >= 02/01/2011
    AND fchdoc <= 02/28/2011
    AND LOOKUP(coddoc, 'fac,bol,tck') > 0:
    FOR EACH ccbddocu OF ccbcdocu WHERE ccbddocu.aftigv = NO:
        IF ccbddocu.AftIgv 
            THEN ccbddocu.ImpIgv = ccbddocu.ImpLin - 
            ROUND( ccbddocu.ImpLin  / ( 1 + (ccbcdocu.PorIgv / 100) ), 4 ).
    END.
    ASSIGN
        f-Igv = 0
        f-Isc = 0
        Ccbcdocu.ImpDto = 0
        Ccbcdocu.ImpIgv = 0
        Ccbcdocu.ImpIsc = 0
        Ccbcdocu.ImpTot = 0
        Ccbcdocu.ImpExo = 0
        Ccbcdocu.ImpDto2 = 0.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:        
       F-Igv = F-Igv + Ccbddocu.ImpIgv.
       F-Isc = F-Isc + Ccbddocu.ImpIsc.

       Ccbcdocu.ImpTot = Ccbcdocu.ImpTot + Ccbddocu.ImpLin.
       Ccbcdocu.ImpDto2 = Ccbcdocu.ImpDto2 + Ccbddocu.ImpDto2.

       IF NOT Ccbddocu.AftIgv THEN Ccbcdocu.ImpExo = Ccbcdocu.ImpExo + Ccbddocu.ImpLin.
       IF Ccbddocu.AftIgv = YES
       THEN Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + ROUND(Ccbddocu.ImpDto / (1 + Ccbcdocu.PorIgv / 100), 2).
       ELSE Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + Ccbddocu.ImpDto.
    END.
    ASSIGN
        Ccbcdocu.ImpIgv = ROUND(F-IGV,2)
        Ccbcdocu.ImpIsc = ROUND(F-ISC,2).
    /* ****************************************************************** */
    ASSIGN
        Ccbcdocu.ImpTot = Ccbcdocu.ImpTot - Ccbcdocu.ImpDto2
        Ccbcdocu.ImpIgv = Ccbcdocu.ImpIgv - ROUND( Ccbcdocu.ImpDto2 / (1 + Ccbcdocu.PorIgv / 100) * Ccbcdocu.PorIgv / 100, 2).
    ASSIGN
        Ccbcdocu.ImpVta = Ccbcdocu.ImpTot - Ccbcdocu.ImpExo - Ccbcdocu.ImpIgv.
    ASSIGN
        Ccbcdocu.ImpBrt = Ccbcdocu.ImpVta + Ccbcdocu.ImpIsc + Ccbcdocu.ImpDto + Ccbcdocu.ImpExo.
    IF ccbcdocu.flgest = 'C' THEN Ccbcdocu.sdoact = 0.
    ELSE ccbcdocu.sdoact = ccbcdocu.imptot.

END.
