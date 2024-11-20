DEF VAR x-anticipo AS DEC NO-UNDO.
DEF VAR x-deposito AS DEC NO-UNDO.
DEF VAR x-distrito AS CHAR NO-UNDO.
DEF VAR x-nc AS DEC NO-UNDO.

OUTPUT TO d:\tmp\cotivsadelantos.txt.
PUT UNFORMATTE
    'COTIZACION|EMISION|ENTREGA|CLIENTE|NOMBRE|IMPORTE|ANTICIPO|DEPOSITO|N/C|CODN VTA|DISTRITO'  SKIP.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '10067'
    AND coddoc = 'COT'
    AND libre_c01 = '10067'
    AND fchped >= DATE(01,01,2019)
    AND flgest <> 'A':
    x-anticipo = 0.
    x-deposito = 0.
    x-nc = 0.
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
        AND LOOKUP(ccbcdocu.coddoc, 'A/R,BD,N/C') > 0
        AND ccbcdocu.flgest = "P"
        AND ccbcdocu.codcli = faccpedi.codcli:
        IF ccbcdocu.coddoc = 'A/R' THEN x-anticipo = x-anticipo + ccbcdocu.sdoact.
        IF ccbcdocu.coddoc = 'BD'  THEN x-deposito = x-deposito + ccbcdocu.sdoact.
        IF ccbcdocu.coddoc = 'N/C'  THEN x-nc = x-nc + ccbcdocu.sdoact.
    END.
    x-distrito = ''.
    FIND gn-clie WHERE gn-clie.codcia = 000 AND gn-clie.codcli = faccpedi.codcli NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN DO:
        FIND TabDistr WHERE TabDistr.CodDepto = gn-clie.CodDept 
            AND TabDistr.CodProvi = gn-clie.CodProv 
            AND TabDistr.CodDistr = gn-clie.CodDist
            NO-LOCK NO-ERROR.
        IF AVAILABL TabDistr THEN x-distrito = TabDistr.NomDistr.
    END.
    PUT UNFORMATTED
        faccpedi.nroped '|'
        faccpedi.fchped '|'
        faccpedi.fchent '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        faccpedi.imptot '|'
        x-anticipo '|'
        x-deposito '|'
        x-nc '|'
        faccpedi.fmapgo '|'
        x-distrito
        SKIP.
END.
INPUT CLOSE.

