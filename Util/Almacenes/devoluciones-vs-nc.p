OUTPUT TO d:\tmp\devolucionesvsnc.txt.
PUT UNFORMATTED
    'ALMACEN|NRO INGRESO|NRO DEVOLUCION|FECHA|REF|NUMERO|CLIENTE|'
    'DOC|NUMERO|EMISION|DIVISION|IMPORTE' SKIP.
FOR EACH almcmov NO-LOCK WHERE codcia = 1
    AND tipmov = 'I'
    AND codmov = 09
    AND fchdoc >= 01/01/2018
    AND flgest <> 'A':
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.nrodoc '|'
        almcmov.nrorf2 '|'
        almcmov.fchdoc '|'
        almcmov.codref '|'
        almcmov.nroref '|'
        almcmov.codcli '|'.
    FIND ccbcdocu WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.coddoc = 'n/c'
        AND ccbcdocu.codcli = almcmov.codcli
        AND ccbcdocu.codref = almcmov.codref
        AND ccbcdocu.nroref = almcmov.nroref
        AND ccbcdocu.flgest <> 'A'
        AND ccbcdocu.cndcre = 'D'
        AND CcbCDocu.CodAlm = Almcmov.CodAlm
        AND CcbCDocu.CodMov = Almcmov.CodMov
        AND CcbCDocu.NroPed = STRING(Almcmov.NroDoc)
        AND CcbCDocu.NroOrd = Almcmov.NroRf2
        NO-LOCK NO-ERROR.
    IF AVAILABLE ccbcdocu THEN
        PUT UNFORMATTED
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.coddiv '|'
        ccbcdocu.imptot
        SKIP.
    ELSE PUT UNFORMATTED '|||' SKIP.
END.
OUTPUT CLOSE.

