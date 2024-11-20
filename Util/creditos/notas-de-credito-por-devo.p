/* N/C por devolucion */
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-fecha-1 AS DATE.
DEF VAR s-fecha-2 AS DATE.
DEF VAR s-codcli AS CHAR.

s-fecha-1 = DATE(01,01,2017).
s-fecha-2 = TODAY.
s-codcli = '20109072177'.
OUTPUT TO d:\tmp\notasdecredito.txt.
PUT UNFORMATTED
    'CODIGO|NUMERO|CLIENTE|NOMBRE|EMISION|IMPORTE|ING. ALMACEN' SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = s-codcia AND
    coddoc = 'N/C' AND
    codcli = s-codcli AND
    fchdoc >= s-fecha-1 AND
    fchdoc <= s-fecha-2 AND
    flgest <> 'A' AND
    cndcre = 'D':
    PUT UNFORMATTED
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.codcli '|'
        ccbcdocu.nomcli '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.imptot '|'.
    FIND almcmov WHERE almcmov.codcia = ccbcdocu.codcia AND
        almcmov.codref = ccbcdocu.codref AND
        almcmov.nroref = ccbcdocu.nroref AND
        almcmov.flgest <> 'A'
        NO-LOCK NO-ERROR.
    IF AVAILABLE almcmov THEN DO:
        PUT UNFORMATTED
            almcmov.nrodoc.
    END.
    PUT SKIP.
END.
OUTPUT CLOSE.

