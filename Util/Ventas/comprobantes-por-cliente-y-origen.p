/* COMPROBANTES CON REFERENCIAS */
DEF VAR x-codcli AS CHAR NO-UNDO.
DEF VAR x-fchdoc-1 AS DATE NO-UNDO.
DEF VAR x-fchdoc-2 AS DATE NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
ASSIGN
    x-codcli = '10238835963'
    x-fchdoc-1 = 11/01/2014
    x-fchdoc-2 = TODAY.
DEF BUFFER b-cdocu FOR ccbcdocu.
OUTPUT TO c:\tmp\comprobantes.txt.
PUT UNFORMATTED
    'EMISOR|ORIGEN|DOC|NUMERO|TIPO|EMISION|CLIENTE|NOMBRE|MONEDA|IMPORTE|REF COD|REF NRO|REF TIPO'
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE
    ccbcdocu.codcia = s-codcia
    AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,TCK,N/C') > 0
    AND ccbcdocu.codcli = X-codcli
    AND ccbcdocu.fchdoc >= x-fchdoc-1
    AND ccbcdocu.fchdoc <= x-fchdoc-2
    AND ccbcdocu.flgest <> "A":
    PUT UNFORMATTED
        ccbcdocu.coddiv '|'
        ccbcdocu.divori '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.tpofac '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.codcli '|'
        ccbcdocu.nomcli '|'
        ccbcdocu.codmon '|'
        (IF ccbcdocu.coddoc = 'N/C' THEN -1 ELSE 1) * ccbcdocu.imptot '|'
        ccbcdocu.codref '|'
        ccbcdocu.nroref '|'.
    FIND b-cdocu WHERE b-cdocu.codcia = s-codcia
        AND b-cdocu.coddoc = ccbcdocu.codref
        AND b-cdocu.nrodoc = ccbcdocu.nroref
        NO-LOCK NO-ERROR.
    IF AVAILABLE b-cdocu AND LOOKUP(ccbcdocu.codref, 'FAC,BOL,TCK') > 0
        THEN PUT UNFORMATTED
        b-cdocu.tpofac.
    ELSE PUT UNFORMATTED '|'.
    PUT UNFORMATTED SKIP.
END.
OUTPUT CLOSE.
