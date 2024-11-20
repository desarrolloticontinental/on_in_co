DEF VAR x-nrocard AS CHAR NO-UNDO.

OUTPUT TO c:\tmp\contipuntos.txt.
    PUT UNFORMATTED
        'DIVISION|FECHA|COD|NUMERO|CLIENTE|NOMBRE|TARJETA|PRODUCTO|DESCRIPCION|CANTIDAD|IMPORTE|MONEDA|ESTADO'
        SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'fac,bol,tck') > 0
    AND fchdoc >= 11/01/2012
    AND fchdoc <= 03/31/2013
    AND flgest <> "A"
    AND LOOKUP(CcbCdocu.TpoFac, 'A,S') = 0,
    EACH ccbddocu OF ccbcdocu NO-LOCK,
    FIRST almmmatg OF ccbddocu NO-LOCK,
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 000
    AND gn-clie.codcli = ccbcdocu.codcli:
    x-nrocard = ccbcdocu.nrocard.
    IF x-nrocard = "" THEN x-nrocard = gn-clie.NroCard.
    IF x-nrocard = "" THEN NEXT.
    PUT UNFORMATTED
        ccbcdocu.divori '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.codcli '|'
        ccbcdocu.nomcli '|'
        x-nrocard '|'
        ccbddocu.codmat '|'
        almmmatg.desmat '|'
        ccbddocu.candes '|'
        ccbddocu.implin '|'
        ccbcdocu.codmon '|'
        ccbcdocu.flgest
        SKIP.

END.
OUTPUT CLOSE.

