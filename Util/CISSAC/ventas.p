OUTPUT TO c:\tmp\ventas.txt.
PUT UNFORMATTED
    "ALMACEN|TIP|MOV|SERI|NUMERO|FECHA|REF1|NROREF1|REF2|NROREF2|CLIENTE|PRODUCTO|"
    "DESCRIPCION|CANTIDAD|UNIDAD|PREUNITARIO|MONEDA|TPOCMB|PROMUNIT"
    SKIP.
FOR EACH almcmov NO-LOCK WHERE codcia = 001
    AND tipmov = 's'
    AND codmov = 02
    AND fchdoc >= 11/01/2012
    AND fchdoc <= 12/31/2012
    AND flgest <> 'A',
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
    AND ccbcdocu.codref = almcmov.codref
    AND ccbcdocu.nroref = almcmov.nroref
    AND ccbcdocu.flgest <> 'A',
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.tipmov '|'
        almcmov.codmov '|'
        almcmov.nroser '|'
        almcmov.nrodoc '|'
        almcmov.fchdoc '|'
        almcmov.codref '|'
        almcmov.nroref '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        almcmov.codcli '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almdmov.candes '|'
        almdmov.codund '|'
        almdmov.preuni '|'
        almdmov.codmon '|'
        almdmov.tpocmb '|'
        Almdmov.VctoMn1
        SKIP.
END.
OUTPUT CLOSE.

