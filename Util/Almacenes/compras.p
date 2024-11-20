OUTPUT TO c:\tmp\compras.txt.
PUT UNFORMATTED
    "TIP|MOV|ALM|FECHA|PROVEEDOR|NOMRE|SERIE|NUMERO|O/C|INGRESO|G/R|MON|TCAMBIO|"
    "PRODUCTO|DESCRIPCION|UNIDAD|CANTIDAD|PUNITARIO"
    SKIP.
FOR EACH almcmov NO-LOCK WHERE codcia = 1
    AND tipmov = 'i'
    AND codmov = 02
    AND fchdoc >= 11/01/2012
    AND fchdoc <= 02/28/2013
    AND flgest <> 'A',
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.tipmov '|'
        almcmov.codmov '|'
        almcmov.codalm '|'
        almcmov.fchdoc '|'
        almcmov.codpro '|'
        almcmov.nomref '|'
        almcmov.nroser '|'
        almcmov.nrodoc '|'
        almcmov.nrorf1 '|'
        almcmov.nrorf2 '|'
        almcmov.nrorf3 '|'
        almcmov.codmon '|'
        almcmov.tpocmb '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almdmov.codund '|'
        almdmov.candes '|'
        almdmov.preuni
        SKIP.
END.
FOR EACH almcmov NO-LOCK WHERE codcia = 1
    AND tipmov = 's'
    AND codmov = 09
    AND fchdoc >= 11/01/2012
    AND fchdoc <= 02/28/2013
    AND flgest <> 'A',
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.tipmov '|'
        almcmov.codmov '|'
        almcmov.codalm '|'
        almcmov.fchdoc '|'
        almcmov.codpro '|'
        almcmov.nomref '|'
        almcmov.nroser '|'
        almcmov.nrodoc '|'
        almcmov.nrorf1 '|'
        almcmov.nrorf2 '|'
        almcmov.nrorf3 '|'
        almcmov.codmon '|'
        almcmov.tpocmb '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almdmov.codund '|'
        almdmov.candes '|'
        almdmov.preuni
        SKIP.
END.
OUTPUT CLOSE.

