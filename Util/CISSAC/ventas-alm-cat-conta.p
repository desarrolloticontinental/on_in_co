OUTPUT TO c:\tmp\ventasalmcissac.txt.
PUT UNFORMATTED
    'ALM|TIP|MOV|NUMERO|FECHA|CLIENTE|NOMBRE|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD|CATCONTA|IMPORTE SOLES'
    SKIP.
FOR EACH almcmov NO-LOCK WHERE codcia = 001
    AND flgest <> 'a'
    AND tipmov = 's'
    AND codmov = 02
    AND fchdoc >= 01/01/2012
    AND fchdoc <= 12/31/2012,
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.tipmov '|'
        almcmov.codmov '|'
        almcmov.nrodoc '|'
        almcmov.fchdoc '|'
        almcmov.codcli '|'
        almcmov.nomref '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almdmov.candes '|'
        almdmov.codund '|'
        almmmatg.catconta[1] '|'
        almdmov.candes * almdmov.factor * Almdmov.VctoMn1 '|'
        SKIP.
END.
FOR EACH almcmov NO-LOCK WHERE codcia = 001
    AND flgest <> 'a'
    AND tipmov = 'i'
    AND (codmov = 09 OR codmov = 56)
    AND fchdoc >= 01/01/2012
    AND fchdoc <= 12/31/2012,
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.tipmov '|'
        almcmov.codmov '|'
        almcmov.nrodoc '|'
        almcmov.fchdoc '|'
        almcmov.codcli '|'
        almcmov.nomref '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almdmov.candes '|'
        almdmov.codund '|'
        almmmatg.catconta[1] '|'
        almdmov.candes * almdmov.factor * Almdmov.VctoMn1 '|'
        SKIP.
END.
OUTPUT CLOSE.
