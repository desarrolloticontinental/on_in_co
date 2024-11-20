OUTPUT TO c:\tmp\detallado-devoluciones.txt.
PUT UNFORMATTED
    'ALMACEN|EMISION|SERIE|NUMERO|REF|NROREF|CLIENTE|NOMBRE|MON|PRODUCTO|DESCRIPCION|UNIDAD|CANTIDAD|IMPORTE'
    SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = 001,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = 001
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = 'i'
    AND almcmov.codmov = 09
    AND almcmov.flgest <> 'A'
    AND almcmov.fchdoc >= 01/01/2012,
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.fchdoc '|'
        almcmov.nroser '|'
        almcmov.nrodoc '|'
        almcmov.codref '|'
        almcmov.nrorf1 '|'
        almcmov.codcli '|'
        almcmov.nomref '|'
        almcmov.codmon '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almdmov.codund '|'
        almdmov.candes '|'
        almdmov.implin
        SKIP.
END.
OUTPUT CLOSE.
