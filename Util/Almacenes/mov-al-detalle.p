/* Mov al detalle */
OUTPUT TO c:\tmp\detallado.txt.
PUT UNFORMATTED
    'ALMACEN|TIPMOV|CODMOV|FECHA|SERIE|NUMERO|ARTICULO|DESCRIPCION|UNIDAD|CANTIDAD|UNITARIO'
    SKIP.
FOR EACH almcmov NO-LOCK WHERE codcia = 1
    AND codmov = 14
    AND fchdoc = TODAY - 1
    AND usuario = 'sistemas',
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.tipmov '|'
        almcmov.codmov '|'
        almcmov.fchdoc '|'
        almcmov.nroser '|'
        almcmov.nrodoc '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almdmov.codund '|'
        almdmov.candes '|'
        almdmov.preuni
        SKIP.
END.
OUTPUT CLOSE.

