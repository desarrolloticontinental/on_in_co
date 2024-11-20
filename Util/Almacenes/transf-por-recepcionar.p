/* Transferencias pendientes de recepcionar */
DEF VAR s-codcia AS INT INIT 001.

OUTPUT TO c:\tmp\transfxrecepcionar.txt.
PUT UNFORMATTED
    'Alm Origen|Alm Destino|Fecha|Articulo|Descripcion|Linea|Sublinea|Cantidad'
    SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 03
    AND almcmov.flgest <> 'A'
    AND almcmov.flgsit = "T",
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.almdes '|'
        almcmov.fchdoc '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        almdmov.candes
        SKIP.
END.
OUTPUT CLOSE.
