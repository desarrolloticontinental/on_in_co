DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00000' NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'PED' NO-UNDO.

OUTPUT TO d:\tmp\pedidos.txt.
PUT UNFORMATTED
    'DIVISION|ALMACEN|DOC|NUMERO|FECHA|CLIENTE|NOMBRE|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD'
    SKIP.
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddoc = s-coddoc
    AND faccpedi.divdes = s-coddiv
    AND faccpedi.fchped >= 12/01/2016
    AND faccpedi.flgest <> 'A',
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    PUT UNFORMATTED
        faccpedi.coddiv '|'
        faccpedi.codalm '|'
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.fchped '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        facdpedi.codmat '|'
        almmmatg.desmat '|'
        facdpedi.canped '|'
        facdpedi.undvta
        SKIP.
END.
OUTPUT CLOSE.
