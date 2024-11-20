DEF VAR x-ctouni LIKE almstkge.ctouni NO-UNDO.
OUTPUT TO d:\tmp\norecepcionadas.txt.
PUT UNFORMATTED
    'ALMACEN|FECHA|DESTINO|SERIE|NUMERO|ARTICULO|DESCRIPCION|UNIDAD|CANTIDAD|UNITARIO'
    SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = 001:
    FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = 1
        AND almcmov.tipmov = 's'
        AND almcmov.codmov = 03
        AND almcmov.flgest <> 'a'
        AND almcmov.flgsit = 'T'
        AND almcmov.fchdoc <= DATE(12,31,2016)
        AND almcmov.codalm = almacen.codalm,
        EACH almdmov OF almcmov NO-LOCK,
        FIRST almmmatg OF almdmov NO-LOCK:
        x-ctouni = 0.
        FIND LAST almstkge WHERE AlmStkge.CodCia = 001
            AND AlmStkge.codmat = almdmov.codmat
            AND AlmStkge.Fecha <= DATE(12,31,2016)
            NO-LOCK NO-ERROR.
        IF AVAILABLE almstkge THEN x-ctouni = AlmStkge.CtoUni.
        PUT UNFORMATTED
            almcmov.codalm '|'
            almcmov.fchdoc '|'
            almcmov.almdes '|'
            almcmov.nroser '|'
            almcmov.nrodoc '|'
            almdmov.codmat '|'
            almmmatg.desmat '|'
            almdmov.codund '|'
            almdmov.candes * almdmov.factor '|'
            x-ctouni
            SKIP.
    END.
END.

