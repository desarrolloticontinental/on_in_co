DEF VAR x-ctouni LIKE almstkge.ctouni NO-UNDO.
DEF BUFFER b-cmov FOR almcmov.
OUTPUT TO d:\tmp\recepcionadas.txt.
PUT UNFORMATTED
    'ALMACEN|FECHA|ORIGEN|NUMERO|ARTICULO|DESCRIPCION|UNIDAD|CANTIDAD|UNITARIO|REFERENCIA'
    SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = 001:
    FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = 1
        AND almcmov.tipmov = 'i'
        AND almcmov.codmov = 03
        AND almcmov.flgest <> 'a'
        AND almcmov.fchdoc > DATE(12,31,2016)
        AND almcmov.codalm = almacen.codalm:
        FIND FIRST b-cmov WHERE b-cmov.codcia = 001
            AND b-cmov.codalm = almcmov.almdes
            AND b-cmov.tipmov = 's'
            AND b-cmov.codmov = 03
            AND b-cmov.nroser = INTEGER(SUBSTRING(almcmov.nrorf1,1,3))
            AND b-cmov.nrodoc = INTEGER(SUBSTRING(almcmov.nrorf1,4))
            AND b-cmov.flgest <> 'a'
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-cmov THEN NEXT.
        IF b-cmov.fchdoc > DATE(12,31,2016) THEN NEXT.
        FOR EACH almdmov OF almcmov NO-LOCK,
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
                almcmov.nrodoc '|'
                almdmov.codmat '|'
                almmmatg.desmat '|'
                almdmov.codund '|'
                almdmov.candes * almdmov.factor '|'
                x-ctouni '|'
                almcmov.nrorf1 FORMAT 'XXX-XXXXXXXXXXXX'
                SKIP.
        END.
    END.
END.

