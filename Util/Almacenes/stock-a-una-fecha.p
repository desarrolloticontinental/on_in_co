OUTPUT TO c:\tmp\stock-a-una-fecha.txt.
PUT UNFORMATTED
    'ALMACEN|PRODUCTO|DESCRIPCION|MARCA|LINEA|SUBLINEA|UNIDAD|CANTIDAD|UNIT REPOS SOLES'
    SKIP.
FOR EACH almmmatg NO-LOCK WHERE codcia = 001:
    FOR EACH almacen NO-LOCK WHERE almacen.codcia = 001:
        FIND LAST almstkal WHERE almstkal.codcia = 001
            AND almstkal.codalm = almacen.codalm
            AND almstkal.codmat = almmmatg.codmat
            AND almstkal.fecha <= 03/03/2013
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE almstkal OR almstkal.stkact = 0 THEN NEXT.
        PUT UNFORMATTED
            almacen.codalm  '|'
            almmmatg.codmat '|'
            almmmatg.desmat '|'
            almmmatg.desmar '|'
            almmmatg.codfam '|'
            almmmatg.subfam '|'
            almmmatg.undstk '|'
            almstkal.stkact '|'
            almmmatg.ctotot * (IF almmmatg.monvta = 1 THEN 1 ELSE almmmatg.tpocmb)
            SKIP.
    END.
END.

