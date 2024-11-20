DEF VAR x-fecha AS DATE FORMAT '99/99/9999' NO-UNDO.

DEF VAR k AS INT NO-UNDO.

OUTPUT TO d:\tmp\martin.txt.
PUT UNFORMATTED
    'ALMACEN|CODIGO|STOCK|UNITARIO|UNIDAD' SKIP.

    x-fecha = DATE(07,31,2016).
    FOR EACH almmmatg NO-LOCK WHERE codcia = 1,
        EACH almmmate OF almmmatg NO-LOCK:
        FIND LAST almstkal WHERE AlmStkal.CodCia = almmmate.codcia
            AND AlmStkal.CodAlm = almmmate.codalm
            AND AlmStkal.codmat = almmmate.codmat
            AND AlmStkal.Fecha <= x-fecha
            NO-LOCK NO-ERROR.
        IF AVAILABLE almstkal AND almstkal.stkact <> 0 THEN DO:
            PUT UNFORMATTED almmmate.codalm '|'
                almmmate.codmat '|'
                almstkal.stkact '|'
                AlmStkal.CtoUni '|'
                almmmatg.undbas
                SKIP.
        END.
    END.
OUTPUT CLOSE.
