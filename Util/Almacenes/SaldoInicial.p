DEF VAR x-ano AS INT NO-UNDO.
DEF VAR x-mes AS INT NO-UNDO.

DEF VAR x-fecha AS DATE FORMAT '99/99/9999' NO-UNDO.

DEF VAR k AS INT NO-UNDO.
ASSIGN
    x-ano = 2016
    x-mes = 10.
OUTPUT TO d:\tmp\saldoinicial.txt.
DO  k = 1 TO 13:
    x-fecha = DATE(x-mes,01,x-ano).
    FOR EACH almmmatg NO-LOCK WHERE codcia = 1 AND tpoart <> 'D',
        EACH almmmate OF almmmatg NO-LOCK,
        FIRST almacen OF almmmate NO-LOCK WHERE almacen.coddiv = '00000':
        FIND LAST almstkal WHERE AlmStkal.CodCia = almmmate.codcia
            AND AlmStkal.CodAlm = almmmate.codalm
            AND AlmStkal.codmat = almmmate.codmat
            AND AlmStkal.Fecha <= x-fecha
            NO-LOCK NO-ERROR.
        IF AVAILABLE almstkal AND almstkal.stkact <> 0 THEN DO:
            PUT x-fecha '|'
                almmmate.codalm '|'
                almmmate.codmat '|'
                almstkal.stkact
                SKIP.
        END.
    END.
    x-mes = x-mes - 1.
    IF x-mes <= 0 THEN ASSIGN x-mes = 12 x-ano = x-ano - 1.
END.
OUTPUT CLOSE.
