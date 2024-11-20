DEF VAR y-codalm AS CHAR NO-UNDO.
DEF VAR y-fecha  AS CHAR NO-UNDO.
DEF VAR x-archivo AS CHAR NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR x-codalm AS CHAR NO-UNDO.
DEF VAR x-fecha  AS DATE FORMAT '99/99/9999' NO-UNDO.
DEF VAR x-stkact AS DEC NO-UNDO.

ASSIGN
    y-codalm = '03,04,83b,05,17,35,35b,11,22,85,21,21s,10,10a,27,501,502'
    y-fecha  = '26/10/2012,29/10/2012,29/10/2012,27/10/2012,03/11/2012,~
27/10/2012,27/10/2012,10/11/2012,10/11/2012,10/11/2012,17/11/2012,17/11/2012,~
08/09/2012,22/12/2012,15/09/2012,13/10/2012,21/12/2012'.

DO k = 1 TO NUM-ENTRIES(y-codalm):
    ASSIGN
        x-codalm = ENTRY(k, y-codalm)
        x-fecha = DATE(ENTRY(k, y-fecha)).
    DISPLAY x-codalm x-fecha.
    PAUSE 0.
    x-archivo = 'c:\tmp\alm' + TRIM(x-codalm) + '.txt'.
    OUTPUT TO VALUE(x-archivo).
    FOR EACH almmmate NO-LOCK WHERE codcia = 001
        AND codalm = x-codalm:
        x-stkact = 0.
        FIND LAST almstkal WHERE almstkal.codcia = 001
            AND almstkal.codalm = x-codalm
            AND almstkal.codmat = almmmate.codmat
            AND almstkal.fecha <= x-fecha
            NO-LOCK NO-ERROR.
        IF AVAILABLE almstkal THEN x-stkact = AlmStkal.StkAct.
        DISPLAY
            x-fecha
            almmmate.codalm
            almmmate.codmat
            x-stkact
            WITH STREAM-IO NO-BOX WIDTH 320.
    END.
    OUTPUT CLOSE.
END.
