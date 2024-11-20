DEF VAR x-stkact AS DEC FORMAT '->>>,>>>,>>9.99' NO-UNDO.

OUTPUT TO c:\tmp\stock-por-producto-y-por-almacen.txt.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1,
    EACH almacen NO-LOCK WHERE almacen.codcia = 1:
    x-stkact = 0.
    FIND LAST almstkal WHERE almstkal.codcia = 1
        AND almstkal.codmat = almmmatg.codmat
        AND almstkal.codalm = almacen.codalm
        AND almstkal.fecha <= 12/31/2012
        NO-LOCK NO-ERROR.
    IF AVAILABLE almstkal THEN x-stkact = almstkal.stkact.
    IF x-stkact <> 0 THEN
        DISPLAY
        almmmatg.codmat
        almmmatg.desmat
        almacen.codalm
        x-stkact
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

