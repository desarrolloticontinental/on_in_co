
DEF VAR x-stkact AS DEC NO-UNDO.
DEF VAR x-ctouni AS DEC NO-UNDO.

OUTPUT TO c:\tmp\debaja.txt.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1
    AND fching < 01/01/09:
    x-stkact = 0.
    x-ctouni = 0.
    FOR EACH almmmate OF almmmatg NO-LOCK:
        x-stkact = x-stkact + almmmate.stkact.
    END.
    IF x-stkact = 0 THEN NEXT.
    FIND LAST almdmov USE-INDEX almd02 WHERE almdmov.codcia = almmmatg.codcia
        AND almdmov.codmat = almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almdmov AND fchdoc >= 01/01/06 THEN NEXT.
    FIND LAST almstkge WHERE almstkge.codcia = almmmatg.codcia
        AND almstkge.codmat = almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almstkge THEN x-ctouni = AlmStkge.CtoUni.
    FOR EACH almmmate OF almmmatg NO-LOCK WHERE almmmate.stkact > 0:
        DISPLAY
            almmmatg.codmat
            '|'
            almmmatg.desmat
            '|'
            almmmate.codalm
            '|'
            almmmate.stkact
            '|'
            almmmatg.undbas
            '|'
            almmmate.codubi
            '|'
            x-ctouni
            WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
        PAUSE 0.
    END.

END.
OUTPUT CLOSE.

