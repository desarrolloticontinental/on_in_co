OUTPUT TO c:\tmp\ultimos.txt.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1:
    FIND LAST almdmov USE-INDEX Almd07
        WHERE almdmov.codcia = 1
        AND almdmov.codmat = Almmmatg.codmat
        AND (almdmov.codalm <> '11T' AND almdmov.almori <> '11T')
        NO-LOCK NO-ERROR.
    IF AVAILABLE almdmov THEN DO:
        DISPLAY
            almmmatg.codmat
            almmmatg.desmat
            almmmatg.desmar
            almmmatg.undbas
            almdmov.fchdoc
            almdmov.tipmov
            almdmov.codmov
            almdmov.nroser
            almdmov.nrodoc
            almdmov.codalm
            almdmov.almori
            almdmov.candes * almdmov.factor
            WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
    END.
END.
OUTPUT CLOSE.

