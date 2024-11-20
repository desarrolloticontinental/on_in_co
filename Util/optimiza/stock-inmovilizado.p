DEF TEMP-TABLE detalle
    FIELD codmat AS CHAR FORMAT 'x(6)'.

INPUT FROM c:\tmp\productos.prn.
REPEAT:
    CREATE detalle.
    IMPORT detalle.
END.
INPUT CLOSE.

DEF VAR x-ultvta-11 AS DATE.
DEF VAR x-ultvta AS DATE.
DEF VAR x-stkact-11 AS DEC.
DEF VAR x-stkact AS DEC.
DEF VAR x-stkhoy AS DEC.

OUTPUT TO c:\tmp\inmovilizado.txt.
FOR EACH detalle NO-LOCK,
    FIRST almmmatg NO-LOCK WHERE almmmatg.codmat = detalle.codmat
    AND almmmatg.codcia = 001:
    x-stkact-11 = 0.
    x-stkact = 0.
    x-stkhoy = 0.
    x-ultvta-11 = ?.
    x-ultvta = ?.
    FIND LAST almdmov USE-INDEX almd03 WHERE almdmov.codcia = 1
        AND almdmov.codalm = '11'
        AND almdmov.tipmov = 's'
        AND almdmov.codmov = 02
        AND almdmov.codmat = detalle.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almdmov 
    THEN DO: 
        x-ultvta-11 = almdmov.fchdoc.
        FIND LAST almstkal WHERE almstkal.codcia = 001
            AND almstkal.codalm = '11'
            AND almstkal.codmat = detalle.codmat
            AND almstkal.fecha <= almdmov.fchdoc
            NO-LOCK NO-ERROR.
        IF AVAILABLE almstkal THEN x-stkact-11 = almstkal.stkact.
    END.
    FIND LAST almdmov USE-INDEX almd02 WHERE almdmov.codcia = 1
        AND almdmov.tipmov = 's'
        AND almdmov.codmov = 02
        AND almdmov.codmat = detalle.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almdmov THEN DO: 
        x-ultvta = almdmov.fchdoc.
        FIND LAST almstkge WHERE almstkge.codcia = 001
            AND almstkge.codmat = detalle.codmat
            AND almstkge.fecha <= almdmov.fchdoc
            NO-LOCK NO-ERROR.
        IF AVAILABLE almstkge THEN x-stkact = almstkge.stkact.
    END.
    FIND almmmate WHERE almmmate.codcia = 001
        AND almmmate.codalm = '11'
        AND almmmate.codmat = detalle.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almmmate THEN x-stkhoy = almmmate.stkact.
    DISPLAY
        detalle.codmat
        almmmatg.desmat
        desmar
        x-ultvta-11
        x-ultvta
        undbas
        x-stkact-11
        x-stkact
        x-stkhoy
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
OUTPUT CLOSE.

