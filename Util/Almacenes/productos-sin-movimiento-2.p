DEF VAR x-stkact AS DEC.
DEF VAR x-ctoprom AS DEC.
    
OUTPUT TO d:\tmp\sin-movimiento.txt.        
FOR EACH almmmatg NO-LOCK WHERE codcia = 1:
    x-stkact = 0.
    FOR EACH almmmate OF almmmatg NO-LOCK, 
        FIRST Almacen OF Almmmate WHERE Almacen.campo-c[6] = 'Si':
        x-stkact = x-stkact + stkact.
    END.
    IF x-stkact <= 0 THEN NEXT.
    FIND FIRST almdmov USE-INDEX almd07 
        WHERE almdmov.codcia = 1
        AND almdmov.codmat = almmmatg.codmat
        AND almdmov.fchdoc >= 04/01/2015
        AND almdmov.fchdoc <= TODAY
        AND almdmov.tipmov = 'S'
        AND almdmov.codmov = 02
        NO-LOCK NO-ERROR.
    IF AVAILABLE almdmov THEN NEXT.
    x-ctoprom = 0.
    FIND LAST almstkge WHERE almstkge.codcia = 1
        AND almstkge.codmat = almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almstkge THEN x-ctoprom = AlmStkge.CtoUni.
    FOR EACH almmmate OF almmmatg NO-LOCK WHERE almmmate.stkact <> 0,
        FIRST Almacen OF Almmmate WHERE Almacen.campo-c[6] = 'Si':
        DISPLAY 
            almmmate.codalm
            almmmatg.codmat 
            almmmatg.desmat FORMAT 'x(60)'
            almmmatg.codfam
            almmmatg.subfam
            almmmatg.undstk
            almmmate.stkact LABEL 'Stock'
            x-ctoprom   FORMAT '(>>>,>>>,>>9.9999)' LABEL 'Cto Promedio'
            WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320.
    END.
END.
OUTPUT CLOSE.
