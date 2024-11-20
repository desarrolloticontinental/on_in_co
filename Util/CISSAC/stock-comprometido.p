DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-stk11 AS DEC NO-UNDO.
DEF VAR x-stk21 AS DEC NO-UNDO.
DEF VAR x-stk21s AS DEC NO-UNDO.
DEF VAR pComprometido AS DEC NO-UNDO.

DEF STREAM s-salida.

OUTPUT STREAM s-salida TO C:\tmp\stocks.txt.
PUT STREAM s-salida
    UNFORMATTED
    'CODIGO|DESCRIPCION|MARCA|UNIDAD|STK11|STK21|STK21S'
    SKIP.
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia:
    ASSIGN
        x-stk11 = 0
        x-stk21 = 0
        x-stk21s = 0.
    FIND almmmate WHERE almmmate.codcia = s-codcia
        AND almmmate.codalm = '11'
        AND almmmate.codmat = Almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almmmate THEN DO:
        x-stk11 = almmmate.stkact.
        RUN vta2/stock-comprometido (almmmate.codmat, almmmate.codalm, OUTPUT pComprometido).
        x-stk11 = x-stk11 - pComprometido.
    END.
    FIND almmmate WHERE almmmate.codcia = s-codcia
        AND almmmate.codalm = '21'
        AND almmmate.codmat = Almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almmmate THEN DO:
        x-stk21 = almmmate.stkact.
        RUN vta2/stock-comprometido (almmmate.codmat, almmmate.codalm, OUTPUT pComprometido).
        x-stk21 = x-stk21 - pComprometido.
    END.
    FIND almmmate WHERE almmmate.codcia = s-codcia
        AND almmmate.codalm = '21s'
        AND almmmate.codmat = Almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almmmate THEN DO:
        x-stk21s = almmmate.stkact.
        RUN vta2/stock-comprometido (almmmate.codmat, almmmate.codalm, OUTPUT pComprometido).
        x-stk21s = x-stk21s - pComprometido.
    END.
    PUT STREAM s-salida
        UNFORMATTED
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        almmmatg.undstk '|'
        x-stk11 '|'
        x-stk21 '|'
        x-stk21s '|'
        SKIP.
END.
OUTPUT CLOSE.


