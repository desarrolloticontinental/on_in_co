OUTPUT TO d:\revisar.txt.
PUT UNFORMATTED 'ALMACEN;ARTICULO;STOCK KARDEX;STOCK MOVIMIENTO' SKIP.
FOR EACH almacen NO-LOCK WHERE codcia = 1,
    EACH almmmate NO-LOCK WHERE almmmate.codcia = 1
    AND almmmate.codalm = almacen.codalm:
    FIND LAST almdmov USE-INDEX almd03 WHERE almdmov.codcia = 1
        AND almdmov.codalm = almmmate.codalm
        AND almdmov.codmat = almmmate.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almdmov AND almmmate.stkact <> almdmov.stksub
        THEN
        PUT UNFORMATTED
        almmmate.codalm ';'
        almmmate.codmat ';' 
        almmmate.stkact ';' 
        almdmov.stksub SKIP.
END.
OUTPUT CLOSE.

