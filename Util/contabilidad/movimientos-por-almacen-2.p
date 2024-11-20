DEF VAR x-stkact AS DEC FORMAT '->>>,>>>,>>9.99' NO-UNDO.

OUTPUT TO c:\tmp\movimientos-por-almacen-2.txt.
FOR EACH almacen NO-LOCK WHERE codcia = 1 
    AND LOOKUP(codalm, '10,10a,27,501,502') > 0,
    EACH almdmov NO-LOCK WHERE almdmov.codcia = 1
    AND almdmov.codalm = almacen.codalm
    AND almdmov.fchdoc >= 09/05/2012
    AND almdmov.fchdoc <= 12/31/2012:
    DISPLAY
        almdmov.codalm
        almdmov.fchdoc
        almdmov.codmat
        almdmov.tipmov
        almdmov.nroser
        almdmov.nrodoc FORMAT '9999999'
        almdmov.candes
        almdmov.factor
        almdmov.codund
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.
