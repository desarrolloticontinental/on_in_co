DEF TEMP-TABLE detalle
    FIELD codmat LIKE almmmatg.codmat
    FIELD codalm LIKE almacen.codalm
    FIELD desmat LIKE almmmatg.desmat
    FIELD undbas LIKE almmmatg.undbas
    FIELD candes AS DEC FORMAT '(>>>,>>>,>>9.99)'
    FIELD implin AS DEC FORMAT '(>>>,>>>,>>9.99)'.
DEF VAR x-almdes AS CHAR NO-UNDO.
FOR EACH ccbddocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'g/r'
    AND fchdoc >= 06/01/09
    AND fchdoc <= 06/30/10
    AND implin > 0,
    FIRST almmmatg OF ccbddocu NO-LOCK,
    FIRST ccbcdocu OF ccbddocu NO-LOCK:
    x-almdes = ccbddocu.almdes.
    IF x-almdes = '' THEN x-almdes = ccbcdocu.codalm.
    FIND detalle WHERE detalle.codmat = ccbddocu.codmat 
        AND detalle.codalm = x-almdes
        NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codmat = almmmatg.codmat
            detalle.codalm = x-almdes
            detalle.desmat = almmmatg.desmat
            detalle.undbas = almmmatg.undbas.
    END.
    ASSIGN
        detalle.candes = detalle.candes + ccbddocu.candes * ccbddocu.factor.
    IF ccbcdocu.codmon = 1 THEN detalle.implin = detalle.implin + ccbddocu.implin.
    ELSE detalle.implin = detalle.implin + ccbddocu.implin * ccbcdocu.tpocmb.
END.


OUTPUT TO c:\tmp\resumen.txt.
FOR EACH detalle:
    DISPLAY
        detalle WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

