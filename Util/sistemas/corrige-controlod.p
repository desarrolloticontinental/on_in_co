DEF VAR x-peso AS DEC NO-UNDO.
FOR EACH controlod EXCLUSIVE-LOCK WHERE codcia = 1
    AND coddoc = 'o/d'
    AND nroetq BEGINS 'hpk-000000'
    AND fchdoc >= 04/01/2020:
    x-peso = 0.
    FOR EACH logisdchequeo NO-LOCK WHERE logisdchequeo.codcia = 1
        AND logisdchequeo.etiqueta = controlod.nroetq
        AND logisdchequeo.coddiv = controlod.coddiv,
        FIRST almmmatg OF logisdchequeo NO-LOCK:
        x-peso = x-peso + (logisdchequeo.canped * almmmatg.pesmat).
    END.
    x-peso = ROUND(x-peso,2).
    IF controlod.pesart <> x-peso THEN DO:
        DISPLAY controlod.nroetq x-peso controlod.pesart WITH STREAM-IO NO-BOX.
        PAUSE 0.
        controlod.pesart = x-peso.
    END.
END.
