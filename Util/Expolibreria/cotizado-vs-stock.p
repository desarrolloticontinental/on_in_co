DEF TEMP-TABLE detalle
    FIELD codcia LIKE almmmate.codcia
    FIELD codmat LIKE almmmate.codmat
    FIELD desmat LIKE almmmatg.desmat
    FIELD desmar LIKE almmmatg.desmar
    FIELD codfam LIKE almmmatg.codfam
    FIELD codpr1 LIKE gn-prov.codpro
    FIELD nompro LIKE gn-prov.nompro
    FIELD canped LIKE facdpedi.canped
    FIELD stkact LIKE almmmate.stkact
    INDEX llave01 codcia codmat.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00015'
    AND coddoc = 'cot'
    AND fchped >= 11/30/09
    AND flgest <> 'a',
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    FIND detalle OF almmmatg NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        BUFFER-COPY almmmatg TO detalle.
        ASSIGN
            detalle.canped = 0
            detalle.stkact = 0.
        FIND almmmate WHERE almmmate.codcia = facdpedi.codcia
            AND almmmate.codalm = facdpedi.almdes
            AND almmmate.codmat = facdpedi.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE almmmate THEN detalle.stkact = almmmate.stkact.
        FIND gn-prov WHERE gn-prov.codcia = 000
            AND gn-prov.codpro = almmmatg.codpr1
            NO-LOCK NO-ERROR.
        IF AVAILABLE gn-prov THEN detalle.nompro = gn-prov.nompro.
    END.
    ASSIGN
        detalle.canped = detalle.canped + (facdpedi.canped * facdpedi.factor).
END.

OUTPUT TO c:\tmp\cotizado.txt.
FOR EACH detalle:
    DISPLAY detalle WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

