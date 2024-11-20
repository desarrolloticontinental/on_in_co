DEF TEMP-TABLE detalle
    FIELD codper LIKE pl-pers.codper
    FIELD nombre AS CHAR FORMAT 'x(60)'
    FIELD val125 AS DEC EXTENT 6 FORMAT '>>,>>9.99'
    FIELD val126 AS DEC EXTENT 6 FORMAT '>>,>>9.99'
    FIELD val127 AS DEC EXTENT 6 FORMAT '>>,>>9.99'
    FIELD val616 AS DEC FORMAT '>>,>>9.99'
    FIELD val617 AS DEC FORMAT '>>,>>9.99'.

FOR EACH PL-mov-mes NO-LOCK WHERE codcia = 1
    AND periodo = 2010
    AND nromes >= 01
    AND nromes <= 06
    AND PL-MOV-MES.codcal = 001
    AND (CodMov = 125 OR codmov = 126 OR codmov = 127),
    FIRST pl-pers OF pl-mov-mes NO-LOCK:
    FIND detalle WHERE detalle.codper = pl-pers.codper NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        detalle.codper = pl-pers.codper.
        detalle.nombre = TRIM(patper) + ' ' + TRIM(matper) + ', ' + nomper.
    END.
    CASE codmov:
    WHEN 125 THEN detalle.val125[nromes]= valcal-mes.
    WHEN 126 THEN detalle.val126[nromes]= valcal-mes.
    WHEN 127 THEN detalle.val127[nromes]= valcal-mes.
    END CASE.
END.
FOR EACH PL-mov-mes NO-LOCK WHERE codcia = 1
    AND periodo = 2010
    AND nromes = 07
    AND PL-MOV-MES.codcal = 004
    AND codmov = 616,
    FIRST pl-pers OF pl-mov-mes NO-LOCK:
    FIND detalle WHERE detalle.codper = pl-pers.codper NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        detalle.codper = pl-pers.codper.
        detalle.nombre = TRIM(patper) + ' ' + TRIM(matper) + ', ' + nomper.
    END.
    detalle.val616 = valcal-mes.
END.
FOR EACH PL-mov-mes NO-LOCK WHERE codcia = 1
    AND periodo = 2010
    AND nromes = 07
    AND PL-MOV-MES.codcal = 004
    AND codmov = 617,
    FIRST pl-pers OF pl-mov-mes NO-LOCK:
    FIND detalle WHERE detalle.codper = pl-pers.codper NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        detalle.codper = pl-pers.codper.
        detalle.nombre = TRIM(patper) + ' ' + TRIM(matper) + ', ' + nomper.
    END.
    detalle.val617 = valcal-mes.
END.
OUTPUT TO c:\tmp\grati.txt.
FOR EACH detalle:
    PUT
        detalle.codper '|'
        detalle.nombre '|'
        detalle.val125[1] '|'
        detalle.val126[1] '|'
        detalle.val127[1] '|'
        detalle.val125[2] '|'
        detalle.val126[2] '|'
        detalle.val127[2] '|'
        detalle.val125[3] '|'
        detalle.val126[3] '|'
        detalle.val127[3] '|'
        detalle.val125[4] '|'
        detalle.val126[4] '|'
        detalle.val127[4] '|'
        detalle.val125[5] '|'
        detalle.val126[5] '|'
        detalle.val127[5] '|'
        detalle.val125[6] '|'
        detalle.val126[6] '|'
        detalle.val127[6] '|'
        detalle.val616 '|'
        detalle.val617 '|'
        SKIP
        .

END.
OUTPUT CLOSE.

