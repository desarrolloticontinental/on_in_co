DEF VAR x-imptot AS DEC NO-UNDO.

OUTPUT TO c:\tmp\informe.txt.
FOR EACH expasist NO-LOCK WHERE codcia = 1
    AND coddiv = '00015'
    AND fecha >= TODAY,
    FIRST gn-clie WHERE gn-clie.codcia = 0
    AND gn-clie.codcli = expasist.codcli:
    x-imptot = 0.
    FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 1
        AND faccpedi.coddoc = "cot"
        AND faccpedi.coddiv = "00015"
        AND faccpedi.fchped = TODAY
        AND faccpedi.flgest <> "A"
        AND faccpedi.codcli = expasist.codcli:
        X-imptot = x-imptot + faccpedi.imptot.
    END.
    DISPLAY
        expasist.codcli
        gn-clie.nomcli
        expasist.fecha
        expasist.codacti
        expasist.estado[1]
        expasist.estado[5] FORMAT "x(8)"
        expasist.hora
        x-imptot
        WITH STREAM-IO NO-BOX WIDTH 200
        .
END.
OUTPUT CLOSE.

