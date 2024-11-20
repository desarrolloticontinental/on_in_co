DEF TEMP-TABLE DETA LIKE FacDPedi
    FIELD Clave AS CHAR
    INDEX Llave01 Clave NroItm.
DEF TEMP-TABLE DDOCU LIKE Ccbddocu.
DEF TEMP-TABLE T-PROM LIKE Expcprom.
DEF VAR s-codcia AS INT INIT 001.
DEF TEMP-TABLE detalle LIKE facdpedi
    INDEX llave01 codmat.

FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddoc = 'cot'
    AND coddiv = '00015'
    AND fchped >= 01/06/09
    AND fchped <= 01/09/09
    AND flgest <> 'a':
    FOR EACH ddocu:
        DELETE ddocu.
    END.
    FOR EACH deta:
        DELETE deta.
    END.
    FOR EACH t-prom:
        DELETE t-prom.
    END.
    {vtaexp/carga-promociones.i}
    FOR EACH deta:
        FIND detalle WHERE detalle.codmat = deta.codmat
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            detalle.codcia = deta.codcia.
            detalle.codmat = deta.codmat.
            detalle.undvta = deta.undvta.
        END.
        detalle.canped = detalle.canped + deta.canped.
    END.
END.

OUTPUT TO c:\tmp\promociones.txt.
FOR EACH detalle, FIRST almmmatg OF detalle NO-LOCK:
    DISPLAY
        detalle.codmat
        almmmatg.desmat
        almmmatg.desmar
        detalle.undvta
        almmmatg.codfam
        almmmatg.subfam
        detalle.canped
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
OUTPUT CLOSE.
