DEF VAR s-CodCia AS INT INIT 001.

DEF VAR x-Propios AS DEC NO-UNDO.
DEF VAR x-Terceros AS DEC NO-UNDO.

DEF TEMP-TABLE detalle
    FIELD nrocard AS CHAR
    FIELD coddiv AS CHAR
    FIELD propios AS DEC EXTENT 2
    FIELD terceros AS DEC EXTENT 2
    FIELD credito AS DEC EXTENT 2
    INDEX llave01 AS PRIMARY nrocard coddiv.

DEF BUFFER B-CDOCU FOR Ccbcdocu.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = s-codcia
    AND LOOKUP(coddoc, 'fac,bol,tck') > 0
    AND fchdoc >= 12/15/09
    AND fchdoc <= 03/20/10
    AND flgest <> 'A'
    AND nrocard <> ''
    AND puntos > 0,
    FIRST GN-CARD NO-LOCK WHERE gn-card.NroCard = ccbcdocu.nrocard:
    FIND detalle WHERE detalle.nrocard = ccbcdocu.nrocard
        AND detalle.coddiv = ccbcdocu.coddiv
        NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.nrocard = ccbcdocu.nrocard
            detalle.coddiv = ccbcdocu.coddiv.
    END.
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK WHERE ccbddocu.implin > 0,
        FIRST almmmatg OF ccbddocu NO-LOCK:
        IF ccbcdocu.codmon = 1 THEN DO:
            IF CHR__02 = 'P' 
            THEN detalle.Propios[1] = detalle.Propios[1] + ccbddocu.implin.
            ELSE detalle.Terceros[1] = detalle.Terceros[1] + ccbddocu.implin.
        END.
        ELSE DO:
            IF CHR__02 = 'P' 
            THEN detalle.Propios[2] = detalle.Propios[2] + ccbddocu.implin.
            ELSE detalle.Terceros[2] = detalle.Terceros[2] + ccbddocu.implin.
        END.
    END.
    /* buscamos la nota de credito por devolucion */
    FOR EACH B-CDOCU WHERE B-CDOCU.codcia = s-codcia
            AND B-CDOCU.coddoc = 'N/C'
            AND B-CDOCU.codref = Ccbcdocu.coddoc
            AND B-CDOCU.nroref = Ccbcdocu.nrodoc
            AND B-CDOCU.flgest <> 'A'
            NO-LOCK:
        IF B-CDOCU.codmon = 1 
        THEN DETALLE.Credito[1] = detalle.Credito[1] + B-CDOCU.ImpTot.
        ELSE DETALLE.Credito[2] = detalle.Credito[2] + B-CDOCU.ImpTot.
    END.        
END.

OUTPUT TO c:\tmp\resumen-propios-terceros.txt.
FOR EACH detalle:
    DISPLAY
        detalle.nrocard     COLUMN-LABEL 'Tarjeta'
        detalle.coddiv      COLUMN-LABEL 'Division'
        detalle.propios[1]  COLUMN-LABEL 'Prod. Propios S/.'
        detalle.propios[2]  COLUMN-LABEL 'Prod. Propios US$'
        detalle.terceros[1]  COLUMN-LABEL 'Prod. Terceros S/.'
        detalle.terceros[2]  COLUMN-LABEL 'Prod. Terceros U$'
        detalle.credito[1]  COLUMN-LABEL 'N/C S/.'
        detalle.credito[2]  COLUMN-LABEL 'N/C U$'
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
OUTPUT CLOSE.

